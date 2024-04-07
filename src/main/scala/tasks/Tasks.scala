package tasks

import u03.Sequences.Sequence
import u04.monads.Monads.Monad
import u03.Optionals.Optional


object Ex1:

  trait ComplexADT:
    type Complex
    def complex(re: Double, im: Double): Complex
    extension (complex: Complex)
      def re(): Double
      def im(): Double
      def sum(other: Complex): Complex
      def subtract(other: Complex): Complex
      def asString(): String

  object BasicComplexADT extends ComplexADT:

    private case class ComplexNumber(re: Double, im: Double)
    // Change assignment below: should probably define a case class and use it?
    opaque type Complex = ComplexNumber
    override def complex(re: Double, im: Double): Complex =
      ComplexNumber(re, im)
    extension (complex: Complex)
      override def re(): Double = complex match {
        case ComplexNumber(re, _) => re
      }
      override def im(): Double = complex match {
        case ComplexNumber(_, im) => im
      }
      override def sum(other: Complex): Complex =
        ComplexNumber(complex.re() + other.re(), complex.im() + other.im())
      override def subtract(other: Complex): Complex =
        ComplexNumber(complex.re() - other.re(), complex.im() - other.im())
      override def asString(): String = complex match
        case ComplexNumber(re, im) =>
          if (im == 0) s"${re}"
          else if (re == 0) s"${im}i"
          else s"${re} ${sign(im)} ${Math.abs(im)}i"
      private def sign(d: Double): String = if (d < 0) "-" else "+"


object Ex2:
  trait SchoolModule:
    type Course
    type Teacher
    type School
    def createEmptySchool(): School
    extension (school: School)
      def addTeacher(name: String): School
      def addCourse(name: String): School
      def teacherByName(name: String): Optional[Teacher]
      def courseByName(name: String): Optional[Course]
      def nameOfTeacher(teacher: Teacher): String
      def nameOfCourse(course: Course): String
      def setTeacherToCourse(teacher: Teacher, course: Course): School
      def coursesOfATeacher(teacher: Teacher): Sequence[Course]

  object SchoolModuleImpl extends SchoolModule:
    private case class CourseImpl(n: String)
    private case class TeacherImpl(n: String, c: Sequence[Course])
    private case class SchoolImpl(
        t: Sequence[Teacher],
        c: Sequence[Course]
    )
    opaque type School = SchoolImpl
    opaque type Teacher = TeacherImpl
    opaque type Course = CourseImpl
    private def teacherHasCourse(teacher: Teacher, course: Course): Boolean =
      Sequence.filter(teacher.c)(_.n == course.n) match
        case Sequence.Nil() => false
        case _              => true
    override def createEmptySchool(): School =
      SchoolImpl(Sequence.Nil(), Sequence.Nil())
    extension (school: School)
      override def addTeacher(name: String): School =
        require(Optional.isEmpty(teacherByName(name)))
        SchoolImpl(
          Sequence.Cons(TeacherImpl(name, Sequence.Nil()), school.t),
          school.c
        )

      override def addCourse(name: String): School =
        require(Optional.isEmpty(courseByName(name)))
        SchoolImpl(school.t, Sequence.Cons(CourseImpl(name), school.c))

      override def teacherByName(name: String): Optional[Teacher] =
        Sequence.filter(school.t)(_.n == name) match
          case Sequence.Cons(h, _) => Optional.Just(h)
          case _                   => Optional.Empty()

      override def courseByName(name: String): Optional[Course] =
        Sequence.filter(school.c)(_.n == name) match
          case Sequence.Cons(h, _) => Optional.Just(h)
          case _                   => Optional.Empty()

      override def nameOfTeacher(teacher: Teacher): String = teacher.n

      override def nameOfCourse(course: Course): String = course.n

      override def setTeacherToCourse(
          teacher: Teacher,
          course: Course
      ): School =
        require(
          !Optional.isEmpty(teacherByName(teacher.n))
            && !Optional.isEmpty(courseByName(course.n))
            && !teacherHasCourse(teacher, course)
        )
        SchoolImpl(
          Sequence.map(school.t)(t =>
            if t == teacher then TeacherImpl(t.n, Sequence.Cons(course, t.c))
            else t
          ),
          school.c
        )

      override def coursesOfATeacher(teacher: Teacher): Sequence[Course] =
        require(!Optional.isEmpty(teacherByName(teacher.n)))
        teacher.c;

object Ex3:

  trait StackADT:
    type Stack[A]
    def empty[A]: Stack[A] // factory
    extension [A](stack: Stack[A])
      def push(a: A): Stack[A]
      def pop(): Optional[(A, Stack[A])]
      def asSequence(): Sequence[A]

  object StackImpl extends StackADT:
    opaque type Stack[A] = Sequence[A]
    override def empty[A]: Stack[A] = Sequence.Nil()
    extension [A](stack: Stack[A])
      override def push(a: A): Stack[A] = Sequence.Cons(a, stack)

      override def pop(): Optional[(A, Stack[A])] = stack match {
        case Sequence.Cons(h, t) => Optional.Just((h, t))
        case _                   => Optional.Empty()
      }
      override def asSequence(): Sequence[A] = stack

object Ex4:

  trait Summable[A]:
    def sum(a1: A, a2: A): A
    def zero: A

  def sumAll[A: Summable](seq: Sequence[A]): A =
    val summable = summon[Summable[A]]
    seq match
      case Sequence.Cons(h1, t1) => summable.sum(h1, sumAll(t1))
      case Sequence.Nil() => summable.zero


  given Summable[Int] with
    def sum(a1: Int, a2: Int): Int = a1 + a2
    def zero: Int = 0

  given Summable[Double] with
    def sum(a1: Double, a2: Double): Double = a1 + a2
    def zero: Double = 0

  given Summable[String] with
    def sum(a1: String, a2: String): String = a1 + a2
    def zero: String = ""

object Ex5:
  def log[A](a: A): Unit = println("The next element is: " + a)

  trait Traversable[T[_]]:
    def logAll[A](t: T[A]): Unit


  given Traversable[Optional] with {
    def logAll[A](opt: Optional[A]): Unit = opt match
      case Optional.Just(a) => log(a)
      case _ => ()
  }

  given Traversable[Sequence] with {
    def logAll[A](seq: Sequence[A]): Unit = seq match
      case Sequence.Cons(h, t) => log(h); logAll(t)
      case _ => ()
  }

  def logAll[B, A[_] : Traversable](t: A[B]): Unit = summon[Traversable[A]].logAll(t)

object Ex6:
  private enum TryImpl[A]:
    case Success(value: A)
    case Failure(exception: Throwable)

  opaque type Try[A] = TryImpl[A]

  def success[A](value: A): Try[A] = TryImpl.Success(value)
  def failure[A](exception: Throwable): Try[A] = TryImpl.Failure(exception)
  def exec[A](expression: => A): Try[A] = try
    success(expression)
  catch
    case e: Throwable => failure(e)

  extension [A](m: Try[A])
    def getOrElse[B >: A](other: B): B = m match
      case TryImpl.Success(value) => value
      case TryImpl.Failure(_) => other

  given Monad[Try] with
    override def unit[A](value: A): Try[A] = exec(value)
    extension [A](m: Try[A])
      override def flatMap[B](f: A => Try[B]): Try[B] =
        m match
          case TryImpl.Success(v) => f(v)
          case TryImpl.Failure(e) => failure(e)