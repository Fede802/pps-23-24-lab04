package tasks.adts
import tasks.adts.Ex2SchoolModel.SchoolModuleImpl.{Course, School, Teacher}
import u03.Sequences.*
import u03.Optionals.*
import u04.moduletypes.Sets.*
import u02.AlgebraicDataTypes.Person

/*  Exercise 2:
 *  Implement the below trait, and write a meaningful test.
 *  Suggestion:
 *  - reuse Sequences and Optionals as imported above
 *  - Course is a simple case classes with just the name
 *  - Teacher is a case class with name and sequence of courses
 *  - School is a case class with (sequences of) teachers and courses
 *  - add/set methods below create the new school
 */

object Ex2SchoolModel:

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

    def createEmptySchool(): School = SchoolImpl(Sequence.Nil(), Sequence.Nil())
    import BasicSetADT.*
    extension (school: School)
      def addTeacher(name: String): School =
        require(Optional.isEmpty(teacherByName(name)))
        school match
          case SchoolImpl(t, c) =>
            SchoolImpl(Sequence.Cons(TeacherImpl(name, Sequence.Nil()), t), c)

      def addCourse(name: String): School =
        require(Optional.isEmpty(courseByName(name)))
        school match
          case SchoolImpl(t, c) =>
            SchoolImpl(t, Sequence.Cons(CourseImpl(name), c))

      def teacherByName(name: String): Optional[Teacher] =
        Sequence.filter(school.t)(_.n == name) match
          case Sequence.Cons(h, Sequence.Nil()) => Optional.Just(h)
          case _                                => Optional.Empty()

      def courseByName(name: String): Optional[Course] =
        Sequence.filter(school.c)(_.n == name) match
          case Sequence.Cons(h, Sequence.Nil()) => Optional.Just(h)
          case _                                => Optional.Empty()

      def nameOfTeacher(teacher: Teacher): String = teacher.n

      def nameOfCourse(course: Course): String = course.n

      def setTeacherToCourse(teacher: Teacher, course: Course): School =
        require(
          !Optional.isEmpty(teacherByName(teacher.n)) && !Optional.isEmpty(
            courseByName(course.n)
          ) && Optional.isEmpty(
            Sequence.filter(coursesOfATeacher(teacher))(_.n == course.n) match
              case Sequence.Nil() => Optional.Empty()
              case _              => Optional.Just(course.n)
          )
        )
        SchoolImpl(
          Sequence.map(school.t)(t =>
            if t == teacher then TeacherImpl(t.n, Sequence.Cons(course, t.c))
            else t
          ),
          school.c
        )

      def coursesOfATeacher(teacher: Teacher): Sequence[Course] =
        require(!Optional.isEmpty(teacherByName(teacher.n)))
        teacher.c;
