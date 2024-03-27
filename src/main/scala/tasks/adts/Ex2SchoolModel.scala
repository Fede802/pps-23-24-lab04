package tasks.adts
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

    def createCourse(n: String): Course
    def createTeacher(n: String): Teacher
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

    def createCourse(n: String): Course = CourseImpl(n)
    def createTeacher(n: String): Teacher = TeacherImpl(n, Sequence.Nil())
    def createEmptySchool(): School = SchoolImpl(Sequence.Nil(), Sequence.Nil())
    import BasicSetADT.*
    extension (school: School)

      def addTeacher(name: String): School = school match
        case SchoolImpl(t, c) =>
          SchoolImpl(Sequence.Cons(createTeacher(name), t), c)

      def addCourse(name: String): School = school match
        case SchoolImpl(t, c) =>
          if Optional.isEmpty(courseByName(name)) then
            SchoolImpl(t, Sequence.Cons(createCourse(name), c))
          else school

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

      def setTeacherToCourse(teacher: Teacher, course: Course): School = ???

      def coursesOfATeacher(teacher: Teacher): Sequence[Course] = ???
