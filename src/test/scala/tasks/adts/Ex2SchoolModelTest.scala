package tasks.adts

import org.junit.*
import org.junit.Assert.*
import org.junit.function.ThrowingRunnable
import org.junit.jupiter.api.BeforeEach
import org.junit.jupiter.api.function.Executable
import tasks.adts.Ex2SchoolModel.{SchoolModule, SchoolModuleImpl}
import u03.Optionals.*
import u03.Sequences.Sequence

class Ex2SchoolModelTest:

  val schoolADT: SchoolModule = SchoolModuleImpl
  import schoolADT.*
  val courseName = "PPS"
  val teacherName = "Viroli"

  @Test def testSchoolInitiallyEmpty(): Unit =
    val school: School = createEmptySchool()
    assertTrue(Optional.isEmpty(school.courseByName(courseName)))
    assertTrue(Optional.isEmpty(school.teacherByName(teacherName)))

  @Test def testAddingCourse(): Unit =
    val school: School = createEmptySchool().addCourse(courseName)
    val course: Optional[Course] = school.courseByName(courseName)
    course match
      case Optional.Just(c) => assertEquals(courseName, school.nameOfCourse(c))
      case _                => fail()

  @Test def testAddingExistingCourse(): Unit =
    assertThrows(
      classOf[IllegalArgumentException],
      () => createEmptySchool().addCourse(courseName).addCourse(courseName)
    )

  @Test def testAddingTeacher(): Unit =
    val school: School = createEmptySchool().addTeacher(teacherName)
    val teacher: Optional[Teacher] = school.teacherByName(teacherName)
    teacher match
      case Optional.Just(t) =>
        assertEquals(teacherName, school.nameOfTeacher(t))
      case _ => fail()

  @Test def testAddingExistingTeacher(): Unit =
    assertThrows(
      classOf[IllegalArgumentException],
      () => createEmptySchool().addTeacher(teacherName).addTeacher(teacherName)
    )

  @Test def testCoursesOfATeacher(): Unit = {
    val school: School = createEmptySchool().addTeacher(teacherName)
    school.teacherByName(teacherName) match
      case Optional.Just(t) =>
        assertEquals(Sequence.Nil(), school.coursesOfATeacher(t))
      case _ => fail()
  }

  @Test def testCoursesOfATeacherWithoutTeacher(): Unit = {
    val school1: School = createEmptySchool().addTeacher(teacherName)
    val school2: School = createEmptySchool()
    school1.teacherByName(teacherName) match
      case Optional.Just(t) =>
        assertThrows(
          classOf[IllegalArgumentException],
          () => school2.coursesOfATeacher(t)
        )
      case _ => fail()
  }

  @Test def testSetTeacherToCourse(): Unit =
    val school: School =
      createEmptySchool().addTeacher(teacherName).addCourse(courseName)
    (school.teacherByName(teacherName), school.courseByName(courseName)) match
      case (Optional.Just(teacher), Optional.Just(course)) =>
        val schoolUpdated: School = school.setTeacherToCourse(teacher, course)
        schoolUpdated.teacherByName(teacherName) match
          case Optional.Just(t) =>
            assertEquals(
              Sequence.Cons(course, Sequence.Nil()),
              Sequence.filter(schoolUpdated.coursesOfATeacher(t))(_ == course)
            )
          case _ => fail()
      case _ => fail()

  @Test def testSetTeacherToCourseWithoutCourse(): Unit =
    val school1: School =
      createEmptySchool().addTeacher(teacherName).addCourse(courseName)
    val school2: School = createEmptySchool().addTeacher(teacherName)
    (school1.teacherByName(teacherName), school1.courseByName(courseName)) match
      case (Optional.Just(teacher), Optional.Just(course)) =>
        assertThrows(
          classOf[IllegalArgumentException],
          () => school2.setTeacherToCourse(teacher, course)
        )
      case _ => fail()

  @Test def testSetTeacherToCourseWithoutTeacher(): Unit =
    val school1: School =
      createEmptySchool().addTeacher(teacherName).addCourse(courseName)
    val school2: School = createEmptySchool().addCourse(courseName)
    (school1.teacherByName(teacherName), school1.courseByName(courseName)) match
      case (Optional.Just(teacher), Optional.Just(course)) =>
        assertThrows(
          classOf[IllegalArgumentException],
          () => school2.setTeacherToCourse(teacher, course)
        )
      case _ => fail()

  @Test def testSetTeacherToSameCourse(): Unit =
    val school: School =
      createEmptySchool().addTeacher(teacherName).addCourse(courseName)
    (school.teacherByName(teacherName), school.courseByName(courseName)) match
      case (Optional.Just(teacher), Optional.Just(course)) =>
        val schoolUpdated: School = school.setTeacherToCourse(teacher, course)
        schoolUpdated.teacherByName(teacherName) match
          case Optional.Just(t) =>
            assertThrows(
              classOf[IllegalArgumentException],
              () =>
                schoolUpdated
                  .setTeacherToCourse(t, course)
            )
          case _ => fail()
      case _ => fail()
