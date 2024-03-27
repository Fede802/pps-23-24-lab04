package tasks.adts

import org.junit.*
import org.junit.Assert.*
import org.junit.jupiter.api.BeforeEach
import tasks.adts.Ex2SchoolModel.{SchoolModule, SchoolModuleImpl}
import u03.Optionals.*

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
    val course: Course = createCourse(courseName)
    val school: School = createEmptySchool().addCourse(courseName)
    assertFalse(Optional.isEmpty(school.courseByName(courseName)))
    assertEquals(Optional.Just(course), school.courseByName(courseName))
    assertEquals(courseName, school.nameOfCourse(course))

  @Test def testAddingTeacher(): Unit =
    val teacher: Teacher = createTeacher(teacherName)
    val school: School = createEmptySchool().addTeacher(teacherName)
    assertFalse(Optional.isEmpty(school.teacherByName(teacherName)))
    assertEquals(Optional.Just(teacher), school.teacherByName(teacherName))
    assertEquals(teacherName, school.nameOfTeacher(teacher))

  @Test def testAssignCourseToTeacherWithInitializedSchool(): Unit =
    val teacher: Teacher = createTeacher(teacherName)
    val course: Course = createCourse(courseName)
    val school: School = createEmptySchool().addTeacher(teacherName).addCourse(courseName)
