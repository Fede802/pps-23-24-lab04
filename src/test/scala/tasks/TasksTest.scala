package tasks

import org.junit.*
import org.junit.Assert.*
import u03.Optionals.Optional
import u03.Sequences.Sequence

object TasksTest {

  import tasks.Ex1.{BasicComplexADT, ComplexADT}
  class Ex1Test:

    val complexADT: ComplexADT = BasicComplexADT
    import complexADT.*

    @Test def testReal(): Unit =
      assertEquals(10, complex(10, 20).re(), 0)

    @Test def testImaginary(): Unit =
      assertEquals(20, complex(10, 20).im(), 0)

    @Test def testSum(): Unit =
      assertEquals(complex(11, 22), complex(10, 20) sum complex(1, 2))

    @Test def testSubtract(): Unit =
      assertEquals(complex(9, 18), complex(10, 20) subtract complex(1, 2))

    @Test def testAsString(): Unit =
      assertEquals("10.0 + 5.0i", complex(10.0, 5.0).asString())

    @Test def optionalTestAdvancedAsString(): Unit =
      assertEquals("0.0", complex(0, 0).asString())
      assertEquals("10.0", complex(10.0, 0).asString())
      assertEquals("10.0 + 5.0i", complex(10.0, 5.0).asString())
      assertEquals("10.0 - 5.0i", complex(10.0, -5.0).asString())
      assertEquals("5.0i", complex(0, 5.0).asString())
      assertEquals("-5.0i", complex(0, -5.0).asString())

  import tasks.Ex2.{SchoolModule, SchoolModuleImpl}
  class Ex2Test:

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
      school.courseByName(courseName) match
        case Optional.Just(c) =>
          assertEquals(courseName, school.nameOfCourse(c))
        case _ => fail()

    @Test def testAddingExistingCourse(): Unit =
      assertThrows(
        classOf[IllegalArgumentException],
        () => createEmptySchool().addCourse(courseName).addCourse(courseName)
      )

    @Test def testAddingTeacher(): Unit =
      val school: School = createEmptySchool().addTeacher(teacherName)
      school.teacherByName(teacherName) match
        case Optional.Just(t) =>
          assertEquals(teacherName, school.nameOfTeacher(t))
        case _ => fail()

    @Test def testAddingExistingTeacher(): Unit =
      assertThrows(
        classOf[IllegalArgumentException],
        () =>
          createEmptySchool().addTeacher(teacherName).addTeacher(teacherName)
      )

    @Test def testCoursesOfATeacher(): Unit =
      val school: School = createEmptySchool().addTeacher(teacherName)
      school.teacherByName(teacherName) match
        case Optional.Just(t) =>
          assertEquals(Sequence.Nil(), school.coursesOfATeacher(t))
        case _ => fail()

    @Test def testCoursesOfATeacherWithoutTeacher(): Unit =
      val school: School = createEmptySchool()
      createEmptySchool()
        .addTeacher(teacherName)
        .teacherByName(teacherName) match
        case Optional.Just(t) =>
          assertThrows(
            classOf[IllegalArgumentException],
            () => school.coursesOfATeacher(t)
          )
        case _ => fail()

    @Test def testSetTeacherToCourse(): Unit =
      var school: School =
        createEmptySchool().addTeacher(teacherName).addCourse(courseName)
      (school.teacherByName(teacherName), school.courseByName(courseName)) match
        case (Optional.Just(teacher), Optional.Just(course)) =>
          school = school.setTeacherToCourse(teacher, course)
          school.teacherByName(teacherName) match
            case Optional.Just(t) =>
              assertEquals(
                Sequence.Cons(course, Sequence.Nil()),
                Sequence.filter(school.coursesOfATeacher(t))(_ == course)
              )
            case _ => fail()
        case _ => fail()

    @Test def testSetTeacherToCourseWithoutCourse(): Unit =
      (
        createEmptySchool().addTeacher(teacherName).teacherByName(teacherName),
        createEmptySchool().addCourse(courseName).courseByName(courseName)
      ) match
        case (Optional.Just(teacher), Optional.Just(course)) =>
          assertThrows(
            classOf[IllegalArgumentException],
            () =>
              createEmptySchool()
                .addTeacher(teacherName)
                .setTeacherToCourse(teacher, course)
          )
        case _ => fail()

    @Test def testSetTeacherToCourseWithoutTeacher(): Unit =
      (
        createEmptySchool().addTeacher(teacherName).teacherByName(teacherName),
        createEmptySchool().addCourse(courseName).courseByName(courseName)
      ) match
        case (Optional.Just(teacher), Optional.Just(course)) =>
          assertThrows(
            classOf[IllegalArgumentException],
            () =>
              createEmptySchool()
                .addCourse(courseName)
                .setTeacherToCourse(teacher, course)
          )
        case _ => fail()

    @Test def testSetTeacherToSameCourse(): Unit =
      var school: School =
        createEmptySchool().addTeacher(teacherName).addCourse(courseName)
      (school.teacherByName(teacherName), school.courseByName(courseName)) match
        case (Optional.Just(teacher), Optional.Just(course)) =>
          school = school.setTeacherToCourse(teacher, course)
          school.teacherByName(teacherName) match
            case Optional.Just(t) =>
              assertThrows(
                classOf[IllegalArgumentException],
                () =>
                  school
                    .setTeacherToCourse(t, course)
              )
            case _ => fail()
        case _ => fail()

  import tasks.Ex3.{StackADT, StackImpl}
  class Ex3Test:

    val stack: StackADT = StackImpl
    import stack.*

    @Test def testEmpty(): Unit =
      assertEquals(Sequence.Nil(), empty[Int].asSequence())

    @Test def testPush(): Unit =
      assertEquals(
        Sequence.Cons(10, Sequence.Nil()),
        empty[Int].push(10).asSequence()
      )

    @Test def testPopOnEmpty(): Unit =
      assertEquals(Optional.Empty(), empty[Int].pop())

    @Test def testPopOnNotEmpty(): Unit =
      assertEquals(
        Optional.Just((10, Sequence.Nil())),
        empty[Int].push(10).pop()
      )

  import tasks.Ex4.*
  class Ex4Test:

    @Test def testSumIntSequence(): Unit =
      val si =
        Sequence.Cons(10, Sequence.Cons(20, Sequence.Cons(30, Sequence.Nil())))
      assertEquals(60, sumAll(si))

    @Test def testSumDoubleSequence(): Unit =
      val si = Sequence.Cons(
        10.0,
        Sequence.Cons(20.0, Sequence.Cons(30.0, Sequence.Nil()))
      )
      assertEquals(60, sumAll(si), 0)

    @Test def testSumStringSequence(): Unit =
      val si = Sequence.Cons(
        "10",
        Sequence.Cons("20", Sequence.Cons("30", Sequence.Nil()))
      )
      assertEquals("102030", sumAll(si))

  import tasks.Ex5.logAll
  class Ex5Test:

    @Test def testLogAllSequence(): Unit =
      val seq =
        Sequence.Cons(1, Sequence.Cons(2, Sequence.Cons(3, Sequence.Nil())))
      logAll(seq)

    @Test def testLogAllOptional(): Unit =
      val opt = Optional.Just(42)
      logAll(opt)

  import tasks.Ex6.*
  class Ex6Test:

    @Test def testTryMonadExposeMapFeature(): Unit =
      assert(success(20).map(_ + 10).getOrElse(-1) == 30)

    @Test def testComputationWithoutErrors(): Unit =
      val result = for
        a <- success(10)
        b <- success(30)
      yield a + b
      assertEquals(40, result.getOrElse(-1))

    @Test def testComputationWithErrors(): Unit =
      val result = for
        a <- success(10)
        b <- failure(new RuntimeException("error"))
        c <- success(30)
      yield a + c
      assertEquals(-1, result.getOrElse(-1))

    @Test def testErrorsHandledCorrectly(): Unit =
      val result3 = for
        a <- exec(10)
        b <- exec(new RuntimeException("error"))
        c <- exec(30)
      yield a + c

}
