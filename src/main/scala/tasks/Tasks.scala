package tasks

/*  Exercise 1:
 *  Complete the implementation of ComplexADT trait below, so that it passes
 *  the test in ComplexTest.
 */

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
    def complex(re: Double, im: Double): Complex = ComplexNumber(re, im)
    extension (complex: Complex)
      def re(): Double = complex match { case ComplexNumber(re, _) => re }
      def im(): Double = complex match { case ComplexNumber(_, im) => im }
      def sum(other: Complex): Complex =
        ComplexNumber(complex.re() + other.re(), complex.im() + other.im())
      def subtract(other: Complex): Complex =
        ComplexNumber(complex.re() - other.re(), complex.im() - other.im())
      def asString(): String = complex match
        case ComplexNumber(re, im) =>
          if (im == 0) s"${re}"
          else if (re == 0) s"${im}i"
          else s"${re} ${sign(im)} ${Math.abs(im)}i"
      private def sign(d: Double): String = if (d < 0) "-" else "+"
