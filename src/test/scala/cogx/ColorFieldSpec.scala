/*
 * (c) Copyright 2016 Hewlett Packard Enterprise Development LP
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package cogx

import org.scalatest.junit.JUnitRunner
import org.scalatest.FunSuite
import org.scalatest.MustMatchers
import org.junit.runner.RunWith
import cogx.api.ImplicitConversions
import cogx.helper.{ColorFieldBuilderInterface, ScalarFieldBuilderInterface}
import cogx.reference.{RefTestInterface, RefColorField}

/** Test code for ColorFields.
  */

@RunWith(classOf[JUnitRunner])
class ColorFieldSpec extends FunSuite
                     with MustMatchers
                     with ImplicitConversions
                     with ScalarFieldBuilderInterface
                     with ColorFieldBuilderInterface {

  val Optimize = true

  test("color field / color planes") {
    val Rows = 5
    val Columns = 7

    val initField = RefColorField.random(Rows, Columns)

    val graph = new ComputeGraph(Optimize) with RefTestInterface {
      // 2 copies of input to keep outputs from being optimized away
      // (no probe support yet).
      val c = TestColorField(initField)
      val d = TestColorField(initField)
      val red = c.red
      val green = c.green
      val blue = c.blue
      val luminance = c.luminance
      val dRebuilt = colorField(d.red, d.green, d.blue)

      probe(red, green, blue, luminance, dRebuilt)
    }

    import graph._
    withRelease {
      step
      require(initField.red == readScalar(red))
      require(initField.green == readScalar(green))
      require(initField.blue == readScalar(blue))
      require(initField.luminance ~== readScalar(luminance))
      require(initField == readColor(dRebuilt))
    }
  }

//  /** Test combining a dynamic scalar field and a constant. */
//  object TestFieldConstant extends Tester {
//    val Size = 5
//
//    val R = 3.21f
//    val initField = ColorField.random(Size, Size)
//
//    // Tests that involve equality should involve a threshold that matches
//    // at least one element.  Here are some elements:
//    val elem1 = initField.red.read(0,0)
//    val elem2 = initField.green.read(Size-1,0)
//    val elem3 = initField.blue.read(Size-1,Size-1)
//    val elem4 = initField.red.read(0,Size-1)
//
//    val a = new DynamicColorField(initField)
//    val sum = new DynamicColorField(Size, Size) {this <== a + R}
//    val diff = new DynamicColorField(Size, Size) {this <== a - R}
//    val product = new DynamicColorField(Size, Size) {this <== a * R}
//    val quotient = new DynamicColorField(Size, Size) {this <== a / R}
//    //val power = new DynamicColorField(Size, Size) {this <== a pow 3}
//    //val pown = new DynamicColorField(Size, Size) {this <== a pown 3}
//
//    val constSum = new DynamicColorField(Size, Size) {this <== R + a}
//    val constDiff = new DynamicColorField(Size, Size) {this <== R - a}
//    val constProduct = new DynamicColorField(Size, Size) {this <== R * a}
//    val constQuotient = new DynamicColorField(Size, Size) {this <== R / a}
//
//    val greater = new DynamicColorField(Size, Size) {this <== (a > 0.5f)}
//    val greaterEq = new DynamicColorField(Size, Size) {this <== (a >= elem1)}
//    val less = new DynamicColorField(Size, Size) {this <== (a < 0.5f)}
//    val lessEq = new DynamicColorField(Size, Size) {this <== (a <= elem2)}
//    val eq = new DynamicColorField(Size, Size) {this <== (a === elem3)}
//    val nEq = new DynamicColorField(Size, Size) {this <== (a !=== elem4)}
//
//    def check {
//      require(read(sum) == (read(a) + R))
//      require(read(diff) == (read(a) - R))
//      require(read(product) == (read(a) * R))
//      require(read(quotient) ~== (read(a) / R))
//      //require(read(power) ~== (read(a) :* read(a) :* read(a)))
//      //require(read(pown) ~== (read(a) :* read(a) :* read(a)))
//
//      require(read(constSum) == (read(a) + R))
//      require(read(constDiff) == (-read(a) + R))
//      require(read(constProduct) == (read(a) * R))
//      require(read(constQuotient) ~== (read(a).reciprocal * R))
//
//      require(read(greater) == read(a).map(mapPixel(_, _ > 0.5f)))
//      require(read(greaterEq) == read(a).map(mapPixel(_, _ >= elem1)))
//      require(read(less) == read(a).map(mapPixel(_, _ < 0.5f)))
//      require(read(lessEq) == read(a).map(mapPixel(_, _ <= elem2)))
//      require(read(eq) == read(a).map(mapPixel(_, _ == elem3)))
//      require(read(nEq) == read(a).map(mapPixel(_, _ != elem4)))
//    }
//  }
//
//  def mapPixel(pixel: Pixel, f: (Float) => Boolean): Pixel = {
//    val red = if (f(pixel.red)) 1f else 0f
//    val green = if (f(pixel.green)) 1f else 0f
//    val blue = if (f(pixel.blue)) 1f else 0f
//    new Pixel(red, green, blue)
//  }
//
//  def compare(a: Pixel, b: Pixel, f: (Float, Float) => Boolean): Pixel = {
//    val red = if (f(a.red, b.red)) 1f else 0f
//    val green = if (f(a.green, b.green)) 1f else 0f
//    val blue = if (f(a.blue, b.blue)) 1f else 0f
//    val alpha = 1f
//    new Pixel(red, green, blue, alpha)
//  }
//
//  /** Test combining a DynamicColorField with a static ColorField. */
//  object TestFieldConstantField extends Tester {
//    val Size = 5
//    val C = ColorField.random(Size, Size)
//    val D = ColorField.random(Size, Size)
//    val initField = ColorField.random(Size, Size)
//    val a = new DynamicColorField(initField)
//    val sum = new DynamicColorField(Size, Size) {this <== a + C}
//    val sum2 = new DynamicColorField(Size, Size) {this <== (a + C) + C}
//    val sum3 = new DynamicColorField(Size, Size) {this <== (a + C) + D}
//    val diff = new DynamicColorField(Size, Size) {this <== a - C}
//    val eq1 = new DynamicColorField(Size, Size) {this <== a === initField}
//    val eq2 = new DynamicColorField(Size, Size) {this <== a === (initField * 0.5f)}
//    val nEq1 = new DynamicColorField(Size, Size) {this <== (a !=== initField)}
//    val nEq2 = new DynamicColorField(Size, Size) {this <== (a !=== (initField * 0.5f))}
//
//    def check {
//      require(read(sum) == (read(a) + C))
//      require(read(sum2) == (read(a) + C + C))
//      require(read(sum3) == (read(a) + C + D))
//      require(read(diff) == (read(a) - C))
//      require(read(eq1) == (read(a).combine(initField, Eq _)))
//      require(read(eq2) == (read(a).combine(initField * 0.5f, Eq _)))
//      require(read(nEq1) == (read(a).combine(initField, notEq _)))
//      require(read(nEq2) == (read(a).combine(initField * 0.5f, notEq _)))
//    }
//
//    def Eq(a: Pixel, b: Pixel): Pixel = compare(a, b, _ == _)
//    def notEq(a: Pixel, b: Pixel): Pixel = compare(a, b, _ != _)
//  }
//
//  /** Test combining two dynamic color fields. */
//  object TestFieldField extends Tester {
//    val Size = 5
//    val a = new DynamicColorField(ColorField.random(Size, Size))
//    val b = new DynamicColorField(ColorField.random(Size, Size))
//    val d = new DynamicColorField(ColorField.random(Size, Size))
//
//    val sum = new DynamicColorField(Size, Size) {this <== a + b}
//    val diff = new DynamicColorField(Size, Size) {this <== a - b}
//    val product = new DynamicColorField(Size, Size) {this <== a * b}
//    val quotient = new DynamicColorField(Size, Size) {this <== a / b}
//    val multiPoint = new DynamicColorField(Size, Size) {this <== a * b + d}
//
//    /** A 1 x 1 color field, can be combined with any other. */
//    val Constant = new Pixel(1f, 0.1f, 0.5f)
//    val c = new DynamicColorField(new ColorField(1, 1,
//      (r, c) => (Constant.red, Constant.green, Constant.blue)))
//
//    val sum0D = new DynamicColorField(Size, Size) {this <== a + c}
//    val diff0D = new DynamicColorField(Size, Size) {this <== a - c}
//    val product0D = new DynamicColorField(Size, Size) {this <== a * c}
//    val quotient0D = new DynamicColorField(Size, Size) {this <== a / c}
//
//    val greater = new DynamicColorField(Size, Size) {this <== (a > b)}
//    val greaterEq = new DynamicColorField(Size, Size) {this <== (a >= b)}
//    val less = new DynamicColorField(Size, Size) {this <== (a < b)}
//    val lessEq = new DynamicColorField(Size, Size) {this <== (a <= b)}
//    val eq1 = new DynamicColorField(Size, Size) {this <== a === a}
//    val eq2 = new DynamicColorField(Size, Size) {this <== a === b}
//    val nEq1 = new DynamicColorField(Size, Size) {this <== (a !=== a)}
//    val nEq2 = new DynamicColorField(Size, Size) {this <== (a !=== b)}
//
//    def check {
//      require(read(sum) == (read(a) + read(b)))
//      require(read(diff) == (read(a) - read(b)))
//      require(read(product) == (read(a) * read(b)))
//      require(read(quotient) ~== (read(a) / read(b)))
//      require(read(multiPoint) ~== ((read(a) * read(b)) + read(d)))
//
//      require(read(sum0D) == (read(a) + Constant))
//      require(read(diff0D) == (read(a) - Constant))
//      require(read(product0D) == (read(a) * Constant))
//      require(read(quotient0D) ~== (read(a) / Constant))
//
//      val aa = read(a)
//      val bb = read(b)
//      require(read(greater) == aa.combine(bb, greaterThan _))
//      require(read(greaterEq) == aa.combine(bb, greaterThanEq _))
//      require(read(less) == aa.combine(bb, lessThan _))
//      require(read(lessEq) == aa.combine(bb, lessThanEq _))
//      require(read(eq1) == aa.combine(aa, Eq _))
//      require(read(eq2) == aa.combine(bb, Eq _))
//      require(read(nEq1) == aa.combine(aa, notEq _))
//      require(read(nEq2) == aa.combine(bb, notEq _))
//    }
//
//    def greaterThan(a: Pixel, b: Pixel): Pixel = compare(a, b, _ > _)
//    def greaterThanEq(a: Pixel, b: Pixel): Pixel = compare(a, b, _ >= _)
//    def lessThan(a: Pixel, b: Pixel): Pixel = compare(a, b, _ < _)
//    def lessThanEq(a: Pixel, b: Pixel): Pixel = compare(a, b, _ <= _)
//    def Eq(a: Pixel, b: Pixel): Pixel = compare(a, b, _ == _)
//    def notEq(a: Pixel, b: Pixel): Pixel = compare(a, b, _ != _)
//
//  }
//
//  /** Test applying unary operations on dynamic color fields. */
//  object TestUnary extends Tester {
//
//    val Size = 5
//    val a = new DynamicColorField(ColorField.random(Size, Size) - 0.5f)
//    val b = new DynamicColorField(ColorField.random(Size, Size) + 0.5f)
//    val abs = new DynamicColorField(Size, Size) {this <== a.abs}
//    val acos = new DynamicColorField(Size, Size) {this <== a.acos}
//
//    val asin = new DynamicColorField(Size, Size) {this <== a.asin}
//    val cos = new DynamicColorField(Size, Size) {this <== a.cos}
//    val cosh = new DynamicColorField(Size, Size) {this <== a.cosh}
//    val exp = new DynamicColorField(Size, Size) {this <== a.exp}
//    val log = new DynamicColorField(Size, Size) {this <== b.log}
//    //val normalizeL1 = new DynamicColorField(Size, Size) {this <== b.normalizeL1}
//    //val normalizeL2 = new DynamicColorField(Size, Size) {this <== b.normalizeL2}
//    val rectify = new DynamicColorField(Size, Size) {this <== a.rectify}
//    //val signum = new DynamicColorField(Size, Size) {this <== a.signum}
//    val sin = new DynamicColorField(Size, Size) {this <== a.sin}
//    val sinh = new DynamicColorField(Size, Size) {this <== a.sinh}
//    val sq = new DynamicColorField(Size, Size) {this <== a.sq}
//    val sqrt = new DynamicColorField(Size, Size) {this <== b.sqrt}
//    val tan = new DynamicColorField(Size, Size) {this <== a.tan}
//    val tanh = new DynamicColorField(Size, Size) {this <== a.tanh}
//    val negative = new DynamicColorField(Size, Size) {this <== -a}
//
//    val Subsize = (Size + 1) / 2
//    val downsample = new DynamicColorField(Subsize, Subsize) {
//      this <== a.downsample()
//    }
//
//    def check {
//      require(read(abs) == read(a).map(_.map(_.abs)))
//      require(read(acos) ~== read(a).map(_.map(e => math.acos(e).toFloat)))
//      require(read(asin) ~== read(a).map(_.map(e => math.asin(e).toFloat)))
//      require(read(cos) ~== read(a).map(_.map(e => math.cos(e).toFloat)))
//      require(read(cosh) ~== read(a).map(_.map(e => math.cosh(e).toFloat)))
//      require(read(exp) ~== read(a).map(_.map(e => math.exp(e).toFloat)))
//      require(read(log) ~== read(b).map(_.map(e => math.log(e).toFloat)))
//
//      //      require(read(normalizeL1) ~== read(b) / read(b).reduce(_ + _))
//      //      require(read(normalizeL2) ~==
//      //            read(b) / (read(b).reduce(_ + _) * read(b).reduce(_ + _)))
//      require(read(rectify) == read(a).map(_.map(_ max 0f)))
//      //      require(read(signum) == read(a).map(e =>
//      //        (if (e < 0) -1f else if (e > 0) 1f else 0f)))
//      require(read(sin) ~== read(a).map(_.map(e => math.sin(e).toFloat)))
//      require(read(sinh) ~== read(a).map(_.map(e => math.sinh(e).toFloat)))
//      require(read(sq) == read(a).map(_.map(e => e * e)))
//      require(read(sqrt) ~== read(b).map(_.map(e => math.sqrt(e).toFloat)))
//      require(read(tan) ~== read(a).map(_.map(e => math.tan(e).toFloat)))
//      require(read(tanh) ~== read(a).map(_.map(e => math.tanh(e).toFloat)))
//      require(read(negative) == read(a) * -1f)
//      require(read(downsample) == read(a).downsample())
//    }
//  }
//
//  /** Test combinations of two dynamic color fields. */
//  object TestBinary extends Tester {
//    val Size = 5
//    val a = new DynamicColorField(ColorField.random(Size, Size) - 0.5f)
//    val b = new DynamicColorField(ColorField.random(Size, Size) + 0.5f)
//    val max = new DynamicColorField(Size, Size) {this <== a max b}
//    val min = new DynamicColorField(Size, Size) {this <== a min b}
//    //val dot = new DynamicColorField(Size, Size) {this <== a dot b}
//    val atan2 = new DynamicColorField(Size, Size) {this <== a.atan2(b)}
//
//    def check {
//      require(read(max) == read(a).combine(read(b), (a,b) => (a max b)))
//      require(read(min) == read(a).combine(read(b), (a,b) => (a min b)))
//      //require(read(dot) == (read(a) dot read(b)))
//      require(read(atan2) ~== read(a).combine(read(b), (a,b) => (a atan2 b)))
//    }
//  }

  /** Test the apply(Range*) operator. */
  test("color field / subfield") {
    val Size = 5
    val SubSize = 3

    val initField = RefColorField.random(Size, Size)

    val graph = new ComputeGraph(Optimize) with RefTestInterface {
      val a = TestColorField(initField)
      val b = a(0 until SubSize, 0 until SubSize)

      probe(b)
    }

    import graph._
    withRelease {
      step
      require(readColor(b) == initField.subfield(0 until SubSize, 0 until SubSize))
    }
  }

//  /** Test combining color fields and scalar fields. */
//  object TestColorScalar extends Tester {
//    val Size = 5
//    val a = new DynamicColorField(ColorField.random(Size, Size))
//    val b = new DynamicScalarField(ScalarField.random(Size, Size) + 1)
//    val sum = new DynamicColorField(Size, Size) {this <== a + b}
//    val diff = new DynamicColorField(Size, Size) {this <== a - b}
//    val product = new DynamicColorField(Size, Size) {this <== a * b}
//    val quotient = new DynamicColorField(Size, Size) {this <== a / b}
//
//    def check {
//      val bb = read(b)
//      val bColor = new ColorField(bb, bb, bb)
//      require(read(sum) == read(a).combine(bColor, (a, b) => a + b))
//      require(read(diff) == read(a).combine(bColor, (a, b) => a - b))
//      require(read(product) == read(a).combine(bColor, (a, b) => a * b))
//      require(read(quotient) ~== read(a).combine(bColor, (a, b) => a / b))
//    }
//  }

  test("color field / trim") {
    val Size = 5
    val trimShape = Shape(2, 3)

    val initField = RefColorField.random(Size, Size)
    val initField2 = RefColorField.random(Size, Size)

    val graph = new ComputeGraph(Optimize) with RefTestInterface {
      val a = TestColorField(initField)
      val a2 = TestColorField(initField2)
      val b = trim(a, trimShape)
      val c = ColorField(trimShape(0), trimShape(1))
      c <== a2

      probe(b, c)
    }

    import graph._
    withRelease {
      step
      require(readColor(b) == initField.subfield(0 until trimShape(0), 0 until trimShape(1)))
      require(readColor(c) == initField2.subfield(0 until trimShape(0), 0 until trimShape(1)))
    }
  }

//  /** Test creation of 3D color fields. */
//  object Test3D extends Tester {
//    val Depth = 3
//    val Rows = 5
//    val Columns = 7
//    val colorField = new ColorField(Depth, Rows, Columns, (depth, row, col) => (depth, row, col))
//    val a = new DynamicColorField(colorField)
//
//    def check {
//    }
//  }
//
//  object Test3DSlice extends Tester {
//    val Depth = 3
//    val Rows = 4
//    val Columns = 5
//    val stack = new ColorField(Depth, Rows, Columns, (depth, row, col) => (depth, row, col))
//    val stack0 = new ColorField(Rows, Columns, (row, col) => (0, row, col))
//    val stack1 = new ColorField(Rows, Columns, (row, col) => (1, row, col))
//    val stack2 = new ColorField(Rows, Columns, (row, col) => (2, row, col))
//
//    val image = new ColorField(Rows, Columns, (row, col) => (row, col, 1.0f))
//    val a = new DynamicColorField(stack)
//    val b0 = DynamicColorField(a(0))
//    val b1 = DynamicColorField(a(1))
//    val b2 = DynamicColorField(a(2))
//
//    def check {
//      require(read(b0) == stack0)
//      require(read(b1) == stack1)
//      require(read(b2) == stack2)
//    }
//  }

  /* Commented out because NVIDIA does not support 3D image writes (AMD does)

  object Test3DPush extends Tester {
    val Depth = 3
    val Rows = 5
    val Columns = 7
    val stack = new ColorField(Depth, Rows, Columns, (depth, row, col) => (depth, row, col))
    val stack0 = new ColorField(Rows, Columns, (row, col) => (0, row, col))
    val stack1 = new ColorField(Rows, Columns, (row, col) => (1, row, col))
    val stack2 = new ColorField(Rows, Columns, (row, col) => (2, row, col))
    val image = new ColorField(Rows, Columns, (row, col) => (row, col, 1.0f))
    val a = new DynamicColorField(stack) {
      this <== this.push(image)
    }
    val b = DynamicColorField(a push image)
    val b0 = DynamicColorField(b(0))
    val b1 = DynamicColorField(b(1))
    val b2 = DynamicColorField(b(2))

    def check {
      require(read(b0) == image)
      require(read(b1) == stack0)
      require(read(b2) == stack1)

      println("TestColorFields not done yet")
    }
  }
  */

  test("color field / upsample") {
    val dataRed = Matrix(
      Array(
        Array(1f, 7f, 5f),
        Array(2f, 0f, 4f),
        Array(4f, 9f, 3f)
      )
    ) / 10f
    val dataGreen = dataRed * 0.2f
    val dataBlue = dataRed * 0.3f
    val input = RefColorField(3, 3,
      (r, c) => (1f, dataRed(r, c), dataGreen(r, c), dataBlue(r, c)))

    val expectedDataRed = Matrix(
      Array(
        Array( 1f, 0f, 7f, 0f, 5f, 0f),
        Array( 0f, 0f, 0f, 0f, 0f, 0f),
        Array( 2f, 0f, 0f, 0f, 4f, 0f),
        Array( 0f, 0f, 0f, 0f, 0f, 0f),
        Array( 4f, 0f, 9f, 0f, 3f, 0f),
        Array( 0f, 0f, 0f, 0f, 0f, 0f)
      )
    ) / 10f
    val expectedDataGreen = expectedDataRed * 0.2f
    val expectedDataBlue = expectedDataRed * 0.3f

    val expected = RefColorField(6, 6,
      (r, c) =>  (1f, expectedDataRed(r, c), expectedDataGreen(r, c),
              expectedDataBlue(r, c)))

    val graph = new ComputeGraph(Optimize) with RefTestInterface {
      val inField = TestColorField(input)
      val outField = upsample(inField)

      probe(outField)
    }

    import graph._
    withRelease {
      step
      require(readColor(outField) == expected)
    }
  }

//  object TestSupersample extends Tester {
//    val dataRed = Matrix(
//      Array(
//        Array(1f, 7f, 5f),
//        Array(-2f, 0f, 4f),
//        Array(4f, -7f, 3f)
//      )
//    )
//    val dataGreen = dataRed * 2f
//    val dataBlue = dataRed * 3f
//    val input = ColorField(3, 3,
//      (r, c) => (1f, dataRed(r, c), dataGreen(r, c), dataBlue(r, c)))
//
//    val expectedDataRed = Matrix(
//      Array(
//        Array( 1f, 1f, 7f, 7f, 5f, 5f),
//        Array( 1f, 1f, 7f, 7f, 5f, 5f),
//        Array(-2f,-2f, 0f, 0f, 4f, 4f),
//        Array(-2f,-2f, 0f, 0f, 4f, 4f),
//        Array( 4f, 4f,-7f,-7f, 3f, 3f),
//        Array( 4f, 4f,-7f,-7f, 3f, 3f)
//      )
//    )
//    val expectedDataGreen = expectedDataRed * 2f
//    val expectedDataBlue = expectedDataRed * 3f
//
//    val expected = ColorField(6, 6,
//      (r, c) =>  (1f, expectedDataRed(r, c), expectedDataGreen(r, c),
//              expectedDataBlue(r, c)))
//    val inField = new DynamicColorField(input)
//    val outField = new DynamicColorField(6, 6) {
//      this <== inField.supersample
//    }
//
//    def check {
//      require(read(outField) == expected)
//    }
//  }
//
//  /** Test the expandBorder operator. */
//  object TestExpandBorder extends Tester {
//    val image1 = ScalarField(Matrix(
//      Array(1f, 2f),
//      Array(3f, 4f)
//    ))
//    val image2 = ScalarField(Matrix(
//      Array(1f, 2f, 2f, 1f),
//      Array(3f, 4f, 4f, 3f),
//      Array(3f, 4f, 4f, 3f),
//      Array(1f, 2f, 2f, 1f)
//    ))
//    val in = new DynamicScalarField(image1)
//    val expanded = new DynamicScalarField(4, 4) {
//      this <== in.expandBorder(4, 4)
//    }
//
//    def check {
//      require(read(expanded) == image2)
//    }
//  }

  /*
    test("All") {
    val app = new CogFragment {
      val tests = Array[Tester] (
        TestFieldConstant,
        TestFieldField,
        TestFieldConstantField,
        TestUnary,
        TestBinary,
        TestInjectorExtractor,
        TestSubfield,
        TestColorScalar,
        TestTrim,
        Test3D,
        Test3DSlice,
        TestUpsample,
        TestSupersample
      )

      /** Test combining a dynamic scalar field and a constant. */
      object TestFieldConstant extends Tester {
        val Size = 5

        val R = 3.21f
        val initField = ColorField.random(Size, Size)

        // Tests that involve equality should involve a threshold that matches
        // at least one element.  Here are some elements:
        val elem1 = initField.red.read(0,0)
        val elem2 = initField.green.read(Size-1,0)
        val elem3 = initField.blue.read(Size-1,Size-1)
        val elem4 = initField.red.read(0,Size-1)

        val a = new DynamicColorField(initField)
        val sum = new DynamicColorField(Size, Size) {this <== a + R}
        val diff = new DynamicColorField(Size, Size) {this <== a - R}
        val product = new DynamicColorField(Size, Size) {this <== a * R}
        val quotient = new DynamicColorField(Size, Size) {this <== a / R}
        //val power = new DynamicColorField(Size, Size) {this <== a pow 3}
        //val pown = new DynamicColorField(Size, Size) {this <== a pown 3}

        val constSum = new DynamicColorField(Size, Size) {this <== R + a}
        val constDiff = new DynamicColorField(Size, Size) {this <== R - a}
        val constProduct = new DynamicColorField(Size, Size) {this <== R * a}
        val constQuotient = new DynamicColorField(Size, Size) {this <== R / a}

        val greater = new DynamicColorField(Size, Size) {this <== (a > 0.5f)}
        val greaterEq = new DynamicColorField(Size, Size) {this <== (a >= elem1)}
        val less = new DynamicColorField(Size, Size) {this <== (a < 0.5f)}
        val lessEq = new DynamicColorField(Size, Size) {this <== (a <= elem2)}
        val eq = new DynamicColorField(Size, Size) {this <== (a === elem3)}
        val nEq = new DynamicColorField(Size, Size) {this <== (a !=== elem4)}

        def check {
          require(read(sum) == (read(a) + R))
          require(read(diff) == (read(a) - R))
          require(read(product) == (read(a) * R))
          require(read(quotient) ~== (read(a) / R))
          //require(read(power) ~== (read(a) :* read(a) :* read(a)))
          //require(read(pown) ~== (read(a) :* read(a) :* read(a)))

          require(read(constSum) == (read(a) + R))
          require(read(constDiff) == (-read(a) + R))
          require(read(constProduct) == (read(a) * R))
          require(read(constQuotient) ~== (read(a).reciprocal * R))

          require(read(greater) == read(a).map(mapPixel(_, _ > 0.5f)))
          require(read(greaterEq) == read(a).map(mapPixel(_, _ >= elem1)))
          require(read(less) == read(a).map(mapPixel(_, _ < 0.5f)))
          require(read(lessEq) == read(a).map(mapPixel(_, _ <= elem2)))
          require(read(eq) == read(a).map(mapPixel(_, _ == elem3)))
          require(read(nEq) == read(a).map(mapPixel(_, _ != elem4)))
        }
      }

      def mapPixel(pixel: Pixel, f: (Float) => Boolean): Pixel = {
        val red = if (f(pixel.red)) 1f else 0f
        val green = if (f(pixel.green)) 1f else 0f
        val blue = if (f(pixel.blue)) 1f else 0f
        new Pixel(red, green, blue)
      }

      def compare(a: Pixel, b: Pixel, f: (Float, Float) => Boolean): Pixel = {
        val red = if (f(a.red, b.red)) 1f else 0f
        val green = if (f(a.green, b.green)) 1f else 0f
        val blue = if (f(a.blue, b.blue)) 1f else 0f
        val alpha = 1f
        new Pixel(red, green, blue, alpha)
      }

      /** Test combining a DynamicColorField with a static ColorField. */
      object TestFieldConstantField extends Tester {
        val Size = 5
        val C = ColorField.random(Size, Size)
        val D = ColorField.random(Size, Size)
        val initField = ColorField.random(Size, Size)
        val a = new DynamicColorField(initField)
        val sum = new DynamicColorField(Size, Size) {this <== a + C}
        val sum2 = new DynamicColorField(Size, Size) {this <== (a + C) + C}
        val sum3 = new DynamicColorField(Size, Size) {this <== (a + C) + D}
        val diff = new DynamicColorField(Size, Size) {this <== a - C}
        val eq1 = new DynamicColorField(Size, Size) {this <== a === initField}
        val eq2 = new DynamicColorField(Size, Size) {this <== a === (initField * 0.5f)}
        val nEq1 = new DynamicColorField(Size, Size) {this <== (a !=== initField)}
        val nEq2 = new DynamicColorField(Size, Size) {this <== (a !=== (initField * 0.5f))}

        def check {
          require(read(sum) == (read(a) + C))
          require(read(sum2) == (read(a) + C + C))
          require(read(sum3) == (read(a) + C + D))
          require(read(diff) == (read(a) - C))
          require(read(eq1) == (read(a).combine(initField, Eq _)))
          require(read(eq2) == (read(a).combine(initField * 0.5f, Eq _)))
          require(read(nEq1) == (read(a).combine(initField, notEq _)))
          require(read(nEq2) == (read(a).combine(initField * 0.5f, notEq _)))
        }

        def Eq(a: Pixel, b: Pixel): Pixel = compare(a, b, _ == _)
        def notEq(a: Pixel, b: Pixel): Pixel = compare(a, b, _ != _)
      }

      /** Test combining two dynamic color fields. */
      object TestFieldField extends Tester {
        val Size = 5
        val a = new DynamicColorField(ColorField.random(Size, Size))
        val b = new DynamicColorField(ColorField.random(Size, Size))
        val d = new DynamicColorField(ColorField.random(Size, Size))

        val sum = new DynamicColorField(Size, Size) {this <== a + b}
        val diff = new DynamicColorField(Size, Size) {this <== a - b}
        val product = new DynamicColorField(Size, Size) {this <== a * b}
        val quotient = new DynamicColorField(Size, Size) {this <== a / b}
        val multiPoint = new DynamicColorField(Size, Size) {this <== a * b + d}

        /** A 1 x 1 color field, can be combined with any other. */
        val Constant = new Pixel(1f, 0.1f, 0.5f)
        val c = new DynamicColorField(new ColorField(1, 1,
          (r, c) => (Constant.red, Constant.green, Constant.blue)))

        val sum0D = new DynamicColorField(Size, Size) {this <== a + c}
        val diff0D = new DynamicColorField(Size, Size) {this <== a - c}
        val product0D = new DynamicColorField(Size, Size) {this <== a * c}
        val quotient0D = new DynamicColorField(Size, Size) {this <== a / c}

        val greater = new DynamicColorField(Size, Size) {this <== (a > b)}
        val greaterEq = new DynamicColorField(Size, Size) {this <== (a >= b)}
        val less = new DynamicColorField(Size, Size) {this <== (a < b)}
        val lessEq = new DynamicColorField(Size, Size) {this <== (a <= b)}
        val eq1 = new DynamicColorField(Size, Size) {this <== a === a}
        val eq2 = new DynamicColorField(Size, Size) {this <== a === b}
        val nEq1 = new DynamicColorField(Size, Size) {this <== (a !=== a)}
        val nEq2 = new DynamicColorField(Size, Size) {this <== (a !=== b)}

        def check {
          require(read(sum) == (read(a) + read(b)))
          require(read(diff) == (read(a) - read(b)))
          require(read(product) == (read(a) * read(b)))
          require(read(quotient) ~== (read(a) / read(b)))
          require(read(multiPoint) ~== ((read(a) * read(b)) + read(d)))

          require(read(sum0D) == (read(a) + Constant))
          require(read(diff0D) == (read(a) - Constant))
          require(read(product0D) == (read(a) * Constant))
          require(read(quotient0D) ~== (read(a) / Constant))

          val aa = read(a)
          val bb = read(b)
          require(read(greater) == aa.combine(bb, greaterThan _))
          require(read(greaterEq) == aa.combine(bb, greaterThanEq _))
          require(read(less) == aa.combine(bb, lessThan _))
          require(read(lessEq) == aa.combine(bb, lessThanEq _))
          require(read(eq1) == aa.combine(aa, Eq _))
          require(read(eq2) == aa.combine(bb, Eq _))
          require(read(nEq1) == aa.combine(aa, notEq _))
          require(read(nEq2) == aa.combine(bb, notEq _))
        }

        def greaterThan(a: Pixel, b: Pixel): Pixel = compare(a, b, _ > _)
        def greaterThanEq(a: Pixel, b: Pixel): Pixel = compare(a, b, _ >= _)
        def lessThan(a: Pixel, b: Pixel): Pixel = compare(a, b, _ < _)
        def lessThanEq(a: Pixel, b: Pixel): Pixel = compare(a, b, _ <= _)
        def Eq(a: Pixel, b: Pixel): Pixel = compare(a, b, _ == _)
        def notEq(a: Pixel, b: Pixel): Pixel = compare(a, b, _ != _)

      }

      /** Test applying unary operations on dynamic color fields. */
      object TestUnary extends Tester {

        val Size = 5
        val a = new DynamicColorField(ColorField.random(Size, Size) - 0.5f)
        val b = new DynamicColorField(ColorField.random(Size, Size) + 0.5f)
        val abs = new DynamicColorField(Size, Size) {this <== a.abs}
        val acos = new DynamicColorField(Size, Size) {this <== a.acos}

        val asin = new DynamicColorField(Size, Size) {this <== a.asin}
        val cos = new DynamicColorField(Size, Size) {this <== a.cos}
        val cosh = new DynamicColorField(Size, Size) {this <== a.cosh}
        val exp = new DynamicColorField(Size, Size) {this <== a.exp}
        val log = new DynamicColorField(Size, Size) {this <== b.log}
        //val normalizeL1 = new DynamicColorField(Size, Size) {this <== b.normalizeL1}
        //val normalizeL2 = new DynamicColorField(Size, Size) {this <== b.normalizeL2}
        val rectify = new DynamicColorField(Size, Size) {this <== a.rectify}
        //val signum = new DynamicColorField(Size, Size) {this <== a.signum}
        val sin = new DynamicColorField(Size, Size) {this <== a.sin}
        val sinh = new DynamicColorField(Size, Size) {this <== a.sinh}
        val sq = new DynamicColorField(Size, Size) {this <== a.sq}
        val sqrt = new DynamicColorField(Size, Size) {this <== b.sqrt}
        val tan = new DynamicColorField(Size, Size) {this <== a.tan}
        val tanh = new DynamicColorField(Size, Size) {this <== a.tanh}
        val negative = new DynamicColorField(Size, Size) {this <== -a}

        val Subsize = (Size + 1) / 2
        val downsample = new DynamicColorField(Subsize, Subsize) {
          this <== a.downsample()
        }

        def check {
          require(read(abs) == read(a).map(_.map(_.abs)))
          require(read(acos) ~== read(a).map(_.map(e => math.acos(e).toFloat)))
          require(read(asin) ~== read(a).map(_.map(e => math.asin(e).toFloat)))
          require(read(cos) ~== read(a).map(_.map(e => math.cos(e).toFloat)))
          require(read(cosh) ~== read(a).map(_.map(e => math.cosh(e).toFloat)))
          require(read(exp) ~== read(a).map(_.map(e => math.exp(e).toFloat)))
          require(read(log) ~== read(b).map(_.map(e => math.log(e).toFloat)))

          //      require(read(normalizeL1) ~== read(b) / read(b).reduce(_ + _))
          //      require(read(normalizeL2) ~==
          //            read(b) / (read(b).reduce(_ + _) * read(b).reduce(_ + _)))
          require(read(rectify) == read(a).map(_.map(_ max 0f)))
          //      require(read(signum) == read(a).map(e =>
          //        (if (e < 0) -1f else if (e > 0) 1f else 0f)))
          require(read(sin) ~== read(a).map(_.map(e => math.sin(e).toFloat)))
          require(read(sinh) ~== read(a).map(_.map(e => math.sinh(e).toFloat)))
          require(read(sq) == read(a).map(_.map(e => e * e)))
          require(read(sqrt) ~== read(b).map(_.map(e => math.sqrt(e).toFloat)))
          require(read(tan) ~== read(a).map(_.map(e => math.tan(e).toFloat)))
          require(read(tanh) ~== read(a).map(_.map(e => math.tanh(e).toFloat)))
          require(read(negative) == read(a) * -1f)
          require(read(downsample) == read(a).downsample())
        }
      }

      /** Test combinations of two dynamic color fields. */
      object TestBinary extends Tester {
        val Size = 5
        val a = new DynamicColorField(ColorField.random(Size, Size) - 0.5f)
        val b = new DynamicColorField(ColorField.random(Size, Size) + 0.5f)
        val max = new DynamicColorField(Size, Size) {this <== a max b}
        val min = new DynamicColorField(Size, Size) {this <== a min b}
        //val dot = new DynamicColorField(Size, Size) {this <== a dot b}
        val atan2 = new DynamicColorField(Size, Size) {this <== a.atan2(b)}

        def check {
          require(read(max) == read(a).combine(read(b), (a,b) => (a max b)))
          require(read(min) == read(a).combine(read(b), (a,b) => (a min b)))
          //require(read(dot) == (read(a) dot read(b)))
          require(read(atan2) ~== read(a).combine(read(b), (a,b) => (a atan2 b)))
        }
      }

      /** Test the apply(Range*) operator. */
      object TestSubfield extends Tester {
        val Size = 5
        val a = new DynamicColorField(ColorField.random(Size, Size))
        val SubSize = 3
        val b = new DynamicColorField(SubSize, SubSize) {
          this <== a(0 until SubSize, 0 until SubSize)
        }

        def check {
          require(read(a).subfield(0 until SubSize, 0 until SubSize) == read(b))
        }
      }

      /** Test InjectorExtractor with two input actuators and two output sensors. */
      object TestInjectorExtractor extends Tester {
        val Size = 5
        val actuatorA = new DynamicColorField(ColorField.random(Size, Size))
        val actuatorB = new DynamicColorField(ColorField.random(Size, Size))
        val sensorProd = new ColorSensor(Size, Size)
        val sensorSum = new ColorSensor(Size, Size)

        val animat = new InjectorExtractor  {
          name = "animat"
          // Statically declare injection and extraction connections
          this ~~> sensorSum
          this ~~> sensorProd
          this <~~ actuatorA
          this <~~ actuatorB

          // Each cycle: extract from actuators, inject into sensors.
          def compute {
            val a = extract(actuatorA)
            val b = extract(actuatorB)

            inject(sensorSum, a + b)
            inject(sensorProd, a * b)
          }

          def reset {
          }
        }
        def check {
          require(read(sensorSum) == read(actuatorA).combine(read(actuatorB), (a,b) => a + b ))
          require(read(sensorProd) == read(actuatorA).combine(read(actuatorB), (a,b) => a * b ))
        }
      }

      /** Test combining color fields and scalar fields. */
      object TestColorScalar extends Tester {
        val Size = 5
        val a = new DynamicColorField(ColorField.random(Size, Size))
        val b = new DynamicScalarField(ScalarField.random(Size, Size) + 1)
        val sum = new DynamicColorField(Size, Size) {this <== a + b}
        val diff = new DynamicColorField(Size, Size) {this <== a - b}
        val product = new DynamicColorField(Size, Size) {this <== a * b}
        val quotient = new DynamicColorField(Size, Size) {this <== a / b}

        def check {
          val bb = read(b)
          val bColor = new ColorField(bb, bb, bb)
          require(read(sum) == read(a).combine(bColor, (a, b) => a + b))
          require(read(diff) == read(a).combine(bColor, (a, b) => a - b))
          require(read(product) == read(a).combine(bColor, (a, b) => a * b))
          require(read(quotient) ~== read(a).combine(bColor, (a, b) => a / b))
        }
      }

      object TestTrim extends Tester {
        val Size = 5
        val trimShape = Shape(2, 3)
        val a = new DynamicColorField(ColorField.random(Size, Size))
        val b = new DynamicColorField(trimShape) {this <== a trim trimShape}
        val c = new DynamicColorField(trimShape) {
          this <== a(0 until trimShape(0), 0 until trimShape(1))
        }

        def check {
          require(read(b) == read(c))
        }
      }

      /** Test creation of 3D color fields. */
      object Test3D extends Tester {
        val Depth = 3
        val Rows = 5
        val Columns = 7
        val colorField = new ColorField(Depth, Rows, Columns, (depth, row, col) => (depth, row, col))
        val a = new DynamicColorField(colorField)

        def check {
        }
      }

      object Test3DSlice extends Tester {
        val Depth = 3
        val Rows = 4
        val Columns = 5
        val stack = new ColorField(Depth, Rows, Columns, (depth, row, col) => (depth, row, col))
        val stack0 = new ColorField(Rows, Columns, (row, col) => (0, row, col))
        val stack1 = new ColorField(Rows, Columns, (row, col) => (1, row, col))
        val stack2 = new ColorField(Rows, Columns, (row, col) => (2, row, col))

        val image = new ColorField(Rows, Columns, (row, col) => (row, col, 1.0f))
        val a = new DynamicColorField(stack)
        val b0 = DynamicColorField(a(0))
        val b1 = DynamicColorField(a(1))
        val b2 = DynamicColorField(a(2))

        def check {
          require(read(b0) == stack0)
          require(read(b1) == stack1)
          require(read(b2) == stack2)
        }
      }

      /* Commented out because NVIDIA does not support 3D image writes (AMD does)

      object Test3DPush extends Tester {
        val Depth = 3
        val Rows = 5
        val Columns = 7
        val stack = new ColorField(Depth, Rows, Columns, (depth, row, col) => (depth, row, col))
        val stack0 = new ColorField(Rows, Columns, (row, col) => (0, row, col))
        val stack1 = new ColorField(Rows, Columns, (row, col) => (1, row, col))
        val stack2 = new ColorField(Rows, Columns, (row, col) => (2, row, col))
        val image = new ColorField(Rows, Columns, (row, col) => (row, col, 1.0f))
        val a = new DynamicColorField(stack) {
          this <== this.push(image)
        }
        val b = DynamicColorField(a push image)
        val b0 = DynamicColorField(b(0))
        val b1 = DynamicColorField(b(1))
        val b2 = DynamicColorField(b(2))

        def check {
          require(read(b0) == image)
          require(read(b1) == stack0)
          require(read(b2) == stack1)

          println("TestColorFields not done yet")
        }
      }
      */

      object TestUpsample extends Tester {
        val dataRed = Matrix(
          Array(
            Array(1f, 7f, 5f),
            Array(-2f, 0f, 4f),
            Array(4f, -7f, 3f)
          )
        )
        val dataGreen = dataRed * 2f
        val dataBlue = dataRed * 3f
        val input = ColorField(3, 3,
          (r, c) => (1f, dataRed(r, c), dataGreen(r, c), dataBlue(r, c)))

        val expectedDataRed = Matrix(
          Array(
            Array( 1f, 0f, 7f, 0f, 5f, 0f),
            Array( 0f, 0f, 0f, 0f, 0f, 0f),
            Array(-2f, 0f, 0f, 0f, 4f, 0f),
            Array( 0f, 0f, 0f, 0f, 0f, 0f),
            Array( 4f, 0f,-7f, 0f, 3f, 0f),
            Array( 0f, 0f, 0f, 0f, 0f, 0f)
          )
        )
        val expectedDataGreen = expectedDataRed * 2f
        val expectedDataBlue = expectedDataRed * 3f

        val expected = ColorField(6, 6,
          (r, c) =>  (1f, expectedDataRed(r, c), expectedDataGreen(r, c),
                  expectedDataBlue(r, c)))
        val inField = new DynamicColorField(input)
        val outField = new DynamicColorField(6, 6) {
          this <== inField.upsample()
        }

        def check {
          require(read(outField) == expected)
        }
      }

      object TestSupersample extends Tester {
        val dataRed = Matrix(
          Array(
            Array(1f, 7f, 5f),
            Array(-2f, 0f, 4f),
            Array(4f, -7f, 3f)
          )
        )
        val dataGreen = dataRed * 2f
        val dataBlue = dataRed * 3f
        val input = ColorField(3, 3,
          (r, c) => (1f, dataRed(r, c), dataGreen(r, c), dataBlue(r, c)))

        val expectedDataRed = Matrix(
          Array(
            Array( 1f, 1f, 7f, 7f, 5f, 5f),
            Array( 1f, 1f, 7f, 7f, 5f, 5f),
            Array(-2f,-2f, 0f, 0f, 4f, 4f),
            Array(-2f,-2f, 0f, 0f, 4f, 4f),
            Array( 4f, 4f,-7f,-7f, 3f, 3f),
            Array( 4f, 4f,-7f,-7f, 3f, 3f)
          )
        )
        val expectedDataGreen = expectedDataRed * 2f
        val expectedDataBlue = expectedDataRed * 3f

        val expected = ColorField(6, 6,
          (r, c) =>  (1f, expectedDataRed(r, c), expectedDataGreen(r, c),
                  expectedDataBlue(r, c)))
        val inField = new DynamicColorField(input)
        val outField = new DynamicColorField(6, 6) {
          this <== inField.supersample
        }

        def check {
          require(read(outField) == expected)
        }
      }

      /** Test the expandBorder operator. */
      object TestExpandBorder extends Tester {
        val image1 = ScalarField(Matrix(
          Array(1f, 2f),
          Array(3f, 4f)
        ))
        val image2 = ScalarField(Matrix(
          Array(1f, 2f, 2f, 1f),
          Array(3f, 4f, 4f, 3f),
          Array(3f, 4f, 4f, 3f),
          Array(1f, 2f, 2f, 1f)
        ))
        val in = new DynamicScalarField(image1)
        val expanded = new DynamicScalarField(4, 4) {
          this <== in.expandBorder(4, 4)
        }

        def check {
          require(read(expanded) == image2)
        }
      }
    }

    app.step(2)
    app.tests.foreach(_.check)
    app.quit()
  }

   */
}
