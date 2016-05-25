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

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner
import cogx.reference.{RefTestInterface, RefScalarField}
import cogx.helper.{MatrixFieldBuilderInterface, VectorFieldBuilderInterface, ScalarFieldBuilderInterface}
import cogx.compiler.parser.op.{PlaneByPlane, ConvolveOp}
import java.io.{DataInputStream, FileInputStream, File}
import cogx.helper.fieldio.FieldIO
import cogx.platform.types.ConvolutionOrientation

@RunWith(classOf[JUnitRunner])
class DynamicConvolutionSpec extends FunSuite
                             with ScalarFieldBuilderInterface
                             with VectorFieldBuilderInterface
                             with MatrixFieldBuilderInterface
                             with FieldIO
{
  import scala.language.reflectiveCalls

  val Optimize = true

  private def importHeader(baseDir: String, index: Int): ConvolveOp = {
    val inStream = new DataInputStream( new FileInputStream( new File( baseDir + index + ".hdr")))

    //read sampling policy portion of the header
    val samplePolicyChars = inStream.readInt()
    val samplePolicyString = Array.tabulate(samplePolicyChars){
      (i) => inStream.readChar.toString
    }.reduce(_+_)
    val samplePolicyStep = inStream.readInt()

    val samplingPolicy:ConvolutionSamplingPolicy = samplePolicyString match{
      case "UpsampleInputConvolution" => UpsampleInputConvolution(samplePolicyStep)
      case "DownsampleOutputConvolution" => DownsampleOutputConvolution(samplePolicyStep)
      case "NoSamplingConvolution" => NoSamplingConvolution
      case _ => throw new RuntimeException("invalid sampling policy")
    }

    //read border policy portion of the header
    val borderPolicyChars = inStream.readInt()
    val borderPolicyString = Array.tabulate(borderPolicyChars){
      (i) => inStream.readChar.toString
    }.reduce(_+_)
    val borderPolicy:BorderPolicy = borderPolicyString match{
      case "BorderClamp" => BorderClamp
      case "BorderCyclic" => BorderCyclic
      case "BorderFull" => BorderFull
      case "BorderValid" => BorderValid
      case "BorderZero" => BorderZero
      case _ => throw new RuntimeException("invalid border policy")
    }

    ConvolveOp(borderPolicy,ConvolutionOrientation,samplingPolicy,
      PlaneByPlane, batchSize = 1)
  }

  test("convolveSeparable") {
    val Size = 10
    val rowKernel = Vector(0.1f, 0.3f, 0.4f, 0.2f, 0.0f)
    val colKernel = Vector(0.0f, 0.5f, -0.4f, -0.2f, -0.1f)
    val squareKernel: Matrix = colKernel outerProduct rowKernel

    val initField = RefScalarField.random(Size, Size)
    val rowField = RefScalarField(rowKernel)
    val colField = RefScalarField(colKernel)
    val squareField = RefScalarField(squareKernel)


    val graph = new ComputeGraph(Optimize) with RefTestInterface {
      val a = TestScalarField(initField)
      val rowFilter = TestScalarField(rowField)
      val colFilter = TestScalarField(colField)
      val squareFilter = TestScalarField(squareField)

      val smooth = convolve(a, squareFilter, BorderZero)
      val smoothSeparable = convolveSeparable(a, rowFilter, colFilter, BorderZero)

      val crossSmooth = crossCorrelate(a, squareFilter, BorderZero)
      val crossSmoothSeparable = crossCorrelateSeparable(a, rowFilter, colFilter, BorderZero)

      probe(smooth, smoothSeparable, crossSmooth, crossSmoothSeparable)
    }
    import graph._
    withRelease {
      step
      //readScalar(graph.a).print
      //readScalar(smooth).print
      //readScalar(smoothSeparable).print

      require(readScalar(smooth) ~== readScalar(smoothSeparable))
      require(readScalar(crossSmooth) ~== readScalar(crossSmoothSeparable))
    }
  }

  //This bit of code previously created an OpenCL compiler failure due to an incomplete if statement in
  // the ConvolveHyperKernel.
  test("Shape based OpenCL compiler failure"){
    val cg = new ComputeGraph{
      val inputLen = 3
      val (inRows, inCols) = (70,70)
      val filterSize = 7
      val numFilters = 64

      val x = VectorField.random(Shape(inRows, inCols), Shape(inputLen))
      val fb = VectorField.random(Shape(filterSize,filterSize),Shape(inputLen*numFilters))
      val z = projectFrame(x, fb, BorderValid, NoSamplingConvolution)
      probe(z)
    }
    cg.step
    cg.release
  }

  test("convolution"){
    val firstChoiceBaseDir = "src/test/resources/platform/opencl/hyperkernels/convolutionDynamic/"

    val baseDir =
      if (new File(firstChoiceBaseDir).isDirectory)
        firstChoiceBaseDir
      else
        "../../" + firstChoiceBaseDir

    val numRefs = 25

    case class Test(x: RefScalarField, y: RefScalarField, k: RefScalarField,
                    wty: RefScalarField, wx: RefScalarField, op: ConvolveOp)

    // We don't run all the tests necessarily
    val candidateTests = Array.tabulate(numRefs) {
      i => Test(
        readScalarFieldFromFile(new File(baseDir + "x" + i + ".fld")),   // x
        readScalarFieldFromFile(new File(baseDir + "y" + i + ".fld")),   // y
        readScalarFieldFromFile(new File(baseDir + "k" + i + ".fld")),   // k
        readScalarFieldFromFile(new File(baseDir + "wty" + i + ".fld")), // wty
        readScalarFieldFromFile(new File(baseDir + "wx" + i + ".fld")),  // wx
        importHeader(baseDir, i )
      )
    }
    // Filter out tests that don't need to be performed (BorderClamp upsampling).
    def excludeTest(test: Test) = test.op.samplingPolicy match {
      case UpsampleInputConvolution(step) =>
        step > 1 && test.op.borderPolicy == BorderClamp
      case _ => false
    }

    val tests = candidateTests.filter(!excludeTest(_))
    val numTests = tests.length

    val graph = new ComputeGraph(Optimize) with RefTestInterface {
      val wxCalcs: Array[Field] = tests.map(
        test => {
          val inputCur = TestScalarField(test.x)
          val filterCur = TestScalarField(test.k)
          convolve(inputCur, filterCur,
            test.op.borderPolicy,
            test.op.samplingPolicy)
        }
      )
      probe(wxCalcs: _*)
    }

    import graph._
    withRelease {
      step
      val wxResult = Array.tabulate(numTests) {
        (i) => readScalar(wxCalcs(i))
      }
      val wxPassed = Array.tabulate(numTests) {
        (i) => {
          val passed = tests(i).wx ~== wxResult(i)
          System.out.print("test " + i + " : " + tests(i).op)
          println(if (passed) " passed." else " failed.")
          passed
        }
      }

      for (result <- wxPassed)
        require(result, "test failed")
    }
  }
  //
  //  The following test throws a CL_INVALID_KERNEL_NAME error if the previous
  //  test fails and doesn't perform a release.  If you want to debug this behavior,
  //  put a "require(false)" statement *before* the try block in the above test.
  //
  //  test("convolution - CL_INVALID_KERNEL_NAME error") {
  //    val matrix = Matrix(
  //      Array(1f, 2f),
  //      Array(3f, 4f)
  //    )
  //
  //    val filter = Matrix(
  //      Array(0.25f, 0.25f, 0.25f),
  //      Array(0.25f, 0.25f, 0.25f),
  //      Array(0.25f, 0.25f, 0.25f)
  //    )
  //    val graph = new ComputeGraph(Optimize) {
  //      val in = TestScalarField(RefScalarField(matrix))
  //      val kernel = TestScalarField(RefScalarField(filter))
  //      val upsampled = in.upsample(3)
  //      val separate = upsampled.convolve(kernel, BorderCyclic)
  //      val integrated = in.convolve(kernel, BorderCyclic,
  //           ConvolutionOrientation, UpsampleInputConvolution(3))
  //    }
  //
  //    import graph._
  //    withRelease {
  //      step
  //      //    readScalar(separate).print
  //      //    println("------------------------")
  //      //    readScalar(integrated).print
  //      require(readScalar(separate) == readScalar(integrated))
  //    }
  //  }

  //  We decided not to support BorderClamp with integrated upsampling
  //
  //  /** Test the integrated upsampling with border clamp, the way our kernel
  //    * implements it.  This does not match the behavior of separate upsampling
  //    * followed by convolution, which treats the borders quite differently:
  //    * the right and bottom edges have 0's propogated out, and the left and
  //    * top edges have densely populated values, unlike the interior.
  //    */
  //  test("convolution - simple upsample with border clamp") {
  //    val matrix = Matrix(
  //      Array(1f, 2f),
  //      Array(3f, 4f)
  //    )
  //
  //    // Expand with clamped borders, followed by upsampling(2)
  //
  //    val handUpsampledWithClampedBorder = Matrix(
  //      Array(1f, 0f, 1f, 0f, 2f, 0f, 2f, 0f),
  //      Array(0f, 0f, 0f, 0f, 0f, 0f, 0f, 0f),
  //      Array(1f, 0f, 1f, 0f, 2f, 0f, 2f, 0f),
  //      Array(0f, 0f, 0f, 0f, 0f, 0f, 0f, 0f),
  //      Array(3f, 0f, 3f, 0f, 4f, 0f, 4f, 0f),
  //      Array(0f, 0f, 0f, 0f, 0f, 0f, 0f, 0f),
  //      Array(3f, 0f, 3f, 0f, 4f, 0f, 4f, 0f),
  //      Array(0f, 0f, 0f, 0f, 0f, 0f, 0f, 0f)
  //    )
  //
  //    val filter = Matrix(
  //      Array(1f, 1f, 1f, 1f, 1f),
  //      Array(1f, 1f, 1f, 1f, 1f),
  //      Array(1f, 1f, 1f, 1f, 1f),
  //      Array(1f, 1f, 1f, 1f, 1f),
  //      Array(1f, 1f, 1f, 1f, 1f)
  //    )
  //
  //    val graph = new ComputeGraph(Optimize) {
  //      val in = TestScalarField(RefScalarField(matrix))
  //      val kernel = TestScalarField(RefScalarField(filter))
  //      val integratedUpsampling = in.convolve(kernel, BorderClamp, ConvolutionOrientation, UpsampleInputConvolution(2))
  //      val handUpsampled = TestScalarField(RefScalarField(handUpsampledWithClampedBorder))
  //      val expected = handUpsampled.convolve(kernel, BorderValid)
  //    }
  //
  //    import graph._
  //    withRelease {
  //      step
  ////    readScalar(expected).print
  ////    println("------------------------")
  ////    readScalar(integrated).print
  //      require(readScalar(expected) == readScalar(integratedUpsampling))
  //    }
  //  }

}