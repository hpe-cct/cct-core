package cogx.performance

import cogx.platform.opencl.OpenCLPlatform
import libcog._

import scala.collection.mutable

/** A performance-only regression test of the matrix-multiple approach to the filterAdjoint operation.
  * Editable constants dictate which devices are tested and a variety of other parameters like stride, filtersize, etc.
  *
  * This test was used to determine the heuristic that the matrix multiply optimized approach was better always for
  * stride > 3 and for filter sizes above 5 for stride == 2.  With this now the baked-in policy, you may want to
  * set the variable 'enablePruningBasedOnStride' to false within the ConvolveHyperKernel object before re-running
  * this test.
  *
  * @author Dick Carter
  */
object FilterAdjointTensorReduceTest extends App {

  // Some constants effecting the testing
  val warmUpSteps = 50
  val testSteps = 150
  val coolDownSeconds = 1
  val deviceNum = -1 // If -1 then all devices

  val numDevices = OpenCLPlatform().devices.length
  val deviceRange =
    if (deviceNum == -1)
      0 to numDevices - 1
    else
      deviceNum to deviceNum

  val batchSizes = Seq(32)
//  val batchSizes = Seq(32, 64, 128)
  val inputSizes = Seq(64, 128, 256)
  val filterSizes = Seq(3, 5, 7, 9, 11)
  val strides = Seq(2, 3, 4)
  val optimizations = Seq(true, false)  // Don't change this- some program logic below depends on this.

  /** Core routine that builds and times a crossCorrelateFilterAdjoint - blockReduceSum operator pair. */
  def timeFilterAdjointTensorReduce(tp: TestParam) = {
    import tp._
    println(s"Starting test with (inputSize, stride, filterSize, batchSize, optimize) = ($inputSize, $stride, $filterSize, $batchSize, $allowReduction)")
    Cog.filterAdjointMerging = allowReduction
    val cg = new ComputeGraph(device = Some(device)) {

      // Calculate the field sizes
      //
      // In the forward direction, we view the operation as:   image-in projectFrame filter -> image-out
      //
      // The corresponding filterAdjoint operation is:         image crossCorrelateFilterAdjoint gradient -> filter

      val filterTensorSize = inputFeatures * outputFeatures

      val gradientFieldSize = (inputSize - filterSize + 1) / stride
      val gradientFieldShape = Shape(gradientFieldSize, gradientFieldSize)
      val gradientTensorShape = Shape(tp.outputFeatures * batchSize)

      val actualImageSize = gradientFieldSize * stride - 1 + filterSize
      val imageFieldShape = Shape(actualImageSize, actualImageSize)
      val imageTensorShape = Shape(inputFeatures * batchSize)

      val image = VectorField(imageFieldShape, imageTensorShape)
      val gradient = VectorField(gradientFieldShape, gradientTensorShape)

      val sampling = if (stride > 1) UpsampleInputConvolution(stride) else NoSamplingConvolution

      val unReduced = crossCorrelateFilterAdjoint(image, gradient, BorderValid, sampling, batchSize)
      val filter = blockReduceSum(unReduced, batchSize)

      probe(filter)
    }
    cg.reset
    cg.step(warmUpSteps)
    val start = System.nanoTime()
    cg.step(testSteps)
    val durationMsec = (System.nanoTime() - start)/1000000.0
    val stepTimeMsec =  durationMsec/testSteps
    val stepfreq =  1000.0/stepTimeMsec
    println(f"Step time = $stepTimeMsec%.3f msec. (freq = $stepfreq%.3f Hz)")
    cg.release
    stepTimeMsec
  }

  case class TestParam(inputFeatures: Int, outputFeatures: Int, inputSize: Int, stride: Int, filterSize: Int, batchSize: Int, allowReduction: Boolean, device: Int)

  println(s"FilterAdjoint operation regression over tuning parameters for devices $deviceRange")

  // Loop over selected devices and layers.  Then perform an inner test loop over the kernel tuning parameters.

  val inputLayerFeatures = 3        // AlexNet layer 1 input
  val outputLayerFeatures = 96      // AlexNet layer 2 output

  for (device <- deviceRange) {
      println(s"\n***************  Beginning testing of (inLayers = $inputLayerFeatures, outLayers = $outputLayerFeatures) " +
        s"on device $device (${OpenCLPlatform().devices(device)}) ******************\n")
      val results = mutable.HashMap[TestParam, Double]()

      val testCases =
        for (inputSize <- inputSizes;  stride <- strides; filterSize <- filterSizes; batchSize <- batchSizes; allow <- optimizations)
          yield (TestParam(inputLayerFeatures, outputLayerFeatures, inputSize, stride, filterSize, batchSize, allow, device))

      var previousTime = 0.0

      for (testCase <- testCases) {
        val newTime = timeFilterAdjointTensorReduce(testCase)
        results.put(testCase, newTime)
        if (coolDownSeconds > 3) print(s"Sleeping for $coolDownSeconds sec to let the GPU cool...")
        Thread.sleep(coolDownSeconds * 1000)
        if (coolDownSeconds > 3) println("done.\n")

        if (testCase.allowReduction == false) {
          val optimizedTime = previousTime
          val unoptimizedTime = newTime
          // Expansion factor of image input to Toeplitz matrix is (filterSize / stride)^2
          // As a metric though filterSize / stride was not seen to be predictive of which approach was better.
//          val expansionMetric = testCase.filterSize.toFloat / testCase.stride
          val best = if (optimizedTime < unoptimizedTime) "optimized" else "unoptimized"
          println(f"\n(optimzed, unoptimized) times were ($previousTime%.2f ms, $newTime%.2f ms), best was $best.\n")
        }
        previousTime = newTime
      }
  }

}
