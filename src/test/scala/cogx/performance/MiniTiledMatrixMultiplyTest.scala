package cogx.performance

import cogx.platform.opencl.OpenCLPlatform
import libcog._

import scala.collection.mutable

/** A performance-only regression test of the MatrixMatrixTransform0DFieldTiledHyperKernel.
  *
  * @author Dick Carter
  */
object MiniTiledMatrixMultiplyTest extends App {

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

  val miniTileRowSeq = Seq(2, 4)
  val miniTileColumnSeq = Seq(2, 4)

  /** Core routine that builds and times a crossCorrelateFilterAdjoint - blockReduceSum operator pair. */
  def timeTransformTiled(tp: TestParam) = {
    import tp._
    val m1Shape = Shape(matrix1Rows, matrix1Columns)
    val matrix2Rows = matrix1Columns
    val m2Shape = Shape(matrix2Rows, matrix2Columns)
    println(s"Starting test with (m1Shape, m2Shape, miniTileRows, miniTileColumns) = ($m1Shape, $m2Shape, $miniTileRows, $miniTileColumns)")
    val cg = new ComputeGraph(device = Some(device)) {
      val m1 = MatrixField(Shape(), m1Shape)
      val m2 = MatrixField(Shape(), m2Shape)
      val output = transformTiled(m1, m2, miniTileRows, miniTileColumns)

      probe(output)
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

  case class TestParam(matrix1Rows: Int, matrix1Columns: Int, matrix2Columns: Int, miniTileRows: Int, miniTileColumns: Int, device: Int)

  println(s"Tiled transform operation regression over tuning parameters for devices $deviceRange")

  // AlexNet Layer 1 convolution-as-matrix-multiply
  val m1Rows = 96
  val m1Columns = 387200
  val m2Columns = 363
  // Generic bit matrix multiply
//  val m1Rows = 1024
//  val m1Columns = 1024
//  val m2Columns = 1024

  for (device <- deviceRange) {
      println(s"\n***************  Beginning testing of (m1Rows = $m1Rows, m1Columns = $m1Columns, m2Columns = $m2Columns) " +
        s"on device $device (${OpenCLPlatform().devices(device)}) ******************\n")
      val results = mutable.HashMap[TestParam, Double]()

      val testCases =
        for (miniTileRows <- miniTileRowSeq;  miniTileColumns <- miniTileColumnSeq)
          yield (TestParam(m1Rows, m1Columns, m2Columns, miniTileRows, miniTileColumns, device))

      for (testCase <- testCases) {
        val newTime = timeTransformTiled(testCase)
        results.put(testCase, newTime)
        if (coolDownSeconds > 3) print(s"Sleeping for $coolDownSeconds sec to let the GPU cool...")
        Thread.sleep(coolDownSeconds * 1000)
        if (coolDownSeconds > 3) println("done.\n")

        println(f"Test time for (miniTileRows, miniTileColumns) = (${testCase.miniTileRows}%d, ${testCase.miniTileColumns}%d) = $newTime%.2f ms")

      }
  }

}
