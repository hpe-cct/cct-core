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

package cogx.runtime.checkpoint

import libcog._
import cogx.platform.checkpoint._

import scala.language.reflectiveCalls
import java.io.File
import java.nio.file.{Paths, Files}

/** Generic test code for checkpointing serializers.
  *
  * Checkpointing is independent of serialization technology.  Once the checkpointer object
  * is defined, any serializer package can use these tests for verification.
  *
  * @author Dick Carter
  */

trait CheckpointTester {

  def removeFile(filename: String): Unit = {
    val f = new File(filename)
    // java.nio.file.Files.delete() gives a relatively detailed error message
    // when the operation fails, unlike the old delete method on java.io.File.
    val path = Paths.get(f.toURI)
    Files.deleteIfExists(path)
    Predef.require(!Files.exists(path), s"Can't remove filename prior to attempt to create it.")
  }

  def primitivesTest(checkpointerType: CheckpointerType): Unit = {
    val rng = new Random()

    /** 'require' that includes random seed that generated the failure to help in repeating the problem. */
    def require(cond: Boolean, msg: String) = {
      val seedMsg = s"\nrng seed = ${rng.seed}"
      Predef.require(cond, msg + seedMsg)
    }

    val filename = checkpointerType.addSuffixIfAbsent("hdf5PrimitivesTest")
    // Make sure we're starting with no file present
    removeFile(filename)

    val saver = Checkpointer.getSaver(filename, checkpointerType)

    // set up values to be written
    val theInt = rng.nextInt()
    val theLong = rng.nextLong()
    val theFloat = rng.nextFloat()
    val theString = "some (not so) random text"
    val theIntArray = Array.tabulate(5){ i => rng.nextInt() }
    val theLongArray = Array.tabulate(7){ i => rng.nextLong() }
    val theFloatArray = Array.tabulate(25){ i => rng.nextFloat() }
    val theStringArray = theString.split(' ')

    // write single primitives
    saver.writeInt("anInt", theInt)
    saver.writeLong("aLong", theLong)
    saver.writeFloat("aFloat", theFloat)
    saver.writeString("aString", theString)
    // write arrays of primitives
    saver.writeIntArray("anIntArray", theIntArray)
    saver.writeLongArray("aLongArray", theLongArray)
    saver.writeFloatArray("aFloatArray", theFloatArray)
    saver.writeStringArray("aStringArray", theStringArray)

    saver.close()

    val restorer = Checkpointer.getRestorer(filename, checkpointerType)

    // read single primitives
    val readInt = restorer.readInt("anInt")
    val readLong = restorer.readLong("aLong")
    val readFloat = restorer.readFloat("aFloat")
    val readString = restorer.readString("aString")
    // read arrays of primitives
    val readIntArray = restorer.readIntArray("anIntArray")
    val readLongArray = restorer.readLongArray("aLongArray")
    val readFloatArray = restorer.readFloatArray("aFloatArray")
    val readStringArray = restorer.readStringArray("aStringArray")

    restorer.close()

    // remove file written (before any requires that might fail)
    removeFile(filename)

    require(theInt == readInt, s"Save/Restore of Int fails: wrote $theInt, read $readInt")
    require(theLong == readLong, s"Save/Restore of Long fails: wrote $theLong, read $readLong")
    require(theFloat == readFloat, s"Save/Restore of Float fails: wrote $theFloat, read $readFloat")
    require(theString == readString, s"Save/Restore of String fails: wrote $theString, read $readString")

    require(theIntArray.length == readIntArray.length &&
      theIntArray.zip(readIntArray).map(x => (x._1 == x._2)).foldLeft(true)(_ && _),
      "Save/Restore of Array[Int] fails: wrote " + theIntArray.mkString(" ") +
        ", read " + readIntArray.mkString(" "))

    require(theLongArray.length == readLongArray.length &&
      theLongArray.zip(readLongArray).map(x => (x._1 == x._2)).foldLeft(true)(_ && _),
      "Save/Restore of Array[Long] fails: wrote " + theLongArray.mkString(" ") +
        ", read " + readLongArray.mkString(" "))

    require(theFloatArray.length == readFloatArray.length &&
      theFloatArray.zip(readFloatArray).map(x => (x._1 == x._2)).foldLeft(true)(_ && _),
      "Save/Restore of Array[Float] fails: wrote " + theFloatArray.mkString(" ") +
        ", read " + readFloatArray.mkString(" "))

    require(theStringArray.length == readStringArray.length &&
      theStringArray.zip(readStringArray).map(x => (x._1 == x._2)).foldLeft(true)(_ && _),
      "Save/Restore of Array[String] fails: wrote " + theStringArray.mkString("'", "' '", "'") +
        ", read " + readStringArray.mkString("'", "' '", "'"))

  }

  def objectsTest(checkpointerType: CheckpointerType): Unit = {
    val rng = new Random()

    /** 'require' that includes random seed that generated the failure to help in repeating the problem. */
    def require(cond: Boolean, msg: String) = {
      val seedMsg = s"\nrng seed = ${rng.seed}"
      Predef.require(cond, msg + seedMsg)
    }

    val filename = checkpointerType.addSuffixIfAbsent("hdf5ObjectsTest")
    // Make sure we're starting with no file present
    removeFile(filename)

    val saver = Checkpointer.getSaver(filename, checkpointerType)

    /** A simple class with String, Int and Float members that can save itself. */
    class AnObject(val aString: String, val anInt: Int, val aFloat: Float) extends Saveable {
      def save(saver: ObjectSaver) {
        saver.writeString("aString", aString)
        saver.writeInt("anInt", anInt)
        saver.writeFloat("aFloat", aFloat)
      }

      /** Skip overriding hashCode since we're not storing AnObject in collections */
      override def equals(other: Any) = other match {
        case that: AnObject =>
          (this.aString == that.aString) && (this.anInt == that.anInt) && (this.aFloat == that.aFloat)
        case _ =>
          false
      }

      override def toString = s"('$aString',$anInt,$aFloat)"
    }

    /** A factory object for creating random AnObjects and restoring them */
    object AnObject extends RestoreFactory {
      def random() = {
        new AnObject("String#" + rng.nextInt(), rng.nextInt(), rng.nextFloat)
      }
      def restore(restorer: ObjectRestorer): AnObject  = {
        val readString = restorer.readString("aString")
        val readInt = restorer.readInt("anInt")
        val readFloat = restorer.readFloat("aFloat")
        new AnObject(readString, readInt, readFloat)
      }
    }

    // set up values to be written
    val theObject = AnObject.random()
    val theObjectArray = Array.tabulate(5) { i => AnObject.random() }

    // write values
    saver.writeObject("anObject", theObject)
    saver.writeObjectArray("objects", theObjectArray.asInstanceOf[Array[Saveable]])

    saver.close()

    val restorer = Checkpointer.getRestorer(filename, checkpointerType)

    // read values
    val readObject = restorer.readRestorable("anObject", AnObject)
    val readObjectArray = restorer.readRestorableArray("objects", AnObject)

    restorer.close()

    // remove file written (before any requires that might fail)
    removeFile(filename)

    require(theObject == readObject, s"Save/Restore of Objects fails: wrote $theObject, read $readObject")

    require(theObjectArray.length == readObjectArray.length &&
      theObjectArray.zip(readObjectArray).map(x => (x._1 == x._2)).foldLeft(true)(_ && _),
      "Save/Restore of Array[AnObject] fails: wrote " + theObjectArray.mkString(" ") +
        ", read " + readObjectArray.mkString(" "))

  }

  def simpleComputeGraphTest(checkpointerType: CheckpointerType): Unit = {
    val filename = checkpointerType.addSuffixIfAbsent("AplusB")
    val Rows = 10
    val Cols = 12
    def fieldVal(r: Int, c: Int) = Cols*r + c
    val graph = new ComputeGraph {
      val A = ScalarField(Rows, Cols, (r, c) => fieldVal(r, c))
      val B = ScalarField(Rows, Cols, (r, c) => 10*fieldVal(r, c))
      A <== A + B
      A.probe()
      B.probe()
    }
    graph.computeGraphName = "AplusB"
    graph.description = "Simple ComputeGraph for unit testing deserialization"
    removeFile(filename)
    try {
      graph.withRelease {
        graph.step(1)
        graph.write(filename, checkpointerType)
      }
      val graph2 = ComputeGraph.readFromFile(filename, checkpointerType)
      graph2.withRelease {
        for (r <- 0 until Rows; c <- 0 until Cols) {
          val expected = 11 * fieldVal(r, c)
          val actual = graph2.readByName("A").asInstanceOf[ScalarFieldReader].read(r, c)
          require(actual == expected, "Restored ComputeGraph has improper state.")
        }
        graph2.step(1)
        for (r <- 0 until Rows; c <- 0 until Cols) {
          val expected = 21 * fieldVal(r, c)
          val actual = graph2.readByName("A").asInstanceOf[ScalarFieldReader].read(r, c)
          require(actual == expected, "Restored ComputeGraph cannot step properly.")
        }
      }
    } finally {
      removeFile(filename)
    }
  }


  /** The common part of a number of tests involving sensors.  The parameters include a hook
    * to generate the particular sensor being tested, which can be pipelined vs. unpipelined, or can
    * involve a separate sensor class vs.just a factory object.
    *
    * @param checkpointerType The checkpointer type, e.g. hdf5 or java serialization.
    * @param filename The filename to use to save and restore the compute graph.
    * @param initialSteps How many steps the model is stepped before it is saved.
    * @param postRestoreSteps After the model is restored and checked, how many further steps before it's checked again.
    * @param sensorGenerator A function that makes the sensor varietal being tested.
    */
  private def commonSensorComputeGraphTest(checkpointerType: CheckpointerType,
                                   filename: String,
                                   initialSteps: Int,
                                   postRestoreSteps: Int,
                                   sensorGenerator: () => Field): Unit = {

    val graph = new ComputeGraph {
      val A = sensorGenerator()
      val B = A + 1
      A.probe()
      B.probe()
    }
    val Rows = graph.A.rows
    val Cols = graph.A.columns

    def fieldVal(r: Int, c: Int) = Cols*r + c

    val suffixedFilename = checkpointerType.addSuffixIfAbsent(filename)
    removeFile(suffixedFilename)
    try {
      graph.withRelease {
        graph.step(initialSteps)
        graph.write(suffixedFilename, checkpointerType)
      }
      val graph2 = ComputeGraph.readFromFile(suffixedFilename, checkpointerType)
      graph2.withRelease {
        for (r <- 0 until Rows; c <- 0 until Cols) {
          val expected = initialSteps + fieldVal(r, c)
          val actual = graph2.readByName("A").asInstanceOf[ScalarFieldReader].read(r, c)
          require(actual == expected, "Restored ComputeGraph has improper state.")
        }
        graph2.step(postRestoreSteps)
        for (r <- 0 until Rows; c <- 0 until Cols) {
          val expected = initialSteps + postRestoreSteps + fieldVal(r, c)
          val actual = graph2.readByName("A").asInstanceOf[ScalarFieldReader].read(r, c)
          require(actual == expected, "Restored ComputeGraph cannot step properly.")
        }
      }
    } finally
      removeFile(suffixedFilename)
  }

  /** A test of an unpipelined sensor with a separate sensor class. */
  def unpipelinedSensorComputeGraphTest(checkpointerType: CheckpointerType): Unit = {
    val filename = "unpipelinedSensorTest"
    val InitialSteps = 1
    val PostRestoreSteps = 2
    val Rows = 10
    val Cols = 12

    commonSensorComputeGraphTest(checkpointerType,
      filename,
      InitialSteps,
      PostRestoreSteps,
      () => UnpipelinedTestSensor(Rows, Cols)
    )
  }

  /** A test of an pipelined sensor with a separate sensor class. */
  def pipelinedSensorComputeGraphTest(checkpointerType: CheckpointerType): Unit = {
    val filename = "pipelinedSensorTest"
    val InitialSteps = 2
    val PostRestoreSteps = 3
    val Rows = 10
    val Cols = 12

    commonSensorComputeGraphTest(checkpointerType,
      filename,
      InitialSteps,
      PostRestoreSteps,
      () => PipelinedTestSensor(Rows, Cols)
    )
  }

  /** A test of an unpipelined sensor with no separate sensor class, i.e. just the factory object. */
  def simpleUnpipelinedSensorComputeGraphTest(checkpointerType: CheckpointerType): Unit = {
    val filename = "simpleUnpipelinedSensorTest"
    val InitialSteps = 2
    val PostRestoreSteps = 3
    val Rows = 10
    val Cols = 12

    commonSensorComputeGraphTest(checkpointerType,
      filename,
      InitialSteps,
      PostRestoreSteps,
      () => SimpleUnpipelinedTestSensor(Rows, Cols)
    )
  }

  /** A test of an pipelined sensor with no separate sensor class, i.e. just the factory object. */
  def simplePipelinedSensorComputeGraphTest(checkpointerType: CheckpointerType): Unit = {
    val filename = "simplePipelinedSensorTest"
    val InitialSteps = 2
    val PostRestoreSteps = 3
    val Rows = 10
    val Cols = 12

    commonSensorComputeGraphTest(checkpointerType,
      filename,
      InitialSteps,
      PostRestoreSteps,
      () => SimplePipelinedTestSensor(Rows, Cols)
    )
  }

  /** The common part of a number of tests involving color sensors.  The parameters include a hook
    * to generate the particular sensor being tested, which can be pipelined vs. unpipelined, or can
    * involve a separate sensor class vs.just a factory object.
    *
    * @param checkpointerType The checkpointer type, e.g. hdf5 or java serialization.
    * @param filename The filename to use to save and restore the compute graph.
    * @param initialSteps How many steps the model is stepped before it is saved.
    * @param postRestoreSteps After the model is restored and checked, how many further steps before it's checked again.
    * @param colorsensorGenerator A function that makes the color sensor varietal being tested.
    */
  private def commonColorSensorComputeGraphTest(checkpointerType: CheckpointerType,
                                           filename: String,
                                           initialSteps: Int,
                                           postRestoreSteps: Int,
                                           colorsensorGenerator: () => Field): Unit = {

    val graph = new ComputeGraph {
      val A = colorsensorGenerator()
      val B = A + 1
      A.probe()
      B.probe()
    }
    val Rows = graph.A.rows
    val Cols = graph.A.columns

    /** The pixel value expected at a given row `r` and column `c`, shifted by a state-dependent `offset`. */
    def fieldVal(r: Int, c: Int, offset: Int) = {
      val red = 3 * (Cols*r + c) + offset
      val green = red + 1
      val blue = red + 2
      new Pixel(red, green, blue)
    }

    val suffixedFilename = checkpointerType.addSuffixIfAbsent(filename)
    removeFile(suffixedFilename)
    try {
      graph.withRelease {
        graph.step(initialSteps)
        graph.write(suffixedFilename, checkpointerType)
      }
      val graph2 = ComputeGraph.readFromFile(suffixedFilename, checkpointerType)
      graph2.withRelease {
        val actual = new Pixel
        for (r <- 0 until Rows; c <- 0 until Cols) {
          val expected = fieldVal(r, c, initialSteps)
          graph2.readByName("A").asInstanceOf[ColorFieldReader].read(r, c, actual)
          require(actual == expected, "Restored ComputeGraph has improper state.")
        }
        graph2.step(postRestoreSteps)
        for (r <- 0 until Rows; c <- 0 until Cols) {
          val expected = fieldVal(r, c, initialSteps + postRestoreSteps)
          graph2.readByName("A").asInstanceOf[ColorFieldReader].read(r, c, actual)
          require(actual == expected, "Restored ComputeGraph cannot step properly.")
        }
      }
    } finally
      removeFile(suffixedFilename)
  }

  /** A test of an unpipelined color sensor with no separate sensor class. */
  def simpleUnpipelinedColorSensorComputeGraphTest(checkpointerType: CheckpointerType): Unit = {
    val filename = "simpleUnpipelinedColorSensorTest"
    val InitialSteps = 5
    val PostRestoreSteps = 7
    val Rows = 11
    val Cols = 6

    commonColorSensorComputeGraphTest(checkpointerType,
      filename,
      InitialSteps,
      PostRestoreSteps,
      () => SimpleUnpipelinedTestColorSensor(Rows, Cols)
    )
  }

  /** A test of a pipelined color sensor with a separate sensor class. */
  def pipelinedColorSensorComputeGraphTest(checkpointerType: CheckpointerType): Unit = {
    val filename = "pipelinedColorSensorTest"
    val InitialSteps = 5
    val PostRestoreSteps = 7
    val Rows = 11
    val Cols = 6

    commonColorSensorComputeGraphTest(checkpointerType,
      filename,
      InitialSteps,
      PostRestoreSteps,
      () => PipelinedTestColorSensor(Rows, Cols)
    )
  }

  /** The common part of a number of tests involving vector sensors.  The parameters include a hook
    * to generate the particular vector sensor being tested, which can be pipelined vs. unpipelined, or can
    * involve a separate sensor class vs.just a factory object.
    *
    * @param checkpointerType The checkpointer type, e.g. hdf5 or java serialization.
    * @param filename The filename to use to save and restore the compute graph.
    * @param initialSteps How many steps the model is stepped before it is saved.
    * @param postRestoreSteps After the model is restored and checked, how many further steps before it's checked again.
    * @param sensorGenerator A function that makes the vector sensor varietal being tested.
    */
  private def commonVectorSensorComputeGraphTest(checkpointerType: CheckpointerType,
                                           filename: String,
                                           initialSteps: Int,
                                           postRestoreSteps: Int,
                                           sensorGenerator: () => Field): Unit = {

    val graph = new ComputeGraph {
      val A = sensorGenerator()
      val B = A + 1
      A.probe()
      B.probe()
    }
    val Rows = graph.A.rows
    val Cols = graph.A.columns
    val vectorLength = graph.A.tensorShape.points

    /** The pixel value expected at a given row `r` and column `c`, at the initial time point. */
    def fieldVal(r: Int, c: Int) = Vector(vectorLength, (i) => i*Rows*Cols + Cols*r + c)

    val suffixedFilename = checkpointerType.addSuffixIfAbsent(filename)
    removeFile(suffixedFilename)
    try {
      graph.withRelease {
        graph.step(initialSteps)
        graph.write(suffixedFilename, checkpointerType)
      }
      val graph2 = ComputeGraph.readFromFile(suffixedFilename, checkpointerType)
      val actual = new Vector(vectorLength)
      graph2.withRelease {
        for (r <- 0 until Rows; c <- 0 until Cols) {
          val expected = fieldVal(r, c) + initialSteps
          graph2.readByName("A").asInstanceOf[VectorFieldReader].read(r, c, actual)
          require(actual == expected, "Restored ComputeGraph has improper state.")
        }
        graph2.step(postRestoreSteps)
        for (r <- 0 until Rows; c <- 0 until Cols) {
          val expected = fieldVal(r, c) + initialSteps + postRestoreSteps
          graph2.readByName("A").asInstanceOf[VectorFieldReader].read(r, c, actual)
          require(actual == expected, "Restored ComputeGraph cannot step properly.")
        }
      }
    } finally
      removeFile(suffixedFilename)
  }

  /** A test of an unpipelined vector sensor with no separate sensor class. */
  def simpleUnpipelinedVectorSensorComputeGraphTest(checkpointerType: CheckpointerType): Unit = {
    val filename = "simpleUnpipelinedVectorSensorTest"
    val InitialSteps = 5
    val PostRestoreSteps = 7
    val fieldShape = Shape(16, 5)
    val tensorShape = Shape(4)

    commonVectorSensorComputeGraphTest(checkpointerType,
      filename,
      InitialSteps,
      PostRestoreSteps,
      () => SimpleUnpipelinedTestVectorSensor(fieldShape, tensorShape)
    )
  }

  /** A test of a pipelined vector sensor with a separate sensor class. */
  def pipelinedVectorSensorComputeGraphTest(checkpointerType: CheckpointerType): Unit = {
    val filename = "pipelinedVectorSensorTest"
    val InitialSteps = 3
    val PostRestoreSteps = 4
    val fieldShape = Shape(8, 9)
    val tensorShape = Shape(7)

    commonVectorSensorComputeGraphTest(checkpointerType,
      filename,
      InitialSteps,
      PostRestoreSteps,
      () => PipelinedTestVectorSensor(fieldShape, tensorShape)
    )
  }

  /** The common part of a number of tests involving actuators.  The parameters include a hook
    * to generate the particular actuator being tested, which can be pipelined vs. unpipelined, or can
    * involve a separate actuator class vs.just a factory object.
    *
    * @param checkpointerType The checkpointer type, e.g. hdf5 or java serialization.
    * @param filename The filename to use to save and restore the compute graph.
    * @param rows The number of rows in the actuated field.
    * @param cols The number of columns in the actuated field.
    * @param offset A number to add to every field value at the initial state
    * @param initialSteps How many steps the model is stepped before it is saved.
    * @param postRestoreSteps After the model is restored and checked, how many further steps before it's checked again.
    * @param actuatorGenerator A function that makes the actuator varietal being tested.
    */
  private def commonActuatorComputeGraphTest(checkpointerType: CheckpointerType,
                                            filename: String,
                                            rows: Int,
                                            cols: Int,
                                            offset: Int,
                                            initialSteps: Int,
                                            postRestoreSteps: Int,
                                            actuatorGenerator: (Field) => Unit,
                                            isUnpipelined: Boolean = true): Unit = {

    def fieldVal(r: Int, c: Int) = offset + cols*r + c

    val graph = new ComputeGraph {
      val A = ScalarField(rows, cols, (r,c) => fieldVal(r,c))
      A <== A + 1
      actuatorGenerator(A)
      A.probe()
    }
    val suffixedFilename = checkpointerType.addSuffixIfAbsent(filename)
    removeFile(suffixedFilename)
    try {
      // The checking performed by the test is built into the Actuator update function
      graph.withRelease {
        graph.step(initialSteps)
        graph.write(suffixedFilename, checkpointerType)
      }
      val graph2 = ComputeGraph.readFromFile(suffixedFilename, checkpointerType)
      graph2.withRelease {
        // We can look up the actuator by name if it's unpipelined
        if (isUnpipelined)
          graph2.getUnpipelinedActuator("A")
        graph2.step(postRestoreSteps)
        graph2.reset
      }
    } finally
      removeFile(suffixedFilename)
  }

  /** A test of an unpipelined sensor with a separate sensor class. */
  def unpipelinedActuatorComputeGraphTest(checkpointerType: CheckpointerType): Unit = {
    val filename = "unpipelinedActuatorTest"
    val InitialSteps = 1
    val PostRestoreSteps = 2
    val Rows = 10
    val Cols = 12

    commonActuatorComputeGraphTest(checkpointerType,
      filename,
      Rows, Cols,
      0,
      InitialSteps,
      PostRestoreSteps,
      (source: Field) => UnpipelinedTestActuator(source)
    )
  }

  /** A test of an unpipelined sensor with no separate sensor class, i.e. just the factory object. */
  def simpleUnpipelinedActuatorComputeGraphTest(checkpointerType: CheckpointerType): Unit = {
    val filename = "simpleUnpipelinedActuatorTest"
    val InitialSteps = 2
    val PostRestoreSteps = 3
    val Rows = 10
    val Cols = 12

    commonActuatorComputeGraphTest(checkpointerType,
      filename,
      Rows, Cols,
      0,
      InitialSteps,
      PostRestoreSteps,
      (source: Field) => SimpleUnpipelinedTestActuator(source)
    )
  }

  /** A test of an unpipelined sensor with a separate sensor class. */
  def unpipelinedActuatorWithArrayComputeGraphTest(checkpointerType: CheckpointerType): Unit = {
    val filename = "unpipelinedActuatorWithArrayTest"
    val InitialSteps = 2
    val PostRestoreSteps = 3
    val Rows = 10
    val Cols = 12

    // This uses a tweeked version of the commonActuatorComputeGraphTest
    // These assignments model the actual argument -> method parameter assignment process
    val initialSteps = InitialSteps
    val rows = Rows
    val cols = Cols
    val offset = 0
    val actuatorGenerator = (source: Field) => UnpipelinedTestActuatorWithArray(source)
    val isUnpipelined = true
    val postRestoreSteps = PostRestoreSteps

    def fieldVal(r: Int, c: Int) = offset + cols*r + c

    def checkData(state: Int, data: Array[Float]): Unit = {
      for (i <- 0 until data.length) {
        require(data(i) == state + i, "Data mismatch, expected " + (state + i) + " found " + data(i))
      }
    }

    val graph = new ComputeGraph {
      val A = ScalarField(rows, cols, (r,c) => fieldVal(r,c))
      A <== A + 1
      actuatorGenerator(A)
      A.probe("A")
    }
    val suffixedFilename = checkpointerType.addSuffixIfAbsent(filename)
    removeFile(suffixedFilename)
    try {
      // The checking performed by the test is built into the Actuator update function
      graph.withRelease {
        val actuator = graph.getUnpipelinedActuator("A").asInstanceOf[UnpipelinedTestActuatorWithArray]
        val data = actuator.data
        for (i <- 0 until initialSteps) {
          graph.step(1)
          val state = actuator.state
          checkData(state, data)
        }
        graph.write(suffixedFilename, checkpointerType)
      }
      val graph2 = ComputeGraph.readFromFile(suffixedFilename, checkpointerType)
      graph2.withRelease {
        val actuator2 = graph2.getUnpipelinedActuator("A").asInstanceOf[UnpipelinedTestActuatorWithArray]
        val data2 = actuator2.data
        for (i <- 0 until postRestoreSteps) {
          graph2.step(1)
          val state = actuator2.state
          checkData(state, data2)
        }
        graph2.reset
      }
    } finally
      removeFile(suffixedFilename)
  }

  /** A test of one approach to restoring actuators by setting up update() and reset() function bindings
    * to an actuator name, then restoring the compute graph.  A cleaner way that doesn't rely on singleton
    * objects is shown in another test.
    */
  def boundUnpipelinedActuatorComputeGraphTest(checkpointerType: CheckpointerType): Unit = {
    val filename = "boundUnpipelinedActuatorTest"
    val InitialSteps = 2
    val PostRestoreSteps = 3
    val Rows = 10
    val Cols = 12

    var state = 0
    def updateAndCheck(it: Iterator[Float]): Unit = {
      var expected = state
      while (it.hasNext) {
        val actual = it.next
        require(expected == actual, s"Data mismatch: expected $expected, saw $actual.")
        expected += 1
      }
      state += 1
    }
    def reset(): Unit = { state = 0 }

    def reset2(): Unit = { state = InitialSteps }

    BoundUnpipelinedTestActuator.addBinding("A", updateAndCheck, reset2)

    commonActuatorComputeGraphTest(checkpointerType,
      filename,
      Rows, Cols,
      0,
      InitialSteps,
      PostRestoreSteps,
      (source: Field) => BoundUnpipelinedTestActuator(source, updateAndCheck, reset)
    )
  }

  /** A test of an unpipelined sensor with a separate sensor class. */
  def pipelinedActuatorComputeGraphTest(checkpointerType: CheckpointerType): Unit = {
    val filename = "pipelinedActuatorTest"
    val InitialSteps = 1
    val PostRestoreSteps = 2
    val Rows = 10
    val Cols = 12

    commonActuatorComputeGraphTest(checkpointerType,
      filename,
      Rows, Cols,
      1,
      InitialSteps,
      PostRestoreSteps,
      (source: Field) => PipelinedTestActuator(source),
      isUnpipelined = false
    )
  }

  /** A test of an unpipelined sensor with a separate sensor class. */
  def simplePipelinedActuatorComputeGraphTest(checkpointerType: CheckpointerType): Unit = {
    val filename = "simplePipelinedActuatorTest"
    val InitialSteps = 3
    val PostRestoreSteps = 1
    val Rows = 5
    val Cols = 7

    commonActuatorComputeGraphTest(checkpointerType,
      filename,
      Rows, Cols,
      1,
      InitialSteps,
      PostRestoreSteps,
      (source: Field) => SimplePipelinedTestActuator(source),
      isUnpipelined = false
    )
  }

  /** The common part of a number of tests involving vector actuators.  The parameters include a hook
    * to generate the particular actuator being tested, which can be pipelined vs. unpipelined, or can
    * involve a separate sensor class vs.just a factory object.
    *
    * @param checkpointerType The checkpointer type, e.g. hdf5 or java serialization.
    * @param filename The filename to use to save and restore the compute graph.
    * @param rows The number of rows in the actuated field.
    * @param cols The number of columns in the actuated field.
    * @param vectorLength The tensor length of the actuated vector field.
    * @param offset A number to add to every field value at the initial state
    * @param initialSteps How many steps the model is stepped before it is saved.
    * @param postRestoreSteps After the model is restored and checked, how many further steps before it's checked again.
    * @param actuatorGenerator A function that makes the vector actuator varietal being tested.
    */
  private def commonVectorActuatorComputeGraphTest(checkpointerType: CheckpointerType,
                                            filename: String,
                                            rows: Int,
                                            cols: Int,
                                            vectorLength: Int,
                                            offset: Int,
                                            initialSteps: Int,
                                            postRestoreSteps: Int,
                                            actuatorGenerator: (Field) => Unit): Unit = {

    /** The pixel value expected at a given row `r` and column `c`, at the initial time point. */
    def fieldVal(r: Int, c: Int) = Vector(vectorLength, (i) => offset + i + vectorLength*(cols*r + c))

    val graph = new ComputeGraph {
      val A = VectorField(rows, cols, (r,c) => fieldVal(r,c))
      A <== A + 1
      actuatorGenerator(A)
      A.probe()
    }
    val suffixedFilename = checkpointerType.addSuffixIfAbsent(filename)
    removeFile(suffixedFilename)
    try {
      // The checking performed by the test is built into the Actuator update function
      graph.withRelease {
        graph.step(initialSteps)
        graph.write(suffixedFilename, checkpointerType)
      }
      val graph2 = ComputeGraph.readFromFile(suffixedFilename, checkpointerType)
      graph2.withRelease {
        graph2.step(postRestoreSteps)
        graph2.reset
      }
    } finally
      removeFile(suffixedFilename)
  }

  /** A test of an unpipelined vector actuator with a separate actuator class. */
  def unpipelinedVectorActuatorComputeGraphTest(checkpointerType: CheckpointerType): Unit = {
    val filename = "unpipelinedVectorActuatorTest"
    val InitialSteps = 2
    val PostRestoreSteps = 4
    val Rows = 10
    val Cols = 12
    val VectorLength = 3

    commonVectorActuatorComputeGraphTest(checkpointerType,
      filename,
      Rows, Cols, VectorLength,
      0,
      InitialSteps,
      PostRestoreSteps,
      (source: Field) => UnpipelinedTestVectorActuator(source)
    )
  }

  /** A test of a pipelined vector actuator with no separate actuator class, i.e. just the factory object. */
  def simplePipelinedVectorActuatorComputeGraphTest(checkpointerType: CheckpointerType): Unit = {
    val filename = "simplePipelinedVectorActuatorTest"
    val InitialSteps = 7
    val PostRestoreSteps = 2
    val Rows = 5
    val Cols = 6
    val VectorLength = 5

    commonVectorActuatorComputeGraphTest(checkpointerType,
      filename,
      Rows, Cols, VectorLength,
      1,
      InitialSteps,
      PostRestoreSteps,
      (source: Field) => SimplePipelinedTestVectorActuator(source)
    )
  }

  /** The common part of a number of tests involving color actuators.  The parameters include a hook
    * to generate the particular actuator being tested, which can be pipelined vs. unpipelined, or can
    * involve a separate actuator class vs.just a factory object.
    *
    * @param checkpointerType The checkpointer type, e.g. hdf5 or java serialization.
    * @param filename The filename to use to save and restore the compute graph.
    * @param rows The number of rows in the actuated field.
    * @param cols The number of columns in the actuated field.
    * @param offset A number to add to every field value at the initial state
    * @param initialSteps How many steps the model is stepped before it is saved.
    * @param postRestoreSteps After the model is restored and checked, how many further steps before it's checked again.
    * @param actuatorGenerator A function that makes the color actuator varietal being tested.
    */
  private def commonColorActuatorComputeGraphTest(checkpointerType: CheckpointerType,
                                                   filename: String,
                                                   rows: Int,
                                                   cols: Int,
                                                   offset: Int,
                                                   initialSteps: Int,
                                                   postRestoreSteps: Int,
                                                   actuatorGenerator: (Field) => Unit): Unit = {

    /** The pixel value expected at a given row `r` and column `c`, shifted by a state-dependent `offset`. */
    def fieldVal(r: Int, c: Int) = {
      val red = 3 * (cols*r + c) + offset
      val green = red + 1
      val blue = red + 2
      new Pixel(red, green, blue)
    }

    // The Cog API doesn't support arithmetic operations on ColorFields, so we have to do some push-ups.
    def incrementColorField(c: ColorField) = {
      val plusOne = toVectorField(ColorField(rows, cols, (r,c) => new Pixel(1, 1, 1)))
      toColorField((toVectorField(c) + plusOne))
    }

    val graph = new ComputeGraph {
      val A = ColorField(rows, cols, (r,c) => fieldVal(r,c))
      A <== incrementColorField(A)
      actuatorGenerator(A)
      probe(A)
    }
    val suffixedFilename = checkpointerType.addSuffixIfAbsent(filename)
    removeFile(suffixedFilename)
    try {
      // The checking performed by the test is built into the Actuator update function
      graph.withRelease {
        graph.step(initialSteps)
        graph.write(suffixedFilename, checkpointerType)
      }
      val graph2 = ComputeGraph.readFromFile(suffixedFilename, checkpointerType)
      graph2.withRelease {
        graph2.step(postRestoreSteps)
        graph2.reset
      }
    } finally
      removeFile(suffixedFilename)
  }

  /** A test of an unpipelined color actuator with a separate actuator class. */
  def unpipelinedColorActuatorComputeGraphTest(checkpointerType: CheckpointerType): Unit = {
    val filename = "unpipelinedColorActuatorTest"
    val InitialSteps = 2
    val PostRestoreSteps = 4
    // Should be less than 255/3 total field points for test to run
    val Rows = 4
    val Cols = 3

    commonColorActuatorComputeGraphTest(checkpointerType,
      filename,
      Rows, Cols,
      0,
      InitialSteps,
      PostRestoreSteps,
      (source: Field) => UnpipelinedTestColorActuator(source)
    )
  }

  /** A test of a pipelined color actuator with no separate actuator class, i.e. just the factory object. */
  def simplePipelinedColorActuatorComputeGraphTest(checkpointerType: CheckpointerType): Unit = {
    val filename = "simplePipelinedColorActuatorTest"
    val InitialSteps = 7
    val PostRestoreSteps = 2
    // Should be less than 255/3 total field points for test to run
    val Rows = 5
    val Cols = 6

    commonColorActuatorComputeGraphTest(checkpointerType,
      filename,
      Rows, Cols,
      1,
      InitialSteps,
      PostRestoreSteps,
      (source: Field) => SimplePipelinedTestColorActuator(source)
    )
  }
}