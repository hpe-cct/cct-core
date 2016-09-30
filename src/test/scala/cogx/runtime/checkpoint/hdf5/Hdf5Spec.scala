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

import cogx.platform.types.Hdf5CheckpointerType
import hdfloader.Hdf5NativesLoader

import scala.language.reflectiveCalls
import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith
import org.scalatest.MustMatchers
import org.scalatest.FunSuite
import java.io.File

import cogx.api.ImplicitConversions
import cogx.runtime.checkpoint.hdf5.Hdf5Common
import ncsa.hdf.hdf5lib.H5._
import ncsa.hdf.hdf5lib.HDF5Constants._

/** Test code for HDF5.
  *
  * @author Dick Carter
  */

@RunWith(classOf[JUnitRunner])
class Hdf5Spec
        extends FunSuite
        with MustMatchers
        with ImplicitConversions
        with CheckpointTester
{
  test("native library loading") {
    // This will emit certain INFO and DEBUG messages that are silenced when invoked by the user.
    Hdf5NativesLoader.load()

    val filename = "testFile"
    val f = new File(filename)
    if (f.exists()) {
      f.delete()
      require(!f.exists(), s"Can't remove $filename prior to attempt to create it." )
    }
    // Create a null HDF5 file, then remove it.
    val fileId = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT)
    H5Fclose(fileId)
    require(f.exists() && f.canRead(), s"Expecting to find file $filename, not there.")
    f.delete()
  }

  test("save/restore primitives") {
    primitivesTest(Hdf5CheckpointerType)
  }

  test("save/restore objects") {
    objectsTest(Hdf5CheckpointerType)
  }

  test("save/restore/step ComputeGraph") {
    simpleComputeGraphTest(Hdf5CheckpointerType)
  }

  test("save/restore/step ComputeGraph with UnpipelinedTestSensor") {
    unpipelinedSensorComputeGraphTest(Hdf5CheckpointerType)
  }

  test("save/restore/step ComputeGraph with PipelinedTestSensor") {
    pipelinedSensorComputeGraphTest(Hdf5CheckpointerType)
  }

  test("save/restore/step ComputeGraph with SimpleUnpipelinedTestSensor") {
    simpleUnpipelinedSensorComputeGraphTest(Hdf5CheckpointerType)
  }

  test("save/restore/step ComputeGraph with SimplePipelinedSensor") {
    simplePipelinedSensorComputeGraphTest(Hdf5CheckpointerType)
  }

  test("save/restore/step ComputeGraph with SimpleUnpipelinedColorSensor") {
    simpleUnpipelinedColorSensorComputeGraphTest(Hdf5CheckpointerType)
  }

  test("save/restore/step ComputeGraph with PipelinedColorSensor") {
    pipelinedColorSensorComputeGraphTest(Hdf5CheckpointerType)
  }

  test("save/restore/step ComputeGraph with SimpleUnpipelinedVectorSensor") {
    simpleUnpipelinedVectorSensorComputeGraphTest(Hdf5CheckpointerType)
  }

  test("save/restore/step ComputeGraph with PipelinedVectorSensor") {
    pipelinedVectorSensorComputeGraphTest(Hdf5CheckpointerType)
  }

  test("save/restore/step ComputeGraph with UnpipelinedActuator") {
    unpipelinedActuatorComputeGraphTest(Hdf5CheckpointerType)
  }

  test("save/restore/step ComputeGraph with SimpleUnpipelinedActuator") {
    simpleUnpipelinedActuatorComputeGraphTest(Hdf5CheckpointerType)
  }

  test("save/restore/step ComputeGraph with BoundUnpipelinedActuator") {
    boundUnpipelinedActuatorComputeGraphTest(Hdf5CheckpointerType)
  }

  test("save/restore/step ComputeGraph with UnpipelinedActuatorWithArray") {
    unpipelinedActuatorWithArrayComputeGraphTest(Hdf5CheckpointerType)
  }

  test("save/restore/step ComputeGraph with PipelinedActuator") {
    pipelinedActuatorComputeGraphTest(Hdf5CheckpointerType)
  }

  test("save/restore/step ComputeGraph with SimplePipelinedActuator") {
    simplePipelinedActuatorComputeGraphTest(Hdf5CheckpointerType)
  }

  test("save/restore/step ComputeGraph with UnpipelinedVectorActuator") {
    unpipelinedVectorActuatorComputeGraphTest(Hdf5CheckpointerType)
  }

  test("save/restore/step ComputeGraph with SimplePipelinedVectorActuator") {
    simplePipelinedVectorActuatorComputeGraphTest(Hdf5CheckpointerType)
  }

  test("save/restore/step ComputeGraph with UnpipelinedColorActuator") {
    unpipelinedColorActuatorComputeGraphTest(Hdf5CheckpointerType)
  }

  test("save/restore/step ComputeGraph with SimplePipelinedColorActuator") {
    simplePipelinedColorActuatorComputeGraphTest(Hdf5CheckpointerType)
  }

}