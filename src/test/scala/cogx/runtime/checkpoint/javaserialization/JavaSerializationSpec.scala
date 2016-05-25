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

import cogx.platform.types.JavaCheckpointerType

import scala.language.reflectiveCalls
import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith
import org.scalatest.MustMatchers
import org.scalatest.FunSuite

import cogx.api.ImplicitConversions

/** Test code for Java Serializer- created to prove that save/restore is implementation independent.
  *
  * @author Dick Carter
  */

@RunWith(classOf[JUnitRunner])
class JavaSerializationSpec
        extends FunSuite
        with MustMatchers
        with ImplicitConversions
        with CheckpointTester
{
  test("save/restore primitives") {
    primitivesTest(JavaCheckpointerType)
  }

  test("save/restore objects") {
    objectsTest(JavaCheckpointerType)
  }

  test("save/restore/step ComputeGraph") {
    simpleComputeGraphTest(JavaCheckpointerType)
  }
  test("save/restore/step ComputeGraph with UnpipelinedTestSensor") {
    unpipelinedSensorComputeGraphTest(JavaCheckpointerType)
  }

  test("save/restore/step ComputeGraph with PipelinedTestSensor") {
    pipelinedSensorComputeGraphTest(JavaCheckpointerType)
  }

  test("save/restore/step ComputeGraph with SimpleUnpipelinedTestSensor") {
    simpleUnpipelinedSensorComputeGraphTest(JavaCheckpointerType)
  }

  test("save/restore/step ComputeGraph with SimplePipelinedSensor") {
    simplePipelinedSensorComputeGraphTest(JavaCheckpointerType)
  }

  test("save/restore/step ComputeGraph with SimpleUnpipelinedColorSensor") {
    simpleUnpipelinedColorSensorComputeGraphTest(JavaCheckpointerType)
  }

  test("save/restore/step ComputeGraph with PipelinedColorSensor") {
    pipelinedColorSensorComputeGraphTest(JavaCheckpointerType)
  }

  test("save/restore/step ComputeGraph with SimpleUnpipelinedVectorSensor") {
    simpleUnpipelinedVectorSensorComputeGraphTest(JavaCheckpointerType)
  }

  test("save/restore/step ComputeGraph with PipelinedVectorSensor") {
    pipelinedVectorSensorComputeGraphTest(JavaCheckpointerType)
  }

  test("save/restore/step ComputeGraph with UnpipelinedActuator") {
    unpipelinedActuatorComputeGraphTest(JavaCheckpointerType)
  }

  test("save/restore/step ComputeGraph with SimpleUnpipelinedActuator") {
    simpleUnpipelinedActuatorComputeGraphTest(JavaCheckpointerType)
  }

  test("save/restore/step ComputeGraph with PipelinedActuator") {
    pipelinedActuatorComputeGraphTest(JavaCheckpointerType)
  }

  test("save/restore/step ComputeGraph with SimplePipelinedActuator") {
    simplePipelinedActuatorComputeGraphTest(JavaCheckpointerType)
  }

  test("save/restore/step ComputeGraph with UnpipelinedVectorActuator") {
    unpipelinedVectorActuatorComputeGraphTest(JavaCheckpointerType)
  }

  test("save/restore/step ComputeGraph with SimplePipelinedVectorActuator") {
    simplePipelinedVectorActuatorComputeGraphTest(JavaCheckpointerType)
  }

  test("save/restore/step ComputeGraph with UnpipelinedColorActuator") {
    unpipelinedColorActuatorComputeGraphTest(JavaCheckpointerType)
  }

  test("save/restore/step ComputeGraph with SimplePipelinedColorActuator") {
    simplePipelinedColorActuatorComputeGraphTest(JavaCheckpointerType)
  }
}