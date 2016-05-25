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
import org.scalatest.junit.JUnitRunner
import org.scalatest.FunSuite


/**
  * 
  * @author Matthew Pickett
  */
@RunWith(classOf[JUnitRunner])
class VectorActuatorSpec extends FunSuite {
  test("0D Vector Actuator"){
    new VectorActuatorProblem(0)
  }
  test("1D Vector Actuator"){
    new VectorActuatorProblem(1)
  }
  test("2D Vector Actuator"){
    new VectorActuatorProblem(2)
  }
  test("3D Vector Actuator"){
    new VectorActuatorProblem(3)
  }
}


//test that data read from a VectorFieldReader are consistent with data exported
//  by a VectorActuator
class VectorActuatorProblem(fieldDims:Int){
  //generate a random field shape given the specified dimensions
  val maxDimSize = 20
  val rng = new Random
  val fieldShape = Shape(Array.tabulate(fieldDims){
    (i) => rng.nextInt(maxDimSize) + 1
  })
  val tensorShape = Shape(rng.nextInt(maxDimSize) + 1)

  var actuatorData:Iterator[Float] = Iterator()
  //create a compute graph with a random field
  val cg = new ComputeGraph{
    val field = VectorField.random(fieldShape, tensorShape)
    val actuator = new VectorActuator(field, (x:Iterator[Float]) => {actuatorData = x} )
    probe(field, "")
  }
  import cg._
  withRelease {
    step

    //read the contents of the field
    val reader = read(field) match {
      case v: VectorFieldReader => v
      case _ => throw new RuntimeException
    }
    val vec = Vector(tensorShape(0), (i)=>0f)
    val readFromGraph = fieldShape.indices.flatMap(idx => {
      fieldShape.dimensions match {
        case 0 => reader.read(vec); vec.toArray
        case 1 => reader.read(idx(0), vec); vec.toArray
        case 2 => reader.read(idx(0), idx(1), vec); vec.toArray
        case 3 => reader.read(idx(0), idx(1), idx(2), vec); vec.toArray
      }
    }).toIterator

    val eq = actuatorData.zip(readFromGraph).map(x => x._1 == x._2).reduce(_&&_)
    require(eq)
  }
}
