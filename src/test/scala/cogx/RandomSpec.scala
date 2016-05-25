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
import scala.collection.mutable.ArrayBuffer

/** Unit tests for the Random operator. Note that only functionality for 1 bit
  * of precision is enabled.
  *
  * @author Matthew Pickett
  */
@RunWith(classOf[JUnitRunner])
class RandomSpec extends FunSuite {

  val maxDimSize = 20
  val minPoints = 64

  def gen(bits:Int, numRandoms:Int) = {
    val rng = new java.util.Random
    val fieldDims = rng.nextInt(4)

    val fieldShape = Shape(Array.tabulate(fieldDims){
      (i) => rng.nextInt(maxDimSize) + 1
    })

    val tensorPoints = rng.nextInt(maxDimSize) + minPoints/fieldShape.points + 1
    val tensorShape = Shape(tensorPoints)
    val points = fieldShape.points*tensorShape.points

    val graph = new ComputeGraph{
      val rand = VectorField.random(fieldShape,tensorShape)
      rand <== random(rand, bits)
      val flat = reshape(rand, Shape(), Shape(points))
      probe(flat, "")
    }

    val buff = new ArrayBuffer[Float]()
    val vec = Vector(points, (i)=>0f)
    val steps = numRandoms/points + 1
    println(s"FieldShape: $fieldShape")
    println(s"TensorShape: $tensorShape")
    import graph._
    withRelease {
      step
      for(i <- 0 until steps){
        step
        val reader = read(flat).asInstanceOf[VectorFieldReader]
        reader.read(vec)
        buff ++= vec.getData
      }
    }
    buff.toVector
  }

  //ensure that the values of the output are consistent with the number of bits
  // i.e. (0f, 1f) for 1 bit.  (0f, 0.333f, 0.666f, 1f) for 2 bits.
  def valuesTest(bits:Int, numValues:Int):Boolean = {
    val values = gen(bits, numValues)
    val possibleVals =math.pow(2,bits).toInt
    val step = 1f/(possibleVals.toFloat-1f)
    val okVals = IndexedSeq.tabulate(possibleVals){(i)=>i*step}
    val remaining = values.filterNot((x)=> okVals.contains(x))
    remaining.length == 0
  }

  test("1 bit values") { require(valuesTest(1, 100000)) }
  /*test("2 bit values") { require(valuesTest(2, 100000)) }
  test("3 bit values") { require(valuesTest(3, 100000)) }
  test("4 bit values") { require(valuesTest(4, 100000)) }
  test("5 bit values") { require(valuesTest(5, 100000)) }
  test("6 bit values") { require(valuesTest(6, 100000)) }
  test("7 bit values") { require(valuesTest(7, 100000)) }
  test("8 bit values") { require(valuesTest(8, 100000)) }*/

  def frequencyTest(bits:Int, numValues:Int) = {
    val values = gen(bits, numValues)
    val points = values.length
    val possibleVals =math.pow(2,bits).toInt
    val step = 1f/(possibleVals.toFloat-1f)
    val bins = Array.tabulate(possibleVals){(i)=>0}
    val okVals = IndexedSeq.tabulate(possibleVals){(i)=>i*step}
    values.foreach(x=> bins(okVals.indexOf(x))+=1)
    val freq = bins.map(_.toFloat/points)
    val total = freq.reduce(_+_)
    val max = freq.reduce((x,y) =>math.max(x,y))
    val min = freq.reduce((x,y) =>math.min(x,y))
    bins.foreach{println}
    println(s"Max: $max")
    println(s"Min: $min")
    println(s"Total: $total")
    val maxMinDiff = (max - min)/min
    require(maxMinDiff < 0.3f,
      "Percent difference between max and min frequency should be less than 30%")
  }

//  test("1 bit freq") { frequencyTest(1, 100000) }
/*  test("2 bit freq") { frequencyTest(2, 100000) }
  test("3 bit freq") { frequencyTest(3, 100000) }
  test("4 bit freq") { frequencyTest(4, 100000) }
  test("5 bit freq") { frequencyTest(5, 100000) }
  test("6 bit freq") { frequencyTest(6, 1000000) }
  test("7 bit freq") { frequencyTest(7, 1000000) }
  test("8 bit freq") { frequencyTest(8, 1000000) }*/

}
