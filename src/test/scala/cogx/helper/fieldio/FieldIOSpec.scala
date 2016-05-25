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

package cogx.helper.fieldio

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.{BeforeAndAfterEach, FunSuite}
import org.scalatest.MustMatchers
import java.io.File
import cogx.cogmath.geometry.Shape
import cogx.reference.{RefVectorField, RefScalarField}

/**
  * @author Matthew Pickett
  */

@RunWith(classOf[JUnitRunner])
class FieldIOSpec extends FunSuite with MustMatchers with FieldIO
                          with BeforeAndAfterEach {

  private val fileName = "temp.fld"
  private var file: File = _

  override def beforeEach() {file = new File(fileName)}
  override def afterEach() {file.delete()}

  test("save to and load scalar field from disk"){
    val testField = RefScalarField.random(100,100)
    writeScalarFieldToFile(testField,file)
    val loadedField = readScalarFieldFromFile(file)
    testField must equal (loadedField)
  }

  test("save to and load vector field from disk"){
    val testField = RefVectorField.random(100,100,Shape(30))
    writeVectorFieldToFile(testField,file)
    val loadedField = readVectorFieldFromFile(file)
    testField must equal (loadedField)
  }

}