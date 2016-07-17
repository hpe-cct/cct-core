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
import api.ImplicitConversions
import cogx.helper.{ColorFieldBuilderInterface, ScalarFieldBuilderInterface}
import cogx.reference.RefTestInterface

import scala.concurrent.duration._
import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global

/** Test code for user-defined GPU operators
  */

@RunWith(classOf[JUnitRunner])
class GPUOperatorSpec
        extends FunSuite
        with MustMatchers
        with ImplicitConversions
        with ScalarFieldBuilderInterface
        with ColorFieldBuilderInterface
{
  val Optimize = true

  /** big / small tensor: average two images */
  private def average(f1: Field, f2: Field): Field = {
    // Semantic checking
    require(f1.fieldType == f2.fieldType, "Fields must have the same type")
    GPUOperator(f1.fieldType) {
      if (f1.tensorShape.points > 4) {
        _forEachTensorElement(f1.tensorShape) {
          val x1 = _readTensorElement(f1, _tensorElement)
          val x2 = _readTensorElement(f2, _tensorElement)
          _writeTensorElement(_out0, (x1 + x2) / 2f, _tensorElement)
        }
      } else {
        val x1 = _readTensor(f1)
        val x2 = _readTensor(f2)
        _writeTensor(_out0, (x1 + x2) / 2.0f)
      }
    }
  }

  /** small tensor: read image, write it out transposed. */
  def userTranspose(f1: Field): Field = {
    GPUOperator(f1.fieldType) {
      if (f1.tensorShape.points > 4) {
        _forEachTensorElement(f1.tensorShape) {
          val element = _readTensorElement(f1, _tensorElement)
          _writeTensorElement(_out0, element, _column, _row, _tensorElement)
        }
      } else {
        val pixel = _readTensor(f1)
        _writeTensor(_out0, pixel, _column, _row)
      }
    }
  }

  /** small tensor: triple tensor in field. */
  def triple(f: Field) = GPUOperator(f.fieldType){
    _writeTensor(_out0, _readTensor(f) * 3f)
  }

  /** small tensor: add two fields. */
  def add(f2D: Field, f0D: Field) = GPUOperator(f2D.fieldType){
    _writeTensor(_out0, _readTensor(f2D) +_readTensor(f0D))
  }

  /** small tensor: multiply field by -1 */
  def negate(f: Field): Field = {
    GPUOperator(f.fieldType) {
      val x = _tensorVar(f)
      x := _readTensor(f) * -1f
      _writeTensor(_out0, x)
    }
  }

  /** small tensor: Copy through a 1D, 2D, 3D array, result should be 3 * input */
  def copyThroughArray(f: Field) = GPUOperator(f.fieldType) {
    val array1D = _tensorArray(f, 3)
    val array2D = _tensorArray(f, 3, 5)
    val array3D = _tensorArray(f, 2, 3, 5)
    array1D(1) := _readTensor(f)
    array2D(1, 1) := _readTensor(f)
    array3D(1, 1, 1) := _readTensor(f)
    val sum = _tensorVar(f)
    sum := array1D(1) + array2D(1, 1) + array3D(1, 1, 1)
    _writeTensor(_out0, sum)
  }

  /** small tensor: absolute value of every element in a field. Shows use of GPUOperator naming. */
  def absValue(f: Field): Field = GPUOperator(f.fieldType, "absValue") {
    val x = _tensorVar(f)
    x := _readTensor(f)
    _if (x < 0f) {
      x := -x
    }
    _writeTensor(_out0, x)
  }

  /** big tensor: halve each element in a tensor field. */
  def half(f: Field): Field = {
    GPUOperator(f.fieldType) {
      _forEachTensorElement(f.tensorShape) {
        val element = _tensorElementVar(f)
        element := _readTensorElement(f, _tensorElement)
        element /= 2.0f
        _writeTensorElement(_out0, element, _tensorElement)
      }
    }
  }

  /** big / small tensor: copy input to output */
  def copy(f: Field): Field = {
    GPUOperator(f.fieldType) {
      if (f.tensorShape.points > 4) {
        _forEachTensorElement(f.tensorShape) {
          val element = _tensorElementVar(f)
          element := _readTensorElement(f, _tensorElement)
          _writeTensorElement(_out0, element, _tensorElement)
        }
      } else {
        _writeTensor(_out0, _readTensor(f))
      }
    }
  }

  /** flip the ordering of tensor elements.  Not necessarily the best way,
    * but stresses the code-generation under merging of _readTensorElement().
    * Operates in SmallTensorAddressing or BigTensorAddressing mode. */
  private def flipTensor(f: Field): Field = {
    // Semantic checking
    GPUOperator(f.fieldType) {
      if (f.tensorShape.dimensions == 0)
        _writeTensor(_out0, _readTensor(f))
      else (f.tensorShape.points) match {
        case 1 => _writeTensor(_out0, _readTensor(f))
        case 2 =>
          _writeTensor(_out0,
            _float2(_readTensorElement(f, 1),
                    _readTensorElement(f, 0)))
        case 3 =>
          _writeTensor(_out0,
            _float3(_readTensorElement(f, 2),
                    _readTensorElement(f, 1),
                    _readTensorElement(f, 0)))
        case 4 =>
          _writeTensor(_out0,
            _float4(_readTensorElement(f, 3),
              _readTensorElement(f, 2),
              _readTensorElement(f, 1),
              _readTensorElement(f, 0)))
        case _ =>
        _forEachTensorElement(f.tensorShape) {
          val x = _readTensorElement(f, f.tensorShape.points - 1 - _tensorElement)
          _writeTensorElement(_out0, x, _tensorElement)
        }
      }
    }
  }

  /** flip the ordering of tensor elements.  Operates in TensorElementAddressing mode with a nonlocal read and
    * a local write. */
  private def flipTensor2(f: Field): Field = {
    // Semantic checking
    GPUOperator(f.fieldType) {
      _globalThreads(f.fieldShape, f.tensorShape)
      val x = _readTensorElement(f, f.tensorShape.points - 1 - _tensorElement)
      _writeTensorElement(_out0, x, _tensorElement)
    }
  }


  /** flip the ordering of tensor elements.  Operates in TensorElementAddressing mode with a local read and
    * a non-local write. */
  private def flipTensor3(f: Field): Field = {
    // Semantic checking
    GPUOperator(f.fieldType) {
      _globalThreads(f.fieldShape, f.tensorShape)
      val x = _readTensorElement(f, _tensorElement)
      _writeTensorElement(_out0, x, f.tensorShape.points - 1 - _tensorElement)
    }
  }

  /** Double the value of all tensor elements.  Operates in TensorElementAddressing mode with a local read and
    * a local write. */
  private def times2(f: Field): Field = {
    // Semantic checking
    GPUOperator(f.fieldType) {
      _globalThreads(f.fieldShape, f.tensorShape)
      val x = _readTensorElement(f, _tensorElement)
      _writeTensorElement(_out0, x * 2f, _tensorElement)
    }
  }

  /** small tensor: multiply field by -1 */
  def addConst(f: Field, aFloat: Float): Field = {
    GPUOperator(f.fieldType) {
      val x = _tensorVar(f)
      x := _readTensor(f) + aFloat
      _writeTensor(_out0, x)
    }
  }
  //-------------------------------- TESTS -------------------------------------

  test("_writeTensor local small") {
    val graph = new ComputeGraph(Optimize) with RefTestInterface {
      val s1 = ScalarField.random(5, 5)
      val s2 = ScalarField.random(5, 5)
      val v1 = VectorField.random(Shape(5, 5), Shape(3))
      val v2 = VectorField.random(Shape(5, 5), Shape(3))
      val m1 = MatrixField.random(Shape(5, 5), Shape(2, 2))
      val m2 = MatrixField.random(Shape(5, 5), Shape(2, 2))
      val c = ColorField.random(5, 5)
      val sAvg = average(s1, s2)
      val vAvg = average(v1, v2)
      val mAvg = average(m1, m2)
      val cCopy = copy(c)

      probe(sAvg, s1, s2, vAvg, v1, v2, mAvg, m1, m2, c, cCopy)
    }
    import graph._
    withRelease {
      step
      require(readScalar(sAvg) == (readScalar(s1) + readScalar(s2)) / 2)
      require(readVector(vAvg) == (readVector(v1) + readVector(v2)) / 2)
      require(readMatrix(mAvg) == (readMatrix(m1) + readMatrix(m2)) / 2)
      require(readColor(c) == readColor(cCopy))
    }
  }

  test("_writeTensorElement local big") {
    val graph = new ComputeGraph(Optimize) with RefTestInterface {
      val s1 = ScalarField.random(5, 5)
      val s2 = ScalarField.random(5, 5)
      val v1 = VectorField.random(Shape(5, 5), Shape(11))
      val v2 = VectorField.random(Shape(5, 5), Shape(11))
      val m1 = MatrixField.random(Shape(5, 5), Shape(7, 7))
      val m2 = MatrixField.random(Shape(5, 5), Shape(7, 7))
      val c = ColorField.random(5, 5)
      val sAvg = average(s1, s2)
      val vAvg = average(v1, v2)
      val mAvg = average(m1, m2)
      val cCopy = copy(c)

      probe(sAvg, s1, s2, vAvg, v1, v2, mAvg, m1, m2, c, cCopy)
    }
    import graph._
    withRelease {
      step
      require(readScalar(sAvg) == (readScalar(s1) + readScalar(s2)) / 2f)
      require(readVector(vAvg) == (readVector(v1) + readVector(v2)) / 2f)
      require(readMatrix(mAvg) == (readMatrix(m1) + readMatrix(m2)) / 2f)
      require(readColor(c) == readColor(cCopy))
    }
  }

  test("_writeTensor nonlocal small") {
     val graph = new ComputeGraph(Optimize) with RefTestInterface {
      val s = ScalarField.random(5, 5)
      val sUserTransposed = userTranspose(s)
      val sSystemTransposed = transpose(s)

      // Since only the userTranspose kernel does a non-local write, this
      // merges down to one kernel- a good test for the merger
      val v = VectorField.random(Shape(5, 5), Shape(3))
      val vSystemTransposed = transpose(v)
      val vUserTransposed = userTranspose(v)

      val m = MatrixField.random(Shape(5, 5), Shape(2, 2))
      val mUserTransposed = userTranspose(m)
      val mSystemTransposed = transpose(m)

      val c = ColorField.random(5, 5)
      val cUserTransposed = userTranspose(c)
      val cSystemTransposed = transpose(c)

      probe(sUserTransposed, sSystemTransposed, vUserTransposed, vSystemTransposed,
        mUserTransposed, mSystemTransposed, cUserTransposed, cSystemTransposed)
    }
    import graph._
    withRelease {
      step
      require(readScalar(sUserTransposed) == readScalar(sSystemTransposed))
      require(readVector(vUserTransposed) == readVector(vSystemTransposed))
      require(readMatrix(mUserTransposed) == readMatrix(mSystemTransposed))
//      println("original")
//      readColor(c).print
//      println("user")
//      readColor(cUserTransposed).print
//      println("system")
//      readColor(cSystemTransposed).print
      require(readColor(cUserTransposed) == readColor(cSystemTransposed))
    }
  }

  test("_writeTensorElement nonlocal big") {
     val graph = new ComputeGraph(Optimize) with RefTestInterface {
      val s = ScalarField.random(5, 5)
      val sTransposed = userTranspose(s)
      val sDoubleTransposed = userTranspose(sTransposed)

      val v = VectorField.random(Shape(5, 5), Shape(9))
      val vTransposed = userTranspose(v)
      val vDoubleTransposed = userTranspose(vTransposed)

      val m = MatrixField.random(Shape(5, 5), Shape(11, 11))
      val mTransposed = userTranspose(m)
      val mDoubleTransposed = userTranspose(mTransposed)

      probe(s, sDoubleTransposed, v, vDoubleTransposed, m, mDoubleTransposed)
    }
    import graph._
    withRelease {
      step
      require(readScalar(s) == readScalar(sDoubleTransposed))
      require(readVector(v) == readVector(vDoubleTransposed))
      require(readMatrix(m) == readMatrix(mDoubleTransposed))
      println("original")
    }
  }

  test("_readTensor") {
    val graph = new ComputeGraph(Optimize) with RefTestInterface {
      val field0D = ScalarField(5f)
      val field1D = ScalarField(5, (col) => 2 * col)
      val field2D = ScalarField(5, 7, (row, col) => 3 * row + 2 * col)
      val field3D = ScalarField(5, 7, 3,
        (layer, row, col) => 5 * layer + 3 * row + 2 * col)

      val triple0D = triple(field0D)
      val triple1D = triple(field1D)
      val triple2D = triple(field2D)
      val triple3D = triple(field3D)

      probe(triple0D, field0D, triple1D, field1D, triple2D, field2D, triple3D, field3D)
    }
    import graph._
    withRelease {
      step
      require(readScalar(triple0D) == readScalar(field0D) * 3)
      require(readScalar(triple1D) == readScalar(field1D) * 3)
      require(readScalar(triple2D) == readScalar(field2D) * 3)
      require(readScalar(triple3D) == readScalar(field3D) * 3)
    }
  }

  test("_readTensor 0D, ND") {
    val Scalar0D = 5f
    val Vector0D = new Vector(3f, 7f)
    val graph = new ComputeGraph(Optimize) with RefTestInterface {
      // Check that _readTensor of 0D field always returns the
      // single tensor in the field regardless of output field shape.
       val scalar0D = ScalarField(Scalar0D)
      val scalar2D = ScalarField(5, 7, (row, col) => 3 * row + 2 * col)
      val scalarSum2D = add(scalar2D, scalar0D)

      val vector0D = VectorField(Vector0D)
      val vector2D = VectorField(5, 7, (row, col) => new Vector(row, col))
      val vectorSum2D = add(vector2D, vector0D)

      probe(scalarSum2D, scalar2D, vectorSum2D, vector2D)
    }
    import graph._
    withRelease {
      step
      require(readScalar(scalarSum2D) == readScalar(scalar2D) + Scalar0D)
      require(readVector(vectorSum2D) == readVector(vector2D) + Vector0D)
    }
  }

  test("_tensorVar :=") {
    val graph = new ComputeGraph(Optimize) with RefTestInterface {
      val in1 = ScalarField.random(5, 5)
      val in2 = VectorField.random(Shape(5, 7), Shape(3))
      val negate1 = negate(in1)
      val negate2 = negate(in2)

      probe(in1, in2, negate1, negate2)
    }
    import graph._
    withRelease {
      step
      require(readScalar(negate1) == (readScalar(in1) * -1))
      require(readVector(negate2) == (readVector(in2) * -1))
    }
  }

  test("_tensorArray: declare, read, write") {
    val graph = new ComputeGraph(Optimize) with RefTestInterface {
      val scalar0D = ScalarField(5f)
      val scalar1D = ScalarField.random(3)
      val scalar2D = ScalarField.random(3, 5)
      val scalar3D = ScalarField.random(3, 5, 7)

      val tripleScalar0D = copyThroughArray(scalar0D)
      val tripleScalar1D = copyThroughArray(scalar1D)
      val tripleScalar2D = copyThroughArray(scalar2D)
      val tripleScalar3D = copyThroughArray(scalar3D)

      val vector0D = VectorField(new Vector(3, 5))
      val vector1D = VectorField.random(Shape(3), Shape(2))
      val vector2D = VectorField.random(Shape(3, 5), Shape(2))
      val vector3D = VectorField.random(Shape(3, 5, 7), Shape(2))

      val tripleVector0D = copyThroughArray(vector0D)
      val tripleVector1D = copyThroughArray(vector1D)
      val tripleVector2D = copyThroughArray(vector2D)
      val tripleVector3D = copyThroughArray(vector3D)

      probe(tripleScalar0D, scalar0D, tripleScalar1D, scalar1D, tripleScalar2D, scalar2D, tripleScalar3D, scalar3D)
      probe(tripleVector0D, vector0D, tripleVector1D, vector1D, tripleVector2D, vector2D, tripleVector3D, vector3D)
    }
    import graph._
    withRelease {
      step

      require(readScalar(tripleScalar0D) == readScalar(scalar0D) * 3)
      require(readScalar(tripleScalar1D) == readScalar(scalar1D) * 3)
      require(readScalar(tripleScalar2D) == readScalar(scalar2D) * 3)
      require(readScalar(tripleScalar3D) == readScalar(scalar3D) * 3)

      require(readVector(tripleVector0D) == readVector(vector0D) * 3)
      require(readVector(tripleVector1D) == readVector(vector1D) * 3)
      require(readVector(tripleVector2D) == readVector(vector2D) * 3)
      require(readVector(tripleVector3D) == readVector(vector3D) * 3)
    }
  }
  /** Test _if (used by absValue kernel), as well as merging of GPUOperator kernels */
  test("_if") {
    val graph = new ComputeGraph(Optimize) with RefTestInterface {
      val scalar2DUnshifted = ScalarField.random(3, 5)
      val scalar2D = scalar2DUnshifted - 0.5f
      val absScalar2D = absValue(scalar2D)

      probe(absScalar2D, scalar2DUnshifted)
    }
    import graph._
    withRelease {
      step
      require(readScalar(absScalar2D) ==
              readScalar(scalar2DUnshifted).map(_ - 0.5f).map(x => x.abs))
    }
  }

  test("_else") {
    // Absolute value of every element in a field, doubled.
    def absValueDoubled(f: Field): Field = GPUOperator(f.fieldType) {
      val x = _tensorVar(f)
      x := _readTensor(f)
      _if (x < 0f) {
        x := -x * 2f
      }
      _else {
        x := x * 2f
      }
      _writeTensor(_out0, x)
    }
    val graph = new ComputeGraph(Optimize) with RefTestInterface {
      val scalar2DUnshifted = ScalarField.random(3, 5)
      val scalar2D = scalar2DUnshifted - 0.5f
      val absScalar2D = absValueDoubled(scalar2D)

      probe(absScalar2D, scalar2DUnshifted)
    }
    import graph._
    withRelease {
      step
      require(readScalar(absScalar2D) ==
              readScalar(scalar2DUnshifted).map(_ - 0.5f).map(x => x.abs * 2))
    }
  }

  test("_elseif") {
    // Clip values to [-0.1, 0.1]
    def clip(f: Field): Field = GPUOperator(f.fieldType) {
      val x = _tensorVar(f)
      x := _readTensor(f)
      _if (x < -0.1f) {
        x := -0.1f
      }
      _elseif (x > 0.1f) {
        x := 0.1f
      }
      _elseif (_isequal(x, 0f)) {
        x := 0f
      }
      _else {
        x := x
      }
      _writeTensor(_out0, x)
    }
    val graph = new ComputeGraph(Optimize) with RefTestInterface {
      val scalar2DUnshifted = ScalarField.random(3, 5)
      val scalar2D = scalar2DUnshifted - 0.5f
      val absScalar2D = clip(scalar2D)

      probe(absScalar2D, scalar2DUnshifted)
    }
    import graph._
    withRelease {
      step
      require(readScalar(absScalar2D) == readScalar(scalar2DUnshifted).map(_ - 0.5f).map(
        x => if (x < -0.1f) -0.1f else if (x > 0.1f) 0.1f else x))
    }
  }

  test("_while") {
    // Increment values until they exceed 10
    def bumpUp(f: Field): Field = GPUOperator(f.fieldType) {
      val x = _tensorVar(f)
      x := _readTensor(f)
      _while (x <= 10f) {
        x := x + 0.001f
      }
      _writeTensor(_out0, x)
    }
    val graph = new ComputeGraph(Optimize) with RefTestInterface {
      val scalar2D = ScalarField.random(3, 5) - 0.5f
      val bumpedScalar2D = bumpUp(scalar2D)

      probe(bumpedScalar2D)
    }
    import graph._
    withRelease {
      step
      readScalar(bumpedScalar2D).map(
        x => {require(x >= 10f); 1f}
      )
    }
  }

  test("_for") {
    // Take each element to fourth power using for loop
    def pow4(f: Field): Field = GPUOperator(f.fieldType) {
      val x = _tensorVar(f)
      x := _readTensor(f)
      val i = _intVar()
      _for(i := 0, i < 2, i += 1) {
        x *= x
      }
      _writeTensor(_out0, x)
    }
    val graph = new ComputeGraph(Optimize) with RefTestInterface {
      val scalar2D = ScalarField.random(3, 5) - 0.5f
      val pow4Scalar2D = pow4(scalar2D)

      probe(pow4Scalar2D, scalar2D)
    }
    import graph._
    withRelease {
      step
      require(readScalar(pow4Scalar2D) ~== readScalar(scalar2D).map(
        x => x * x * x * x)
      )
    }
  }

  test("_local_tensorArray: sharing") {
    // Read a tile of the input field into shared local memory using
    // multiple threads
    def tile(field: Field) = {
      val _memory = _local(_tensorArray(field, _localRows, _localColumns))
      val _row = _intVar()
      val _column = _intVar()
      _for(_row := 0, _row < _localRows, _row := _row + 1) {
        val _readRow = _intVar()
        _readRow := _groupRow * _localRows + _row
        _for(_column := 0, _column < _localColumns, _column := _column + 1) {
          val _readColumn = _intVar()
          _readColumn := _groupColumn * _localColumns + _column
          _if((_readRow < field.rows) && (_readColumn < field.columns) &&
                  (_readRow >= 0) && (_readColumn >= 0)) {
            _memory(_row, _column) := _readTensor(field, _readRow, _readColumn)
          }
        }

      }
      _syncThreadsGlobal
      _memory
    }

    // Read a field through a tile, then write it out. Output should equal
    // input, this merely checks local memory functionality.
    def readThroughTiles(field: Field) =
      GPUOperator(field.fieldType) {
        val inputTile = tile(field)
        val localRow = _intVar()
        localRow := _row - _groupRow * _localRows
        val localColumn = _intVar()
        localColumn := _column - _groupColumn * _localColumns
        _writeTensor(_out0, inputTile(localRow, localColumn))
      }

    val graph = new ComputeGraph(Optimize) with RefTestInterface {
      val scalar2D = ScalarField.random(256, 256)
      val outScalar2D = readThroughTiles(scalar2D)
      val vector2D = VectorField.random(Shape(256, 256), Shape(4))
      val outVector2D = readThroughTiles(vector2D)

      probe(outScalar2D, scalar2D, outVector2D, vector2D)
    }
    import graph._
    withRelease {
      step
      require(readScalar(outScalar2D) == readScalar(scalar2D))
      require(readVector(outVector2D) == readVector(vector2D))
    }
  }

  test("vector literal / vector addressing") {
    // Compress 4 scalar fields to a vector field
    def toTensor(f1: Field, f2: Field, f3: Field, f4: Field): Field =
      GPUOperator(new FieldType(f1.fieldShape, Shape(4), Float32)) {
        val x = _readTensor(f1)
        val y = _readTensor(f2)
        val z = _readTensor(f3)
        val w = _readTensor(f4)
        val pixel = _float4(x, y, z, w)
        _writeTensor(_out0, pixel)
      }

    // identity transform, scrambling then unscrambling internally
    def scrambleDescramble(f: Field): Field =
      GPUOperator(f.fieldType) {
        val tensor = _readTensor(f)
        val xx = tensor.xx
        val xy = tensor.xy
        val xz = tensor.xz
        val xw = tensor.xw
        val zxw = tensor.zxw
        val yyz = tensor.yyz
        val wwx = tensor.wwx
        val x = zxw.y
        val y = yyz.x
        val z = xz.y
        val w = zxw.z
        _writeTensor(_out0, _float4(x, y, z, w))
      }

    def channel(f: Field, channel: Int): Field =
      GPUOperator(new FieldType(f.fieldShape, Shape(), Float32)) {
        channel match {
          case 0 => _writeTensor(_out0, _readTensor(f).x)
          case 1 => _writeTensor(_out0, _readTensor(f).y)
          case 2 => _writeTensor(_out0, _readTensor(f).z)
          case 3 => _writeTensor(_out0, _readTensor(f).w)
        }
      }

    val graph = new ComputeGraph(Optimize) with RefTestInterface {
      val f0 = ScalarField.random(256, 256)
      val f1 = ScalarField.random(256, 256)
      val f2 = ScalarField.random(256, 256)
      val f3 = ScalarField.random(256, 256)
      val fused = scrambleDescramble(toTensor(f0, f1, f2, f3))
      val chan0 = channel(fused, 0)
      val chan1 = channel(fused, 1)
      val chan2 = channel(fused, 2)
      val chan3 = channel(fused, 3)

      probe(chan0, f0, chan1, f1, chan2, f2, chan3, f3)
    }
    import graph._
    withRelease {
      step
      require(readScalar(chan0) == readScalar(f0))
      require(readScalar(chan1) == readScalar(f1))
      require(readScalar(chan2) == readScalar(f2))
      require(readScalar(chan3) == readScalar(f3))
    }
  }

  test("built-in functions") {
    // Built-in functions merely generate strings, so functionality tests
    // are not required. We just pick two functions, check them, and hope
    // that correct strings are generated for the rest.
    def maxElement(f1: Field, f2: Field): Field = {
      GPUOperator(f1.fieldType) {
        val x1 = _readTensor(f1)
        val x2 = _readTensor(f2)
        val max = _fmax(x1, x2)
        _writeTensor(_out0, max)
      }
    }

    def dotProduct(f1: Field, f2: Field): Field = {
      GPUOperator(new FieldType(f1.fieldShape, Shape(), Float32)) {
        val x1 = _readTensor(f1)
        val x2 = _readTensor(f2)
        val dot = _dot(x1, x2)
        _writeTensor(_out0, dot)
      }
    }

    val graph = new ComputeGraph(Optimize) with RefTestInterface {
      val in1 = ScalarField.random(5, 5)
      val in2 = ScalarField.random(5, 5)
      val maxElems = maxElement(in1, in2)

      val v1 = VectorField.random(Shape(5, 5), Shape(3))
      val v2 = VectorField.random(Shape(5, 5), Shape(3))
      val dotty = dotProduct(v1, v2)

      probe(in1, in2, maxElems, v1, v2, dotty)
    }
    import graph._
    withRelease {
      step
      val input1 = readScalar(in1)
      val input2 = readScalar(in2)
      val expectedMax = input1.combine(input2, (x, y) => x max y)
      require(readScalar(maxElems) == expectedMax)

      val vector1 = readVector(v1)
      val vector2 = readVector(v2)
      val expectedDot = vector1 dot vector2
      require(readScalar(dotty) ~== expectedDot)
    }
  }

  test("type conversions") {
    // Expects float4 field with integral values, output equals input.
    def copyThrough4(f1: Field): Field = {
      GPUOperator(f1.fieldType) {
        val f = _readTensor(f1)
        val i = _convert_int4(f)
        val out = _convert_float4(i)
        _writeTensor(_out0, out)
      }
    }

    // Expects float2 field with integral values, output equals input.
    def copyThrough2(f1: Field): Field = {
      GPUOperator(f1.fieldType) {
        val f = _readTensor(f1)
        val i = _convert_int2(f)
        val out = _convert_float2(i)
        _writeTensor(_out0, out)
      }
    }

    // Expects float4 field with integral values, output equals input.
    def copyThrough1(f1: Field): Field = {
      GPUOperator(f1.fieldType) {
        val f = _readTensor(f1)
        val i = _convert_int(f)
        val out = _convert_float(i)
        _writeTensor(_out0, out)
      }
    }
    val graph = new ComputeGraph(Optimize) with RefTestInterface {
      val scalarIn = ScalarField(5, 5, (r, c) => r + c)
      val vectorIn2 = VectorField(5, 5, (r, c) => new Vector(2*r, 5*c))
      val vectorIn4 = VectorField(5, 5, (r, c) => new Vector(r, c, 2*r+1, 3*c-1))
      val scalarOut = copyThrough1(scalarIn)
      val vectorOut2 = copyThrough2(vectorIn2)
      val vectorOut4 = copyThrough4(vectorIn4)

      probe(scalarIn, scalarOut, vectorIn2, vectorOut2, vectorIn4, vectorOut4)
    }
    import graph._
    withRelease {
      step
      require(readScalar(scalarIn) == readScalar(scalarOut))
      require(readVector(vectorIn2) == readVector(vectorOut2))
      require(readVector(vectorIn4) == readVector(vectorOut4))
    }
  }

  test("atomic functions") {

    // This code is a little tricky. Basically we are counting the number
    // of threads in a workgroup by having each thread increment location (0, 0)
    // in a local array.
    def countWorkGroupSize(input: Field) =
      GPUOperator(input.fieldType) {
        // Must read *something* from the input field.
        val dummy = _readTensor(input)

        // Allocate shared array and initialize
        val count = _volatile(_local(_intArray(_localRows, _localColumns)))
        count(_localRow, _localColumn) := 0
        _syncThreadsLocal

        // Each thread increments counter in count[0][0]
        val intResult = _intVar()
        intResult := _atomic_inc(_pointerTo(count(0, 0)))
        _syncThreadsLocal

        // If we divide the counter by the number of threads in a workgroup,
        // we should get 1.0
        val workGroupThreads = _convert_float(_localRows * _localColumns)
        val counter = _convert_float(count(0, 0))
        val normalized = counter / workGroupThreads

        // Return result (should be 1.0) for every thread.
        _writeTensor(_out0, normalized)
      }


    val graph = new ComputeGraph(Optimize) with RefTestInterface {
      val scalarIn = ScalarField.random(256, 256)
      val scalarOnes = ScalarField(256, 256, (r, c) => 1.0f)
      val out = countWorkGroupSize(scalarIn)

      probe(out, scalarOnes)
    }
    import graph._
    withRelease {
      step
      require(readScalar(out) == readScalar(scalarOnes))
    }
  }

  test("local threads") {
    def tripleDefault(f: Field) = GPUOperator(f.fieldType) {
      _writeTensor(_out0, _readTensor(f) * 3f)
    }

    def triple16x16(f: Field) = GPUOperator(f.fieldType) {
      _localThreads(Shape(16, 16))
      _writeTensor(_out0, _readTensor(f) * 3f)
    }

    def triple32x8(f: Field) = GPUOperator(f.fieldType) {
      _localThreads(Shape(32, 8))
      _writeTensor(_out0, _readTensor(f) * 3f)
    }

    def triple64x4(f: Field) = GPUOperator(f.fieldType) {
      _localThreads(Shape(64, 4))
      _writeTensor(_out0, _readTensor(f) * 3f)
    }

    def triple128x2(f: Field) = GPUOperator(f.fieldType) {
      _localThreads(Shape(128, 2))
      _writeTensor(_out0, _readTensor(f) * 3f)
    }

    def triple256x1(f: Field) = GPUOperator(f.fieldType) {
      _localThreads(Shape(256, 1))
      _writeTensor(_out0, _readTensor(f) * 3f)
    }

    val graph = new ComputeGraph(Optimize) with RefTestInterface {
      val scalarIn = ScalarField.random(179, 179)
      val tripDefault = tripleDefault(scalarIn)
      val trip16x16   = triple16x16(scalarIn)
      val trip32x8    = triple32x8(scalarIn)
      val trip64x4    = triple64x4(scalarIn)
      val trip128x2   = triple128x2(scalarIn)
      val trip256x1   = triple256x1(scalarIn)

      probe(tripDefault, scalarIn, trip16x16, trip32x8, trip64x4, trip128x2, trip256x1)
    }
    import graph._
    withRelease {
      step
      require(readScalar(tripDefault) == readScalar(scalarIn) * 3f)
      require(readScalar(trip16x16) == readScalar(scalarIn) * 3f)
      require(readScalar(trip32x8) == readScalar(scalarIn) * 3f)
      require(readScalar(trip64x4) == readScalar(scalarIn) * 3f)
      require(readScalar(trip128x2) == readScalar(scalarIn) * 3f)
      require(readScalar(trip256x1) == readScalar(scalarIn) * 3f)
    }
  }

  test("global threads") {
    // Read 16 x 16 tiles, sum them up, then write each some out to a single
    // pixel in the output. This is massive reduction in field size, but the
    // number of global threads is equal to the number of pixels in the input
    // field. So we maintain the massive parallelism.
    def tileSum(f: ScalarField): ScalarField = {
      val outRows = f.rows / 16
      val outColumns = f.columns / 16
      val outShape = Shape(outRows, outColumns)
      GPUOperator(new FieldType(outShape, Shape(), Float32)) {
        _localThreads(Shape(16, 16))
        _globalThreads(f.fieldShape)


        _forEachTensorElement(f.tensorShape) {

          // Init sum to 0. All threads write, a race, doesn't matter here.
          val sum = _volatile(_local(_intVar()))
          sum := 0
          _syncThreadsLocal

          // Atomically sum the tile into sum.
          val pixel = _tensorElementVar(f)
          pixel := _readTensorElement(f, _tensorElement)
          _syncThreadsGlobal
          val intResult = _intVar()
          intResult := _atomic_add(_pointerTo(sum), _convert_int(pixel))
          _syncThreadsLocal

          // OK, now thread (0, 0) for the work group needs to write out the
          // result to the corresponding location in the output.
          _if((_localRow === 0) && (_localColumn === 0)) {
            _writeTensorElement(_out0, _convert_float(sum),
              _groupRow, _groupColumn, _tensorElement)
          }
          _syncThreadsGlobal
        }
      }
    }
    val graph = new ComputeGraph(Optimize) with RefTestInterface {
      val scalarOnes = ScalarField(64, 64, (r, c) => 1.0f)
      val out = tileSum(scalarOnes)
      val expectOut = ScalarField(4, 4, (r, c) => 256.0f)

      probe(out, expectOut)
    }
    import graph._
    withRelease {
      step
      require(readScalar(out) == readScalar(expectOut))
    }
  }

  /** Test that the same GPUOperator applied to the same inputs is recognized by CSE */
  test("common sub elim with probing") {
    def test(probeOutA: Boolean, probeOutB: Boolean) {
      val graph = new ComputeGraph(Optimize) with RefTestInterface {
        val inA = ScalarField.random(3, 5)
        val inB = ScalarField.random(3, 5)
        // outA and outB are produced by identical operators, they should be consolidated by CSE.
        val outA = average(inA, inB)
        val outB = average(inA, inB)
        if (probeOutA)
          probe(outA)
        if (probeOutB)
          probe(outB)
      }
      import graph._
      withRelease {
        step
        // Make sure probing of the fields works as needed in the presence of CSE
        if (probeOutA)
          readScalar(outA)
        if (probeOutB)
          readScalar(outB)
        if (probeOutA && probeOutB)
          require(readScalar(outA) == readScalar(outB))
        if (Optimize && (probeOutA || probeOutB))
          require(hyperkernelCount == 1, s"Expecting 1 hyperkernel, found " + hyperkernelCount)
        if (Optimize && !probeOutA && !probeOutB)
          require(hyperkernelCount == 0, s"Expecting 0 hyperkernels, found " + hyperkernelCount)
      }
    }
    test(false, false)
    test(false, true)
    test(true,  false)
    test(true,  true)
  }

  /** Test that _readTensorElement functions within a merged kernel */
  test("_readTensorElement within a merged kernel using flip") {
    def test(vectorSize: Int) {
      val graph = new ComputeGraph(Optimize) with RefTestInterface {
        val inA = VectorField.random(Shape(3, 5), Shape(vectorSize))
        val inAx2 = inA * 2.0f
        // This should not be merged into one kernel.
        val outA = flipTensor(inAx2)

        probe(outA, inA)
      }
      import graph._
      withRelease {
        step
        require(readVector(outA) == readVector(inA).map(_ * 2.0f).map(v => v.flip))
      }
    }
    for (vectorSize <- 1 to 5)
      test(vectorSize)
  }

  /** Test that _readTensorElement functions within a merged kernel */
  test("_readTensorElement within a merged kernel using flip variant 2") {
    def test(vectorSize: Int) {
      val graph = new ComputeGraph(Optimize) with RefTestInterface {
        val inA = VectorField.random(Shape(3, 5), Shape(vectorSize))
        val inAx2 = inA * 2.0f
        // This should not be merged into one kernel.
        val outA = flipTensor2(inAx2)

        probe(outA, inA)
      }
      import graph._
      withRelease {
        step
        println("2x flipped")
        readVector(outA).print
        println("------------")
        readVector(inA).print
        require(readVector(outA) == readVector(inA).map(_ * 2.0f).map(v => v.flip))
//        if (Optimize && vectorSize <= 4)
//          require(hyperkernelCount == 1, s"Expecting 1 hyperkernel, found " + hyperkernelCount)
      }
    }
    for (vectorSize <- 1 to 5)
      test(vectorSize)
  }

  /** Test that _readTensorElement functions within a merged kernel */
  test("_readTensorElement within a merged kernel using flip variant 3") {
    def test(vectorSize: Int) {
      val graph = new ComputeGraph(Optimize) with RefTestInterface {
        val inA = VectorField.random(Shape(3, 5), Shape(vectorSize))
        val inAx2 = inA * 2.0f
        // This should not be merged into one kernel.
        val outA = flipTensor3(inAx2)

        probe(outA, inA)
      }
      import graph._
      withRelease {
        step
        require(readVector(outA) == readVector(inA).map(_ * 2.0f).map(v => v.flip))
        // The *2.0f kernel operates in SmallTensor mode, the flip3 in TensorAddressing mode, so no merging
        // for tensor sizes 1, 2, 3 and 4
        if (Optimize && vectorSize >= 5)
          require(hyperkernelCount == 1, s"Expecting 1 hyperkernel, found " + hyperkernelCount)
      }
    }
    for (vectorSize <- 1 to 7)
      test(vectorSize)
  }

  /** Test that _readTensorElement functions within a merged kernel */
  test("_readTensorElement within a merged kernel using flip variant 3 and Times2 GPU Operator") {
    def test(vectorSize: Int) {
      val graph = new ComputeGraph(Optimize) with RefTestInterface {
        val inA = VectorField.random(Shape(3, 5), Shape(vectorSize))
        val inAx2 = times2(inA)
        val outA = flipTensor3(inAx2)

        probe(outA, inA)
      }
      import graph._
      withRelease {
        step
        require(readVector(outA) == readVector(inA).map(_ * 2.0f).map(v => v.flip))
        // The *2.0f kernel operates in TensorAddressing mode, so merging for all tensor sizes is expected
        if (Optimize)
          require(hyperkernelCount == 1, s"Expecting 1 hyperkernel, found " + hyperkernelCount)
      }
    }
    for (vectorSize <- 1 to 7)
      test(vectorSize)
  }

  /** Test that _writeTensorElement functions within a merged kernel. */
  test("_writeTensorElement using flip variant 3") {
    def test(vectorSize: Int) {
      val graph = new ComputeGraph(Optimize) with RefTestInterface {
        val inA = VectorField.random(Shape(3, 5), Shape(vectorSize))
        // This should not be merged into one kernel.
        val outA = flipTensor3(flipTensor3(inA))

        probe(outA, inA)
      }
      import graph._
      withRelease {
        step
        require(readVector(outA) == readVector(inA))
      }
    }
    for (vectorSize <- 1 to 7)
      test(vectorSize)
  }

  /** Test that _writeTensorElement functions within a merged kernel. */
  test("_writeTensorElement within a merged kernel using flip variants 2 and 3") {
    def test(vectorSize: Int) {
      val graph = new ComputeGraph(Optimize) with RefTestInterface {
        val inA = VectorField.random(Shape(3, 5), Shape(vectorSize))
        val outA = flipTensor3(flipTensor2(inA))

        probe(outA, inA)
      }
      import graph._
      withRelease {
        step
        require(readVector(outA) == readVector(inA))
      }
    }
    for (vectorSize <- 1 to 7)
      test(vectorSize)
  }

    /** Test that common subexpression elimination distinguishes between two outputs of the same kernel */
  test("simple dual-output GPUoperator- test of CSE") {
    val cg = new ComputeGraph{
      val a = ScalarField()

      val (b0,b1) =
        GPUOperator(a.fieldType, a.fieldType, "makeTwoDifferentOutputs"){
          val x = _readTensor(a)
          _writeTensor(_out0, x + 1f)
          _writeTensor(_out1, x + 2f)
        }

      // Use different outputs identically: CSE should not consolidate "* 1f"
      // since the inputs are different outputs of the same kernel
      val c = b0 * 1f + b1 * 1f
      // The bug this test caught evidenced itself by the c field becoming unprobeable
      probe(c)
    }
    import cg._
    withRelease {
      step
      require(read(c).asInstanceOf[ScalarFieldReader].read() == 3f)
    }
  }

  /** Test dual-output operators on a toy complex-field class */
  test("multi-output operators") {

    /** Factory method for user-level ComplexFields */
    object UserComplexField {
      def apply(real: Field, imaginary: Field) = new UserComplexField(real, imaginary)

      def apply(tuple2: Tuple2[Field, Field]) = new UserComplexField(tuple2._1, tuple2._2)

      def apply(fieldShape: Shape, tensorShape: Shape = Shape()) = new UserComplexField(
        Field(new FieldType(fieldShape, tensorShape, Float32)),
        Field(new FieldType(fieldShape, tensorShape, Float32))
      )

      def random(fieldShape: Shape) = new UserComplexField(
        ScalarField.random(fieldShape),
        ScalarField.random(fieldShape)
      )

      def random(fieldShape: Shape, tensorShape: Shape = Shape()) =
        tensorShape.dimensions match {
          case 0 => new UserComplexField(
            ScalarField.random(fieldShape),
            ScalarField.random(fieldShape)
          )
          case 1 => new UserComplexField(
            VectorField.random(fieldShape, tensorShape),
            VectorField.random(fieldShape, tensorShape)
          )
          case 2 => new UserComplexField(
            MatrixField.random(fieldShape, tensorShape),
            MatrixField.random(fieldShape, tensorShape)
          )
          case x => throw new RuntimeException(s"Tensor shape of dimension $x not supported, must be <= 2.")
        }
    }

    /** Simple user-level ComplexField class.  Supports only '+' and '*' */
    class UserComplexField(val real: Field, val imaginary: Field) {
      require(real.fieldType == imaginary.fieldType,
        s"Mismatched real/imaginary fields: $real.fieldType $imaginary.fieldType")

      def componentFieldType = real.fieldType

      def tensorShape = componentFieldType.tensorShape

      /** big / small tensor: add two complex fields */
      private def complexAdd(f1: UserComplexField, f2: UserComplexField): UserComplexField = {
        // Semantic checking
        require(f1.componentFieldType == f2.componentFieldType, "Fields must have the same type")
        UserComplexField(GPUOperator(f1.componentFieldType, f1.componentFieldType, "complexAdd") {
          if (f1.tensorShape.points > 4) {
            _forEachTensorElement(f1.tensorShape) {
              val r1 = _readTensorElement(f1.real, _tensorElement)
              val i1 = _readTensorElement(f1.imaginary, _tensorElement)
              val r2 = _readTensorElement(f2.real, _tensorElement)
              val i2 = _readTensorElement(f2.imaginary, _tensorElement)
              _writeTensorElement(_out0, r1 + r2, _tensorElement)
              _writeTensorElement(_out1, i1 + i2, _tensorElement)
            }
          } else {
            val r1 = _readTensor(f1.real)
            val i1 = _readTensor(f1.imaginary)
            val r2 = _readTensor(f2.real)
            val i2 = _readTensor(f2.imaginary)
            _writeTensor(_out0, r1 + r2)
            _writeTensor(_out1, i1 + i2)
          }
        })
      }

      /** big / small tensor: multiply two complex fields */
      private def complexMultiply(f1: UserComplexField, f2: UserComplexField): UserComplexField = {
        // Semantic checking
        require(f1.componentFieldType == f2.componentFieldType, "Fields must have the same type")
        UserComplexField(GPUOperator(f1.componentFieldType, f1.componentFieldType, "complexMultiply") {
          if (f1.tensorShape.points > 4) {
            _forEachTensorElement(f1.tensorShape) {
              val r1 = _readTensorElement(f1.real, _tensorElement)
              val i1 = _readTensorElement(f1.imaginary, _tensorElement)
              val r2 = _readTensorElement(f2.real, _tensorElement)
              val i2 = _readTensorElement(f2.imaginary, _tensorElement)
              _writeTensorElement(_out0, r1*r2-i1*i2, _tensorElement)
              _writeTensorElement(_out1, r1*i2 + i1*r2, _tensorElement)
            }
          } else {
            val r1 = _readTensor(f1.real)
            val i1 = _readTensor(f1.imaginary)
            val r2 = _readTensor(f2.real)
            val i2 = _readTensor(f2.imaginary)
            _writeTensor(_out0, r1*r2-i1*i2)
            _writeTensor(_out1, r1*i2 + i1*r2)
          }
        })
      }

      def +(that: UserComplexField) = complexAdd(this, that)

      def *(that: UserComplexField) = complexMultiply(this, that)

    }

    /** Test UserComplexField against legacy ComplexField class */
    def test(fieldShape: Shape) {
      val graph = new ComputeGraph(Optimize) with RefTestInterface {
        val inA = ComplexField.random(fieldShape)
        val inB = ComplexField.random(fieldShape)
        val outAdd = inA + inB
        val outMultiply = inA * inB

        // Now do the same with this test's GPU operators

        val userInA = UserComplexField(realPart(inA), imaginaryPart(inA))
        val userInB = UserComplexField(realPart(inB), imaginaryPart(inB))
        val userOutAdd = userInA + userInB
        val userOutMultiply = userInA * userInB

        probe(outAdd, userOutAdd.real, userOutAdd.imaginary,
          outMultiply, userOutMultiply.real, userOutMultiply.imaginary)

      }
      import graph._
      withRelease {
        step
        require(readComplex(outAdd).realPart == readScalar(userOutAdd.real))
        require(readComplex(outAdd).imaginaryPart == readScalar(userOutAdd.imaginary))
        require(readComplex(outMultiply).realPart ~== readScalar(userOutMultiply.real))
        require(readComplex(outMultiply).imaginaryPart ~== readScalar(userOutMultiply.imaginary))
      }
    }


    /** Test UserComplexField against legacy ComplexVectorField class */
    def testVector(fieldShape: Shape, vectorLength: Int) {
      val graph = new ComputeGraph(Optimize) with RefTestInterface {
        val inA = ComplexVectorField.random(fieldShape, Shape(vectorLength))
        val inB = ComplexVectorField.random(fieldShape, Shape(vectorLength))
        val inC = ComplexVectorField.random(fieldShape, Shape(vectorLength))
        val outAdd = inA + inB
        val outMultiply = inA * inB
        val outVarious = inA * inB + inB * inC + inA * inC

        // Now do the same with this test's GPU operators

        val userInA = UserComplexField(realPart(inA), imaginaryPart(inA))
        val userInB = UserComplexField(realPart(inB), imaginaryPart(inB))
        val userInC = UserComplexField(realPart(inC), imaginaryPart(inC))
        val userOutAdd = userInA + userInB
        val userOutMultiply = userInA * userInB
        val userOutVarious = userInA * userInB + userInB * userInC + userInA * userInC

        probe(outAdd, userOutAdd.real, userOutAdd.imaginary,
          outMultiply, userOutMultiply.real, userOutMultiply.imaginary,
          outVarious, userOutVarious.real, userOutVarious.imaginary)
      }
      import graph._
      withRelease {
        step
        require(readRealVector(outAdd) == readVector(userOutAdd.real))
        require(readImaginaryVector(outAdd) == readVector(userOutAdd.imaginary))
        require(readRealVector(outMultiply) ~== readVector(userOutMultiply.real))
        require(readImaginaryVector(outMultiply) ~== readVector(userOutMultiply.imaginary))
        require(readRealVector(outVarious) ~== readVector(userOutVarious.real))
        require(readImaginaryVector(outVarious) ~== readVector(userOutVarious.imaginary))
      }
    }

    // 2D and 3D ComplexField tests
    test(Shape(5,5))
    test(Shape(4,5,6))

    // 2D ComplexVectorField tests
    for(vectorDepth <- 1 to 5)
      testVector(Shape(vectorDepth*2, vectorDepth*3), vectorDepth)
  }

  /** Test proper handling of NaN and other special Float values. */
  test("Proper handling of Float.NaN and other special Float values.") {
    // The following attempt to catch a failed compile doesn't work- must happen on another thread.
    def test(f: Float) {
      try {
        val graph = new ComputeGraph(Optimize) with RefTestInterface {
          val inA = ScalarField()
          // This should not be merged into one kernel.
          val outA = addConst(inA, f)

          probe(outA, inA)
        }
        graph.withRelease {
          graph.step
        }
        require(true)
      } catch {
        case e: com.jogamp.opencl.CLException => require(false, s"Floating constant $f did not compile.")
      }
    }
    test(Float.NaN)
    test(Float.PositiveInfinity)
    test(Float.NegativeInfinity)
  }

  /** Test that _readTensorElement functions within a merged kernel */
  test("concurrent compilation and running of compute graphs with GPUOperators") {
    def test(id: Int, vectorSize: Int, rows: Int, columns: Int, flipPairs: Int): Boolean = {
      def flipNtimes(n: Int, f: Field): Field = {
        if (n == 0) f else flipNtimes(n-1, flipTensor(f))
      }
      val graph = new ComputeGraph(Optimize) with RefTestInterface {
        val inA = VectorField.random(Shape(rows, columns), Shape(vectorSize))
        val inAx2 = inA * 2.0f
        val outA = flipNtimes(1+flipPairs*2, inAx2)
        probe(outA, inA)
      }
      import graph._
      withRelease {
        step
        require(readVector(outA) == readVector(inA).map(_ * 2.0f).map(v => v.flip), s"test $id fails.")
      }
      true
    }

    def runTest(id: Int, vectorSize: Int, rows: Int, columns: Int, flipPairs: Int) = Future[Boolean] {
      val result = test(id, vectorSize, rows, columns, flipPairs)
      result
    }

    val numTests = 10

    // Something to try if this test's sporadic hanging returns:
//    val system = CogActorSystem()
//    implicit val ec: ExecutionContext = system.dispatcher

    val runners = Array.tabulate[Future[Boolean]](numTests)(i => runTest(i, 10 + i, 3 + i, 5 + i, 1 + i))

    println(s"Waiting for $numTests concurrent tests to finish.")
    runners.foreach(runner => Await.result(runner, 60 seconds))
    println(s"All $numTests concurrent tests have finished.")
  }

  /*
  test("_convert_float problem") {
    def tileSum(f: ScalarField): ScalarField = {
      val outRows = f.rows / 16
      val outColumns = f.columns / 16
      val outShape = Shape(outRows, outColumns)
      GPUOperator(new FieldType(outShape, Shape(), Float32)) {
        _localThreads(Shape(16, 16))
        _globalThreads(f.fieldType)

        val intValue = _intVar()
        intValue := 123

        ////////// Problem shows up here: the two assignments have different
        ////////// results
        val floatValue = _convert_float(intValue)
//        val floatValue = 123.0f

        _if ((_localRow === 0) && (_localColumn === 0)) {
          _writeTensor(_out0, floatValue, _groupRow, _groupColumn)
        }
        // Uncomment the next line to see the generated OpenCL code:
        //_debug
      }
    }

    val graph = new ComputeGraph(Optimize) with RefTestInterface {
      val scalarOnes = ScalarField(64, 64, (r, c) => 1.0f)
      val out = tileSum(scalarOnes)
    }
    import graph._
    withRelease {
      step
      //readScalar(out).print
    }
  }
  */
}