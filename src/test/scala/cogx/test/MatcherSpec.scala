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

package cogx.test

import scala.language.reflectiveCalls
import org.scalatest.MustMatchers
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith
import cogx.runtime.ComputeGraph
import cogx.compiler.parser.syntaxtree.{ScalarField, VectorField, MatrixField, ColorField}
import cogx.cogmath.algebra.real.{Vector, Matrix}
import cogx.platform.types.Pixel
import cogx.platform.cpumemory.readerwriter.FieldReader

@RunWith(classOf[JUnitRunner])
class MatcherSpec extends FunSuite with MustMatchers with CogMatchers {
  test("scalar, 0D") {
    val app = new ComputeGraph(optimize = false) {
      val f1 = ScalarField()
      val f2 = ScalarField()
      val f3 = ScalarField(0f)
      val f4 = ScalarField(1f)
      val f5 = ScalarField(0.0001f)
    }

    app.step
    val f1: FieldReader = app.read(app.f1)
    val f2 = app.read(app.f2)
    val f3 = app.read(app.f3)
    val f4 = app.read(app.f4)
    val f5 = app.read(app.f5)

    f1 must equal (f2)
    f1 must approxEqual (f2)
    f1 must equal (f3)
    f1 must approxEqual (f3)
    f1 must not (equal (f4))
    f1 must not (approxEqual (f4))
    f1 must not (equal (f5))
    f1 must approxEqual (f5)

    app.release
  }

  test("scalar, 1D") {
    val app = new ComputeGraph(optimize = false) {
      val f1 = ScalarField(5)
      val f2 = ScalarField(5)
      val f3 = ScalarField(5, _ => 0f)
      val f4 = ScalarField(5, _ => 1f)
      val f5 = ScalarField(10)
      val f6 = ScalarField()
    }

    app.step
    val f1 = app.read(app.f1)
    val f2 = app.read(app.f2)
    val f3 = app.read(app.f3)
    val f4 = app.read(app.f4)
    val f5 = app.read(app.f5)
    val f6 = app.read(app.f6)

    f1 must equal (f2)
    f1 must approxEqual (f2)
    f1 must equal (f3)
    f1 must approxEqual (f3)
    f1 must not (equal (f4))
    f1 must not (approxEqual (f4))
    f1 must not (equal (f5))
    f1 must not (approxEqual (f5))
    f1 must not (equal (f6))
    f1 must not (approxEqual (f6))

    app.release
  }

  test("scalar, 2D") {
    val app = new ComputeGraph(optimize = false) {
      val f1 = ScalarField(5, 6)
      val f2 = ScalarField(5, 6)
      val f3 = ScalarField(5, 6, (_, _) => 0f)
      val f4 = ScalarField(5, 6, (_, _) => 1f)
      val f5 = ScalarField(5, 7)
      val f6 = ScalarField(5)
      val f7 = ScalarField()
    }

    app.step
    val f1 = app.read(app.f1)
    val f2 = app.read(app.f2)
    val f3 = app.read(app.f3)
    val f4 = app.read(app.f4)
    val f5 = app.read(app.f5)
    val f6 = app.read(app.f6)
    val f7 = app.read(app.f7)

    f1 must equal (f2)
    f1 must approxEqual (f2)
    f1 must equal (f3)
    f1 must approxEqual (f3)
    f1 must not (equal (f4))
    f1 must not (approxEqual (f4))
    f1 must not (equal (f5))
    f1 must not (approxEqual (f5))
    f1 must not (equal (f6))
    f1 must not (approxEqual (f6))
    f1 must not (equal (f7))
    f1 must not (approxEqual (f7))

    app.release
  }

  test("scalar, 3D") {
    val app = new ComputeGraph(optimize = false) {
      val f1 = ScalarField(5, 6, 7)
      val f2 = ScalarField(5, 6, 7)
      val f3 = ScalarField(5, 6, 7, (_, _, _) => 0f)
      val f4 = ScalarField(5, 6, 7, (_, _, _) => 1f)
      val f5 = ScalarField(5, 6, 8)
      val f6 = ScalarField(5, 6)
      val f7 = ScalarField(5)
      val f8 = ScalarField()
    }

    app.step
    val f1 = app.read(app.f1)
    val f2 = app.read(app.f2)
    val f3 = app.read(app.f3)
    val f4 = app.read(app.f4)
    val f5 = app.read(app.f5)
    val f6 = app.read(app.f6)
    val f7 = app.read(app.f7)
    val f8 = app.read(app.f8)

    f1 must equal (f2)
    f1 must approxEqual (f2)
    f1 must equal (f3)
    f1 must approxEqual (f3)
    f1 must not (equal (f4))
    f1 must not (approxEqual (f4))
    f1 must not (equal (f5))
    f1 must not (approxEqual (f5))
    f1 must not (equal (f6))
    f1 must not (approxEqual (f6))
    f1 must not (equal (f7))
    f1 must not (approxEqual (f7))
    f1 must not (equal (f8))
    f1 must not (approxEqual (f8))

    app.release
  }

  test("vector, 0D") {
    val app = new ComputeGraph(optimize = false) {
      val f1 = VectorField(Vector(0f, 0f))
      val f2 = VectorField(Vector(0f, 0f))
      val f3 = VectorField(Vector(1f, 0f))
      val f4 = VectorField(Vector(0f))
      val f5 = VectorField(Vector(0.0001f, 0f))
    }

    app.step
    val f1 = app.read(app.f1)
    val f2 = app.read(app.f2)
    val f3 = app.read(app.f3)
    val f4 = app.read(app.f4)
    val f5 = app.read(app.f5)

    f1 must equal (f2)
    f1 must approxEqual (f2)
    f1 must not (equal (f3))
    f1 must not (approxEqual (f3))
    f1 must not (equal (f4))
    f1 must not (approxEqual (f4))
    f1 must not (equal (f5))
    f1 must approxEqual (f5)

    app.release
  }

  test("vector, 1D") {
    val app = new ComputeGraph(optimize = false) {
      val f1 = VectorField(5, (_) => Vector(0f, 0f))
      val f2 = VectorField(5, (_) => Vector(0f, 0f))
      val f3 = VectorField(5, (_) => Vector(1f, 0f))
      val f4 = VectorField(5, (_) => Vector(0f))
      val f5 = VectorField(4, (_) => Vector(0f, 0f))
      val f6 = VectorField(Vector(0f, 0f))
    }

    app.step
    val f1 = app.read(app.f1)
    val f2 = app.read(app.f2)
    val f3 = app.read(app.f3)
    val f4 = app.read(app.f4)
    val f5 = app.read(app.f5)
    val f6 = app.read(app.f6)

    f1 must equal (f2)
    f1 must approxEqual (f2)
    f1 must not (equal (f3))
    f1 must not (approxEqual (f3))
    f1 must not (equal (f4))
    f1 must not (approxEqual (f4))
    f1 must not (equal (f5))
    f1 must not (approxEqual (f5))
    f1 must not (equal (f6))
    f1 must not (approxEqual (f6))

    app.release
  }

  test("vector, 2D") {
    val app = new ComputeGraph(optimize = false) {
      val f1 = VectorField(5, 6, (_, _) => Vector(0f, 0f))
      val f2 = VectorField(5, 6, (_, _) => Vector(0f, 0f))
      val f3 = VectorField(5, 6, (_, _) => Vector(1f, 0f))
      val f4 = VectorField(5, 6, (_, _) => Vector(0f))
      val f5 = VectorField(5, 5, (_, _) => Vector(0f, 0f))
      val f6 = VectorField(5, (_) => Vector(0f, 0f))
      val f7 = VectorField(Vector(0f, 0f))
    }

    app.step
    val f1 = app.read(app.f1)
    val f2 = app.read(app.f2)
    val f3 = app.read(app.f3)
    val f4 = app.read(app.f4)
    val f5 = app.read(app.f5)
    val f6 = app.read(app.f6)
    val f7 = app.read(app.f7)

    f1 must equal (f2)
    f1 must approxEqual (f2)
    f1 must not (equal (f3))
    f1 must not (approxEqual (f3))
    f1 must not (equal (f4))
    f1 must not (approxEqual (f4))
    f1 must not (equal (f5))
    f1 must not (approxEqual (f5))
    f1 must not (equal (f6))
    f1 must not (approxEqual (f6))
    f1 must not (equal (f7))
    f1 must not (approxEqual (f7))

    app.release
  }

  test("vector, 3D") {
    val app = new ComputeGraph(optimize = false) {
      val f1 = VectorField(5, 6, 7, (_, _, _) => Vector(0f, 0f))
      val f2 = VectorField(5, 6, 7, (_, _, _) => Vector(0f, 0f))
      val f3 = VectorField(5, 6, 7, (_, _, _) => Vector(1f, 0f))
      val f4 = VectorField(5, 6, 7, (_, _, _) => Vector(0f))
      val f5 = VectorField(5, 6, 8, (_, _, _) => Vector(0f, 0f))
      val f6 = VectorField(5, 6, (_, _) => Vector(0f, 0f))
      val f7 = VectorField(5, _ => Vector(0f, 0f))
      val f8 = VectorField(Vector(0f, 0f))
    }

    app.step
    val f1 = app.read(app.f1)
    val f2 = app.read(app.f2)
    val f3 = app.read(app.f3)
    val f4 = app.read(app.f4)
    val f5 = app.read(app.f5)
    val f6 = app.read(app.f6)
    val f7 = app.read(app.f7)
    val f8 = app.read(app.f8)

    f1 must equal (f2)
    f1 must approxEqual (f2)
    f1 must not (equal (f3))
    f1 must not (approxEqual (f3))
    f1 must not (equal (f4))
    f1 must not (approxEqual (f4))
    f1 must not (equal (f5))
    f1 must not (approxEqual (f5))
    f1 must not (equal (f6))
    f1 must not (approxEqual (f6))
    f1 must not (equal (f7))
    f1 must not (approxEqual (f7))
    f1 must not (equal (f8))
    f1 must not (approxEqual (f8))

    app.release
  }

  test("matrix, 0D") {
    val app = new ComputeGraph(optimize = false) {
      val f1 = MatrixField(Matrix(4, 4, (_, _) => 0f))
      val f2 = MatrixField(Matrix(4, 4, (_, _) => 0f))
      val f3 = MatrixField(Matrix(4, 4, (_, _) => 1f))
      val f4 = MatrixField(Matrix(4, 3, (_, _) => 0f))
      val f5 = MatrixField(Matrix(4, 4, (_, _) => 0.0001f))
    }

    app.step
    val f1 = app.read(app.f1)
    val f2 = app.read(app.f2)
    val f3 = app.read(app.f3)
    val f4 = app.read(app.f4)
    val f5 = app.read(app.f5)

    f1 must equal (f2)
    f1 must approxEqual (f2)
    f1 must not (equal (f3))
    f1 must not (approxEqual (f3))
    f1 must not (equal (f4))
    f1 must not (approxEqual (f4))
    f1 must not (equal (f5))
    f1 must approxEqual (f5)

    app.release
  }

  test("matrix, 1D") {
    val app = new ComputeGraph(optimize = false) {
      val f1 = MatrixField(7, (_) => Matrix(4, 4, (_, _) => 0f))
      val f2 = MatrixField(7, (_) => Matrix(4, 4, (_, _) => 0f))
      val f3 = MatrixField(7, (_) => Matrix(4, 4, (_, _) => 1f))
      val f4 = MatrixField(7, (_) => Matrix(4, 3, (_, _) => 0f))
      val f5 = MatrixField(8, (_) => Matrix(4, 4, (_, _) => 0f))
      val f6 = MatrixField(Matrix(4, 4, (_, _) => 0f))
    }

    app.step
    val f1 = app.read(app.f1)
    val f2 = app.read(app.f2)
    val f3 = app.read(app.f3)
    val f4 = app.read(app.f4)
    val f5 = app.read(app.f5)
    val f6 = app.read(app.f6)

    f1 must equal (f2)
    f1 must approxEqual (f2)
    f1 must not (equal (f3))
    f1 must not (approxEqual (f3))
    f1 must not (equal (f4))
    f1 must not (approxEqual (f4))
    f1 must not (equal (f5))
    f1 must not (approxEqual (f5))
    f1 must not (equal (f6))
    f1 must not (approxEqual (f6))

    app.release
  }

  test("matrix, 2D") {
    val app = new ComputeGraph(optimize = false) {
      val f1 = MatrixField(6, 7, (_, _) => Matrix(4, 4, (_, _) => 0f))
      val f2 = MatrixField(6, 7, (_, _) => Matrix(4, 4, (_, _) => 0f))
      val f3 = MatrixField(6, 7, (_, _) => Matrix(4, 4, (_, _) => 1f))
      val f4 = MatrixField(6, 7, (_, _) => Matrix(4, 3, (_, _) => 0f))
      val f5 = MatrixField(6, 8, (_, _) => Matrix(4, 4, (_, _) => 0f))
      val f6 = MatrixField(6, (_) => Matrix(4, 4, (_, _) => 0f))
      val f7 = MatrixField(Matrix(4, 4, (_, _) => 0f))
    }

    app.step
    val f1 = app.read(app.f1)
    val f2 = app.read(app.f2)
    val f3 = app.read(app.f3)
    val f4 = app.read(app.f4)
    val f5 = app.read(app.f5)
    val f6 = app.read(app.f6)
    val f7 = app.read(app.f7)

    f1 must equal (f2)
    f1 must approxEqual (f2)
    f1 must not (equal (f3))
    f1 must not (approxEqual (f3))
    f1 must not (equal (f4))
    f1 must not (approxEqual (f4))
    f1 must not (equal (f5))
    f1 must not (approxEqual (f5))
    f1 must not (equal (f6))
    f1 must not (approxEqual (f6))
    f1 must not (equal (f7))
    f1 must not (approxEqual (f7))

    app.release
  }

  test("matrix, 3D") {
    val app = new ComputeGraph(optimize = false) {
      val f1 = MatrixField(5, 6, 7, (_, _, _) => Matrix(4, 4, (_, _) => 0f))
      val f2 = MatrixField(5, 6, 7, (_, _, _) => Matrix(4, 4, (_, _) => 0f))
      val f3 = MatrixField(5, 6, 7, (_, _, _) => Matrix(4, 4, (_, _) => 1f))
      val f4 = MatrixField(5, 6, 7, (_, _, _) => Matrix(4, 3, (_, _) => 0f))
      val f5 = MatrixField(5, 6, 8, (_, _, _) => Matrix(4, 4, (_, _) => 0f))
      val f6 = MatrixField(5, 6, (_, _) => Matrix(4, 4, (_, _) => 0f))
      val f7 = MatrixField(5, _ => Matrix(4, 4, (_, _) => 0f))
      val f8 = MatrixField(Matrix(4, 4, (_, _) => 0f))
    }

    app.step
    val f1 = app.read(app.f1)
    val f2 = app.read(app.f2)
    val f3 = app.read(app.f3)
    val f4 = app.read(app.f4)
    val f5 = app.read(app.f5)
    val f6 = app.read(app.f6)
    val f7 = app.read(app.f7)
    val f8 = app.read(app.f8)

    f1 must equal (f2)
    f1 must approxEqual (f2)
    f1 must not (equal (f3))
    f1 must not (approxEqual (f3))
    f1 must not (equal (f4))
    f1 must not (approxEqual (f4))
    f1 must not (equal (f5))
    f1 must not (approxEqual (f5))
    f1 must not (equal (f6))
    f1 must not (approxEqual (f6))
    f1 must not (equal (f7))
    f1 must not (approxEqual (f7))
    f1 must not (equal (f8))
    f1 must not (approxEqual (f8))

    app.release
  }

  test("image") {
    val app = new ComputeGraph(optimize = false) {
      val f1 = ColorField(6, 7, (_, _) => new Pixel(0, 0, 0))
      val f2 = ColorField(6, 7, (_, _) => new Pixel(0, 0, 0))
      val f3 = ColorField(6, 7, (_, _) => new Pixel(1, 0, 0))
      val f4 = ColorField(6, 8, (_, _) => new Pixel(0, 0, 0))
    }

    app.step
    val f1 = app.read(app.f1)
    val f2 = app.read(app.f2)
    val f3 = app.read(app.f3)
    val f4 = app.read(app.f4)

    f1 must equal (f2)
    f1 must approxEqual (f2)
    f1 must not (equal (f3))
    f1 must not (approxEqual (f3))
    f1 must not (equal (f4))
    f1 must not (approxEqual (f4))

    app.release
  }
}