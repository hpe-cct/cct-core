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

import cogx.api.CogFunctionAPI
import org.scalatest.FunSuite
import cogx.reference.RefTestInterface
import cogx.helper.ScalarFieldBuilderInterface
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

/** Tests for reshape operator
  *
  * @author Matthew Pickett
  */
@RunWith(classOf[JUnitRunner])
class ReshapeSpec
  extends FunSuite
  with ScalarFieldBuilderInterface
  with CogFunctionAPI
{
  test("reshape a 2d scalar field to a 0d vector field and vice versa"){
    val aRef = Array(
      Array(0f, 1f, 2f),
      Array(3f, 4f, 5f),
      Array(6f, 7f, 8f),
      Array(9f, 10f, 11f)
    )
    val bRef = Array(0f,1f,2f,3f,4f,5f,6f,7f,8f,9f,10f,11f)

    val graph = new ComputeGraph(optimize = false) with RefTestInterface {
      val aIn = ScalarField(4,3,(r,c)=>aRef(r)(c))
      val bIn = VectorField(Vector(12,(i)=>bRef(i)))

      val aReshaped = reshape(aIn, Shape(),Shape(12))
      val bReshaped = reshape(bIn, Shape(4,3),Shape())

      probe(bReshaped, aIn, aReshaped, bIn)
    }

    import graph._
    withRelease {
      step
      require(readScalar(bReshaped) == readScalar(aIn))
      require(readVector(aReshaped) == readVector(bIn))
    }
  }

  test("reshape a 2d scalar field to a 2d 1x1 vector field and vice versa"){
    val aRef = Array(
      Array(0f, 1f, 2f),
      Array(3f, 4f, 5f),
      Array(6f, 7f, 8f),
      Array(9f, 10f, 11f)
    )
    val bRef = Array(0f,1f,2f,3f,4f,5f,6f,7f,8f,9f,10f,11f)

    val graph = new ComputeGraph(optimize = false) with RefTestInterface {
      val aIn = ScalarField(4,3,(r,c)=>aRef(r)(c))
      val bIn = VectorField(1,1,(r,c)=>Vector(12,(i)=>bRef(i)))

      val aReshaped = reshape(aIn, Shape(1,1),Shape(12))
      val bReshaped = reshape(bIn, Shape(4,3),Shape())

      probe(bReshaped, aIn, aReshaped, bIn)
    }

    import graph._
    withRelease {
      step
      require(readScalar(bReshaped) == readScalar(aIn))
      require(readVector(aReshaped) == readVector(bIn))
    }
  }

  test("reshape a 2d vector field with length-1 vectors to a 0d vector field and vice versa"){
    val aRef = Array(
      Array(0f, 1f, 2f),
      Array(3f, 4f, 5f),
      Array(6f, 7f, 8f),
      Array(9f, 10f, 11f)
    )
    val bRef = Array(0f,1f,2f,3f,4f,5f,6f,7f,8f,9f,10f,11f)

    val graph = new ComputeGraph(optimize = false) with RefTestInterface {
      val aIn = VectorField(4,3,(r,c)=> Vector(aRef(r)(c)))
      val bIn = VectorField(Vector(12,(i)=>bRef(i)))

      val aReshaped = reshape(aIn, Shape(),Shape(12))
      val bReshaped = reshape(bIn, Shape(4,3),Shape(1))

      probe(bReshaped, aIn, aReshaped, bIn)
    }

    import graph._
    withRelease {
      step
      require(readVector(bReshaped) == readVector(aIn))
      require(readVector(aReshaped) == readVector(bIn))
    }
  }

  // The behavior of this case changed as of libcog 4.3.  Before that
  // the reshape operator was not true to the statement that the data
  // layout should be unchanged with a reshape.
  test("reshape a 1d scalar field to a 1d vector field and vice versa"){
    val aRef = Array(
      Array(0f, 1f, 2f),     // the vector element 0's
      Array(3f, 4f, 5f),     // the vector element 1's
      Array(6f, 7f, 8f),     // the vector element 2's
      Array(9f, 10f, 11f)    // the vector element 3's
    )
    // i.e. first vector is (0f, 3f, 6f, 9f)

    val bRef = Array(0f,1f,2f,3f,4f,5f,6f,7f,8f,9f,10f,11f)
    // The scalarfield layout is:
    //
    // 0f  1f  2f  3f  4f  5f  6f  7f  8f  9f 10f 11f
    //
    // The vectorfield interpretation of this sequence, without reordering is:
    //
    // 0f  1f  2f    <- The vector element 0 "plane" of the 1D vector field
    //
    // 3f  4f  5f    <- The vector element 1 "plane" of the 1D vector field
    //
    // 6f  7f  8f    <- The vector element 2 "plane" of the 1D vector field
    //
    // 9f 10f 11f    <- The vector element 3 "plane" of the 1D vector field

    val graph = new ComputeGraph(optimize = false) with RefTestInterface {
      val aIn = ScalarField(12,(c)=>bRef(c))
      val bIn = VectorField(3, (c) => Vector(4,(i)=>aRef(i)(c)))

      val aReshaped = reshape(aIn, Shape(3),Shape(4))
      val bReshaped = reshape(bIn, Shape(12),Shape())

      probe(bReshaped, aIn, aReshaped, bIn)
    }

    import graph._
    withRelease {
      step
      require(readScalar(bReshaped) == readScalar(aIn))
      require(readVector(aReshaped) == readVector(bIn))
    }
  }

  // The behavior of this case changed as of libcog 4.3.  Before that
  // the reshape operator was not true to the statement that the data
  // layout should be unchanged with a reshape.
  test("reshape a 2d scalar field to a 1d vector field and vice versa"){
    // Scalar field with each sub-array being a row
    val aRef = Array(
      Array(0f, 1f, 2f),     // also the vector element 0's
      Array(3f, 4f, 5f),     // also the vector element 1's
      Array(6f, 7f, 8f),     // also the vector element 2's
      Array(9f, 10f, 11f)    // also the vector element 3's
    )

    // The scalarfield layout is:
    //
    // 0f  1f  2f  3f  4f  5f  6f  7f  8f  9f 10f 11f
    //
    // The vectorfield interpretation of this sequence, without reordering is:
    //
    // 0f  1f  2f    <- The vector element 0 "plane" of the 1D vector field
    //
    // 3f  4f  5f    <- The vector element 1 "plane" of the 1D vector field
    //
    // 6f  7f  8f    <- The vector element 2 "plane" of the 1D vector field
    //
    // 9f 10f 11f    <- The vector element 3 "plane" of the 1D vector field

    val graph = new ComputeGraph(optimize = false) with RefTestInterface {
      val aIn = ScalarField(4,3,(r,c)=>aRef(r)(c))
      val bIn = VectorField(3, (c) => Vector(4,(i)=>aRef(i)(c)))

      val aReshaped = reshape(aIn, Shape(3),Shape(4))
      val bReshaped = reshape(bIn, Shape(4,3),Shape())

      probe(bReshaped, aIn, aReshaped, bIn)
    }

    import graph._
    withRelease {
      step
      require(readScalar(bReshaped) == readScalar(aIn))
      require(readVector(aReshaped) == readVector(bIn))
    }
  }

  // The behavior of this case changed as of libcog 4.3.  Before that
  // the reshape operator was not true to the statement that the data
  // layout should be unchanged with a reshape.
  test("reshape a 3d scalar field to a 0d vector field and vice versa"){
    val aRef = Array(
      Array(
        Array(0f, 1f, 2f),
        Array(3f, 4f, 5f),
        Array(6f, 7f, 8f),
        Array(9f, 10f, 11f)
      ),
      Array(
        Array(12f, 13f, 14f),
        Array(15f, 16f, 17f),
        Array(18f, 19f, 20f),
        Array(21f, 22f, 23f)
      )
    )
    val bRef = Array(0f,1f,2f,3f,4f,5f,6f,7f,8f,9f,10f,11f,
                    12f,13f,14f,15f,16f,17f,18f,19f,20f,21f,22f,23f)

    val graph = new ComputeGraph(optimize = false) with RefTestInterface {
      val aIn = ScalarField(2,4,3,(l,r,c)=>aRef(l)(r)(c))
      val bIn = VectorField(Vector(24,(i)=>bRef(i)))

      val aReshaped = reshape(aIn, Shape(),Shape(24))
      val bReshaped = reshape(bIn, Shape(2,4,3),Shape())

      probe(bReshaped, aIn, aReshaped, bIn)
    }

    import graph._
    withRelease {
      step
      require(readScalar(bReshaped) == readScalar(aIn))
      require(readVector(aReshaped) == readVector(bIn))
    }
  }

  // The behavior of this case changed as of libcog 4.3.  Before that
  // the reshape operator was not true to the statement that the data
  // layout should be unchanged with a reshape.
  test("reshape a 3d scalar field to a 2d scalar field and vice versa"){
    val aRef = Array(
      Array(
        Array(0f, 1f, 2f),
        Array(3f, 4f, 5f),
        Array(6f, 7f, 8f),
        Array(9f, 10f, 11f)
      ),
      Array(
        Array(12f, 13f, 14f),
        Array(15f, 16f, 17f),
        Array(18f, 19f, 20f),
        Array(21f, 22f, 23f)
      )
    )
    val bRef = Array(
      Array(0f, 1f, 2f),
      Array(3f, 4f, 5f),
      Array(6f, 7f, 8f),
      Array(9f, 10f, 11f),
      Array(12f, 13f, 14f),
      Array(15f, 16f, 17f),
      Array(18f, 19f, 20f),
      Array(21f, 22f, 23f)
    )

    val graph = new ComputeGraph(optimize = false) with RefTestInterface {
      val aIn = ScalarField(2,4,3,(l,r,c)=>aRef(l)(r)(c))
      val bIn = ScalarField(8,3,(r,c)=>bRef(r)(c))

      val aReshaped = reshape(aIn, Shape(8,3),Shape())
      val bReshaped = reshape(bIn, Shape(2,4,3),Shape())

      probe(bReshaped, aIn, aReshaped, bIn)
    }

    import graph._
    withRelease {
      step
      require(readScalar(bReshaped) == readScalar(aIn))
      require(readScalar(aReshaped) == readScalar(bIn))
    }
  }

  test("reshape a 2d scalar field to a different-shape 2d scalar field and vice versa"){
    val aRef = Array(
      Array( 0f,  1f,  2f,  3f,  4f,  5f),
      Array( 6f,  7f,  8f,  9f, 10f, 11f),
      Array(12f, 13f, 14f, 15f, 16f, 17f),
      Array(18f, 19f, 20f, 21f, 22f, 23f)
    )
    val bRef = Array(
      Array(0f, 1f, 2f),
      Array(3f, 4f, 5f),
      Array(6f, 7f, 8f),
      Array(9f, 10f, 11f),
      Array(12f, 13f, 14f),
      Array(15f, 16f, 17f),
      Array(18f, 19f, 20f),
      Array(21f, 22f, 23f)
    )

    val graph = new ComputeGraph(optimize = false) with RefTestInterface {
      val aIn = ScalarField(4,6,(r,c)=>aRef(r)(c))
      val bIn = ScalarField(8,3,(r,c)=>bRef(r)(c))

      val aReshaped = reshape(aIn, Shape(8,3),Shape())
      val bReshaped = reshape(bIn, Shape(4,6),Shape())

      probe(bReshaped, aIn, aReshaped, bIn)
    }

    import graph._
    withRelease {
      step
      require(readScalar(bReshaped) == readScalar(aIn))
      require(readScalar(aReshaped) == readScalar(bIn))
    }
  }

  test("reshape a 2d vector field to a different-fieldshape 2d vector field and vice versa"){
    // Twelve length-2 vectors.  Shown to suggest a 4row x 3column fieldshape
    val aRef = Array(
      Vector( 0f,  1f), Vector( 2f,  3f), Vector( 4f,  5f),
      Vector( 6f,  7f), Vector( 8f,  9f), Vector( 10f, 11f),
      Vector(12f, 13f), Vector( 14f, 15f), Vector( 16f, 17f),
      Vector(18f, 19f), Vector( 20f, 21f), Vector( 22f, 23f)
    )

    val graph = new ComputeGraph(optimize = false) with RefTestInterface {
      // 4 rows x 3 columns
      val aIn = VectorField(4,3,(r,c)=>aRef(r*3 +c))
      // 3 rows x 4 columns
      val bIn = VectorField(3,4,(r,c)=>aRef(r*4 +c))

      val aReshaped = reshape(aIn, Shape(3,4),Shape(2))
      val bReshaped = reshape(bIn, Shape(4,3),Shape(2))

      probe(bReshaped, aIn, aReshaped, bIn)
    }

    import graph._
    withRelease {
      step
      require(readVector(bReshaped) == readVector(aIn))
      require(readVector(aReshaped) == readVector(bIn))
    }
  }

  test("reshape a 2d vector field to a same-fieldshape 2d matrix field and vice versa"){
    // Data for a 2x2 vector field of length 6 vectors
    val aRef = Array(
      Array(Vector( 0f,  1f, 2f,  3f, 4f,  5f),
            Vector( 6f,  7f, 8f,  9f, 10f, 11f)),
      Array(Vector(12f, 13f, 14f, 15f, 16f, 17f),
            Vector(18f, 19f, 20f, 21f, 22f, 23f))
    )
    // Data for a 2x2 matrix field of 2x3 matrices
    val bRef = Array(
      Array(Matrix( Array(0f,  1f, 2f), Array( 3f, 4f,  5f)),
            Matrix( Array(6f,  7f, 8f), Array( 9f, 10f, 11f))),
      Array(Matrix( Array(12f, 13f, 14f), Array(15f, 16f, 17f)),
            Matrix( Array(18f, 19f, 20f), Array(21f, 22f, 23f)))
    )

    val graph = new ComputeGraph(optimize = false) with RefTestInterface {
      // 4 rows x 3 columns
      val aIn = VectorField(2,2,(r,c)=>aRef(r)(c))
      // 3 rows x 4 columns
      val bIn = MatrixField(2,2,(r,c)=>bRef(r)(c))

      val aReshaped = reshape(aIn, Shape(2,2),Shape(2,3))
      val bReshaped = reshape(bIn, Shape(2,2),Shape(6))

      probe(bReshaped, aIn, aReshaped, bIn)
    }

    import graph._
    withRelease {
      step
      require(readVector(bReshaped) == readVector(aIn))
      require(readMatrix(aReshaped) == readMatrix(bIn))
    }
  }

  // The behavior of this case changed as of libcog 4.3.  Before that
  // the reshape operator was not true to the statement that the data
  // layout should be unchanged with a reshape.
  test("reshape a 3d vector field to a different-fieldshape 3d vector field and vice versa"){
    // Twelve length-2 vectors.  Shown to suggest a 3layer x 2row x 2column fieldshape
    val aRef = Array(
      Vector( 0f,  1f), Vector( 2f,  3f),
      Vector( 4f,  5f), Vector( 6f,  7f),

      Vector( 8f,  9f), Vector( 10f, 11f),
      Vector(12f, 13f), Vector( 14f, 15f),

      Vector( 16f, 17f), Vector(18f, 19f),
      Vector( 20f, 21f), Vector( 22f, 23f)
    )

    val graph = new ComputeGraph(optimize = false) with RefTestInterface {
      // 3 layers x 2 rows x 2 columns
      val aIn = VectorField(3,2,2,(l,r,c)=>aRef(l*2*2 + r*2 +c))
      // 2 layers x 6 rows x 1 columns
      val bIn = VectorField(2,6,1,(l,r,c)=>aRef(l*6*1 + r*1 +c))

      val aReshaped = reshape(aIn, Shape(2, 6, 1),Shape(2))
      val bReshaped = reshape(bIn, Shape(3, 2, 2),Shape(2))

      probe(bReshaped, aIn, aReshaped, bIn)
    }

    import graph._
    withRelease {
      step
      require(readVector(bReshaped) == readVector(aIn))
      require(readVector(aReshaped) == readVector(bIn))
    }
  }

  test("reshape a 2d scalar field to a 0d matrix field and vice versa"){
    val aRef = Array(
      Array(0f, 1f, 2f),
      Array(3f, 4f, 5f),
      Array(6f, 7f, 8f),
      Array(9f, 10f, 11f)
    )

    val graph = new ComputeGraph(optimize = false) with RefTestInterface {
      val aIn = ScalarField(4,3,(r,c)=>aRef(r)(c))
      val bIn = MatrixField(Matrix(aRef))

      val aReshaped = reshape(aIn, Shape(),Shape(4,3))
      val bReshaped = reshape(bIn, Shape(4,3),Shape())

      probe(bReshaped, aIn, aReshaped, bIn)
    }

    import graph._
    withRelease {
      step
      require(readScalar(bReshaped) == readScalar(aIn))
      require(readMatrix(aReshaped) == readMatrix(bIn))
    }
  }
}
