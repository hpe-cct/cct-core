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

package cogx.api

import cogx.compiler.parser.syntaxtree._

import scala.language.implicitConversions
import cogx.cogmath.algebra.complex.{ComplexVector, Complex}
import cogx.compiler.parser.op.RealToComplexOp
import cogx.compiler.codegenerator.common.FieldPolicies._
import cogx.cogmath.algebra.real.Vector

/** Implicit conversions needed to emulate static typing in Cog.
  *
  * @author Greg Snider
  */

trait ImplicitConversions {
  /** Implicit conversion of a Float to a CogFloat which allows for fields
    * to be combined with floating point numbers in a simple way.
    * This makes it possible to express commutative operations with a common
    * syntax.
    *
    * For example
    * {{{
    *    Field + Float
    * }}}
    * and
    * {{{
    *    Float + Field
    * }}}
    * are both legal and compilable with this implicit conversion.
    *
    * @param f The Float to be converted to a CogFloat
    * @return The CogFloat created from `f`.
    */
  implicit def floatToCogFloat(f: Float): CogFloat = new CogFloat(f)

  /** Implicit conversion of a Vector to a 0D constant VectorField which allows
    * for fields to be combined with vectors in a simple way.
    * This makes it possible to express commutative operations with a common
    * syntax.
    *
    * For example
    * {{{
    *    Field + Vector
    * }}}
    * and
    * {{{
    *    Vector + Field
    * }}}
    * are both legal and compilable with this implicit conversion.
    *
    * @param v The vector to be converted to a 0D vector field.
    * @return A 0D vector field initialized to `v`.
    */
  implicit def vectorTo0DVectorField(v: Vector) = VectorField(v)

  /** Implicit conversion of a Complex to a CogComplex which allows for fields
    * to be combined with floating point numbers in a simple way.
    * This makes it possible to express commutative operations with a common
    * syntax.
    *
    * For example
    * {{{
    *    Field + Complex
    * }}}
    * and
    * {{{
    *    Complex + Field
    * }}}
    * are both legal and compilable with this implicit conversion.
    *
    * @param c The Complex to be converted to a CogComplex
    * @return The CogComplex created from `c`.
    */
  implicit def complexToCogComplex(c: Complex): CogComplex = new CogComplex(c)


  /** Implicit conversion of a ComplexVector to a 0D constant ComplexVectorField
    * which allows for fields to be combined with ComplexVectors in a simple way.
    * This makes it possible to express commutative operations with a common
    * syntax.
    *
    * For example
    * {{{
    *    Field + ComplexVector
    * }}}
    * and
    * {{{
    *    ComplexVector + Field
    * }}}
    * are both legal and compilable with this implicit conversion.
    *
    * @param v The complex vector to be converted to a 0D complex vector field.
    * @return A 0D complex vector field initialized to `v`.
    */
  implicit def complexVectorTo0DComplexVectorField(v: ComplexVector) =
    ComplexVectorField(v)

  /** Implicit conversion of a Field to a ScalarField. Since Field is abstract,
    * the only possible conversion is if the field is already a ScalarField but
    * is stored in a Field variable. This merely does the necessary type
    * coercion.
    *
    * @param f A field which needs coercion to the ScalarField type.
    * @return The converted field.
    */
  implicit def fieldToScalarField(f: Field): ScalarField =
    f.asInstanceOf[ScalarField]

  /** Implicit conversion of a Field to a VectorField. Field is abstract, so
    * this method will perform the appropriate type coercion if the field is
    * already a VectorField but stored in a Field variable. This method also
    * supports converting a ColorField to a VectorField.
    *
    * @param f A field which needs coercion to the VectorField type.
    * @return The converted field.
    */
  implicit def fieldToVectorField(f: Field): VectorField = {
    val fieldType = f.fieldType
    val guaranteedVector =
      if (isRealField(fieldType) && fieldType.tensorOrder == 1)
        f
      //
      // The following conversion (which I explictly requested NOT to be put in,
      // but was put in anyway) is a very bad idea. For a description of why
      // it's a bad idea, and how you should code it instead, see the comments
      // below in the fieldToColorField method.
      //
      //else if (isColorField(fieldType))
      //  Field.vectorField(f.red, f.green, f.blue)
      else
        throw new RuntimeException("Conversion to VectorField not possible from " + f.fieldType)
    guaranteedVector.asInstanceOf[VectorField]
  }

  /** Implicit conversion of a Field to a MatrixField. Since Field is abstract,
    * the only possible conversion is if the field is already a MatrixField but
    * is stored in a Field variable. This merely does the necessary type
    * coercion.
    *
    * @param f A field which needs coercion to the MatrixField type.
    * @return The converted field.
    */
  implicit def fieldToMatrixField(f: Field): MatrixField =
    f.asInstanceOf[MatrixField]

  /** Implicit conversion of a Field to a ComplexField. Field is abstract, so
    * this method will perform the appropriate type coercion if the field is
    * already a ComplexField but stored in a Field variable. This method also
    * supports converting a ScalarField to a ComplexField.
    *
    * @param f A field which needs coercion to the ComplexField type.
    * @return The converted field.
    */
  implicit def fieldToComplexField(f: Field): ComplexField = {
    val guaranteedComplex =
      if (isComplexField(f.fieldType))
        f
      else if (isRealField(f.fieldType))
        UnaryOperator(RealToComplexOp, f)
      else
        throw new RuntimeException("Conversion to ComplexField not possible from " + f.fieldType)
    guaranteedComplex.asInstanceOf[ComplexField]
  }

  /** Implicit conversion of a Field to a ComplexField. Field is abstract, so
    * this method will perform the appropriate type coercion if the field is
    * already a ComplexField but stored in a Field variable. This method also
    * supports converting a ScalarField to a ComplexField.
    *
    * @param f A field which needs coercion to the ComplexVectorField type.
    * @return The converted field.
    */
  implicit def fieldToComplexVectorField(f: Field): ComplexVectorField = {
    val guaranteedComplex =
      if (isComplexField(f.fieldType) && f.fieldType.tensorOrder == 1)
        f
      else if (isRealField(f.fieldType) && f.fieldType.tensorOrder == 1)
        UnaryOperator(RealToComplexOp, f)
      else
        throw new RuntimeException("Conversion to ComplexVectorField not possible from " + f.fieldType)
    guaranteedComplex.asInstanceOf[ComplexVectorField]
  }

  /** Implicit conversion of a Field to a ColorField. Field is abstract, so
    * this method will perform the appropriate type coercion if the field is
    * already a ColorField but stored in a Field variable. This method also
    * supports converting a ScalarField or VectorField to a ColorField.
    *
    * @param f A field which needs coercion to the ColorField type.
    * @return The converted field.
    */
 implicit def fieldToColorField(f: Field): ColorField = {
    val fieldType = f.fieldType
    val guaranteedColor =
      if (isColorField(fieldType))
        f
      else if (isRealField(fieldType) && fieldType.dimensions == 2 && fieldType.tensorOrder == 0)
        Field.colorField(f)
      //
      // The following conversion is a very bad idea. It creates a lot of extra
      // kernels that slow performance down...a lot. Implicit conversions can
      // be useful between closely related types. But color fields and vector
      // fields are NOT closely related, and users must be put in charge of
      // those conversions so they can make informed design trade-offs when
      // designing algorithms.
      //
      // If you really need to convert between color fields and vectors fields,
      // do it explicitly:
      //
      //      colorField.toVectorField
      //      vectorField.toColorField
      //
      // This allows the compiler to generate the most efficient code.
      //
      //else if (isRealField(fieldType) && fieldType.dimensions == 2 && fieldType.tensorShape == Shape(3))
      //  Field.colorField(f.vectorElement(0), f.vectorElement(1), f.vectorElement(2))
      //
      else
        throw new RuntimeException("Conversion to ColorField not possible from " + f.fieldType)

    guaranteedColor.asInstanceOf[ColorField]
  }

  /** Implicit conversion of an array of Fields to an array of ScalarFields.
    *
    * @param a An array of Fields.
    * @return An array of ScalarFields.
    */
  implicit def fieldArrayToScalarFieldArray(a: Array[Field]): Array[ScalarField] = {
    a.map(_.asInstanceOf[ScalarField])
  }

  /** Implicit conversion of an array of Fields to an array of ScalarFields.
    *
    * @param a An array of Fields.
    * @return An array of VectorFields.
    */
  implicit def fieldArrayToVectorFieldArray(a: Array[Field]): Array[VectorField] = {
    a.map(_.asInstanceOf[VectorField])
  }

  /** Implicit conversion of an array of Fields to an array of ScalarFields.
    *
    * @param a An array of Fields.
    * @return An array of MatrixFields.
    */
  implicit def fieldArrayToMatrixFieldArray(a: Array[Field]): Array[MatrixField] = {
    a.map(_.asInstanceOf[MatrixField])
  }
}