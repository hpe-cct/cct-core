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

package cogx.compiler.codegenerator.opencl.fragments

import cogx.platform.types.{FieldMemoryLayoutImpl, FieldType}
import cogx.platform.types.ElementTypes._

/** Translates a read or write request in a user's hyperkernel code to
  * an executable OpenCL code string.
  *
  * This takes care of all the address calculations and generates correct
  * code for both the Image API and regular API (so you don't have to worry
  * about OpenCL's schizophrenia). It is the caller's responsibility to ensure
  * that the read and write calls are semantically correct, this module just
  * generates code strings.
  *
  * @author Greg Snider
  */
private[cogx]
object FieldIO {

  /** Read a float value from an input field. Note that this may only be called
    * on non-image fields and non-tensor fields.
    *
    * @param fieldType FieldType of the field to be read.
    * @param fieldName Name of the field to be read.
    * @param fieldLocal True implies (_layer, _row, _column) addressing, false
    *        implies the nonlocal (layer, row, column) addressing.
    * @param tensorLocal True implices (_tensorIndex) addressing, false implies
    *        nonlocal (tensorIndex) addressing.
    * @return A string expression that evaluates to a float read from the field
    */
  def readElement(fieldType: FieldType, fieldName: String, fieldLocal: Boolean,
                  tensorLocal: Boolean): String =
  {
    fieldType.elementType match {
      case Float32 =>
        fieldName + "[" + fieldOffset(fieldType, fieldName, fieldLocal) +
                tensorOffset(fieldType, fieldName, tensorLocal) + "]"
      case Complex32 =>
        "(float2) (" +
                fieldName + "[" + fieldOffset(fieldType, fieldName, fieldLocal) +
                tensorOffset(fieldType, fieldName, tensorLocal) + "]" + "," +
                fieldName + "[" + fieldOffset(fieldType, fieldName, fieldLocal) +
                tensorOffset(fieldType, fieldName, tensorLocal) +
                " + " + fieldName + "_partStride" + "]" + ")"
      case x =>
        throw new RuntimeException("FieldIO.readElement not supported " +
              "for " + x)
    }
  }

  /** Read a compact tensor value from an input field. Compact tensor types
    * are CLFloat, CLFloat2, CLFloat3, CLFloat4, CLPixel.
    *
    * @param fieldType FieldType of the field to be read.
    * @param fieldName Name of the field to be read.
    * @param tensorType CLType (e.g. float, float2) of the field to be read.
    * @param fieldLocal True implies (_layer, _row, _column) addressing, false
    *        implies the nonlocal (layer, row, column) addressing.
    * @return A string expression that evaluates to a float read from the field
    */
  def readTensor(fieldType: FieldType, fieldName: String, tensorType: CLType, fieldLocal: Boolean): String =
  {
    val baseIndex = fieldOffset(fieldType, fieldName, fieldLocal)
    val string = tensorType match {
      case CLFloat =>
        fieldName + "[" + baseIndex + "]"
      case CLComplex =>
        "(float2)(" + fieldName + "[" + baseIndex + "], " +
                "" + fieldName + "[" + baseIndex + " + %name%_partStride] )"
      case CLFloat2 =>
        "(float2)(" + fieldName + "[" + baseIndex + "], " +
                "" + fieldName + "[" + baseIndex + " + %name%_tensorStride] )"
      case CLFloat3 =>
        "(float3)(" + fieldName + "[" + baseIndex + "], " +
                "" + fieldName + "[" + baseIndex + " + %name%_tensorStride], " +
                "" + fieldName + "[" + baseIndex + " + 2 * %name%_tensorStride] )"
      case CLFloat4 =>
        "(float4)(" + fieldName + "[" + baseIndex + "], " +
                "" + fieldName + "[" + baseIndex + " + %name%_tensorStride], " +
                "" + fieldName + "[" + baseIndex + " + 2 * %name%_tensorStride], " +
                "" + fieldName + "[" + baseIndex + " + 3 * %name%_tensorStride] )"
      case CLFloat8 =>
        "(float8)(" + fieldName + "[" + baseIndex + "], " +
                "" + fieldName + "[" + baseIndex + " + %name%_tensorStride], " +
                "" + fieldName + "[" + baseIndex + " + 2 * %name%_tensorStride], " +
                "" + fieldName + "[" + baseIndex + " + 3 * %name%_tensorStride], " +
                "" + fieldName + "[" + baseIndex + " + 4 * %name%_tensorStride], " +
                "" + fieldName + "[" + baseIndex + " + 5 * %name%_tensorStride], " +
                "" + fieldName + "[" + baseIndex + " + 6 * %name%_tensorStride], " +
                "" + fieldName + "[" + baseIndex + " + 7 * %name%_tensorStride] )"
      case CLFloat16 =>
        "(float16)(" + fieldName + "[" + baseIndex + "], " +
                "" + fieldName + "[" + baseIndex + " + %name%_tensorStride], " +
                "" + fieldName + "[" + baseIndex + " + 2 * %name%_tensorStride], " +
                "" + fieldName + "[" + baseIndex + " + 3 * %name%_tensorStride], " +
                "" + fieldName + "[" + baseIndex + " + 4 * %name%_tensorStride], " +
                "" + fieldName + "[" + baseIndex + " + 5 * %name%_tensorStride], " +
                "" + fieldName + "[" + baseIndex + " + 6 * %name%_tensorStride], " +
                "" + fieldName + "[" + baseIndex + " + 7 * %name%_tensorStride], " +
                "" + fieldName + "[" + baseIndex + " + 8 * %name%_tensorStride], " +
                "" + fieldName + "[" + baseIndex + " + 9 * %name%_tensorStride], " +
                "" + fieldName + "[" + baseIndex + " + 10 * %name%_tensorStride], " +
                "" + fieldName + "[" + baseIndex + " + 11 * %name%_tensorStride], " +
                "" + fieldName + "[" + baseIndex + " + 12 * %name%_tensorStride], " +
                "" + fieldName + "[" + baseIndex + " + 13 * %name%_tensorStride], " +
                "" + fieldName + "[" + baseIndex + " + 14 * %name%_tensorStride], " +
                "" + fieldName + "[" + baseIndex + " + 15 * %name%_tensorStride] )"
      case CLPixel =>
        // Images use (x, y) coordinates rather than (row, column), or
        // (x, y, z) rather than (layer, row, column) for 3D images.
        // Support here for reading of 3d images is somewhat worthless, since
        // NVidia (and others?) don't support the OpenCL extension to do so.
        // This was put in to avoid special-case checks in the SliceHyperKernel or
        // the ColorFieldGenerator to prevent slicing of 3d images.
        if (fieldLocal)
          """
#ifdef %name%_layers
    read_imagef(%name%, sampler, (int4)(_column, _row, _layer, 0))
#else
    read_imagef(%name%, sampler, (int2)(_column, _row))
#endif
          """
        else
          """
#ifdef %name%_layers
    read_imagef(%name%, sampler, (int4)(column, row, layer, 0))
#else
    read_imagef(%name%, sampler, (int2)(column, row))
#endif
          """
      case x =>
        throw new RuntimeException("FieldIndex.readIndexed not supported " +
                "for " + tensorType)
    }
    string.replaceAll("%name%", fieldName)
  }


  /** Write a float value to the output field. Note that this may only be called
    * on non-image fields.
    *
    * @param fieldType FieldType of the field to be read.
    * @param fieldName Name of the field to be read.
    * @param fieldLocal True implies (_layer, _row, _column) addressing, false
    *        implies the nonlocal (layer, row, column) addressing.
    * @param tensorLocal True implices (_tensorIndex) addressing, false implies
    *        nonlocal (tensorIndex) addressing.
    * @param value The value to be written to the output field.
    * @return A string containing an OpenCL statement that writes the value
    *        to the output field.
    */
  def writeElement(fieldType: FieldType, fieldName: String, fieldLocal: Boolean,
                   tensorLocal: Boolean, value: String): String =
  {
    fieldType.elementType match {
      case Float32 =>
        val reference = fieldName + "[" + fieldOffset(fieldType, fieldName, fieldLocal) +
                tensorOffset(fieldType, fieldName, tensorLocal) + "]"
        "    " + reference + " = " + value + ";\n"
      case Complex32 =>
        val realPartReference =
          fieldName + "[" + fieldOffset(fieldType, fieldName, fieldLocal) +
          tensorOffset(fieldType, fieldName, tensorLocal) + "]"
        val imaginaryPartReference =
          fieldName + "[" + fieldOffset(fieldType, fieldName, fieldLocal) +
          tensorOffset(fieldType, fieldName, tensorLocal) +
          " + " + fieldName + "_partStride" + "]"
        "    " + realPartReference + " = " + value + ".x;\n" +
        "    " + imaginaryPartReference + " = " + value + ".y;\n"
      case x =>
        throw new RuntimeException("FieldIO.readElement not supported " +
                "for " + x)
    }
  }

  /** Compute a pointer into the (non-image) output field.
    *
    * @param fieldType FieldType of the field to be read.
    * @param fieldName Name of the field to be read.
    * @param fieldLocal True implies (_layer, _row, _column) addressing, false
    *        implies the nonlocal (layer, row, column) addressing.
    * @param tensorLocal True implices (_tensorIndex) addressing, false implies
    *        nonlocal (tensorIndex) addressing.
    * @return A string containing an OpenCL statement that writes the value
    *        to the output field.
    */
  def writePointer(fieldType: FieldType, fieldName: String,  fieldLocal: Boolean, tensorLocal: Boolean): String =
  {
    fieldName + "[" + fieldOffset(fieldType, fieldName, fieldLocal) +
            tensorOffset(fieldType, fieldName, tensorLocal) + "]"
  }

  /** Compute a pointer into the (non-image) output Tensor0 field (no _tensorElement or tensorElement used).
    *
    * @param fieldType FieldType of the field to be read.
    * @param fieldName Name of the field to be read.
    * @param fieldLocal True implies (_layer, _row, _column) addressing, false
    *        implies the nonlocal (layer, row, column) addressing.
    * @return A string containing an OpenCL statement that writes the value
    *        to the output field.
    */
  def writeTensor0FieldPointer(fieldType: FieldType, fieldName: String,  fieldLocal: Boolean): String =
  {
    fieldName + "[" + fieldOffset(fieldType, fieldName, fieldLocal) + "]"
  }

  /** Write a compact tensor value to the output field. Compact tensor types
    * are CLFloat, CLFloat2, CLFloat3, CLFloat4.
    *
    * @param fieldType FieldType of the field to be written.
    * @param fieldName Name of the field to be read.
    * @param tensorType CLType (e.g. float, float2) of the field to be written.
    * @param fieldLocal True implies (_layer, _row, _column) addressing, false
    *        implies the nonlocal (layer, row, column) addressing.
    * @param result The tensor value to be written to the output field.
    * @return A string containing an OpenCL statement that writes the value
    *        to the output field.
    */
  def writeTensor(fieldType: FieldType, fieldName: String,  tensorType: CLType, fieldLocal: Boolean, result: String): String =
  {
    val baseIndex = fieldOffset(fieldType, fieldName, fieldLocal)
    tensorType match {
      case CLFloat =>
        "    " + fieldName + "[" + baseIndex + "] = " + result + ";\n"
      case CLComplex =>
        "    " + fieldName + "[" + baseIndex + "] = " + result + ".x;\n" +
        "    " + fieldName + "[" + baseIndex + " + " + fieldName + "_partStride] = " + result + ".y;\n"
      case CLFloat2 =>
        "    " + fieldName + "[" + baseIndex + "] = " + result + ".x;\n" +
                "    " + fieldName + "[" + baseIndex + " + " + fieldName + "_tensorStride] = " + result + ".y;\n"
      case CLFloat3 =>
        "    " + fieldName + "[" + baseIndex + "] = " + result + ".x;\n" +
                "    " + fieldName + "[" + baseIndex + " + " + fieldName + "_tensorStride] = " + result + ".y;\n" +
                "    " + fieldName + "[" + baseIndex + " + 2 * " + fieldName + "_tensorStride] = " + result + ".z;\n"
      case CLFloat4 =>
        "    " + fieldName + "[" + baseIndex + "] = " + result + ".x;\n" +
                "    " + fieldName + "[" + baseIndex + " + " + fieldName + "_tensorStride] = " + result + ".y;\n" +
                "    " + fieldName + "[" + baseIndex + " + 2 * " + fieldName + "_tensorStride] = " + result + ".z;\n" +
                "    " + fieldName + "[" + baseIndex + " + 3 * " + fieldName + "_tensorStride] = " + result + ".w;\n"
      case CLFloat8 =>
        "    " + fieldName + "[" + baseIndex + "] = " + result + ".s0;\n" +
                "    " + fieldName + "[" + baseIndex + " + " + fieldName + "_tensorStride] = " + result + ".s1;\n" +
                "    " + fieldName + "[" + baseIndex + " + 2 * " + fieldName + "_tensorStride] = " + result + ".s2;\n" +
                "    " + fieldName + "[" + baseIndex + " + 3 * " + fieldName + "_tensorStride] = " + result + ".s3;\n" +
                "    " + fieldName + "[" + baseIndex + " + 4 * " + fieldName + "_tensorStride] = " + result + ".s4;\n" +
                "    " + fieldName + "[" + baseIndex + " + 5 * " + fieldName + "_tensorStride] = " + result + ".s5;\n" +
                "    " + fieldName + "[" + baseIndex + " + 6 * " + fieldName + "_tensorStride] = " + result + ".s6;\n" +
                "    " + fieldName + "[" + baseIndex + " + 7 * " + fieldName + "_tensorStride] = " + result + ".s7;\n"
      case CLFloat16 =>
        "    " + fieldName + "[" + baseIndex + "] = " + result + ".s0;\n" +
                "    " + fieldName + "[" + baseIndex + " + " + fieldName + "_tensorStride] = " + result + ".s1;\n" +
                "    " + fieldName + "[" + baseIndex + " + 2 * " + fieldName + "_tensorStride] = " + result + ".s2;\n" +
                "    " + fieldName + "[" + baseIndex + " + 3 * " + fieldName + "_tensorStride] = " + result + ".s3;\n" +
                "    " + fieldName + "[" + baseIndex + " + 4 * " + fieldName + "_tensorStride] = " + result + ".s4;\n" +
                "    " + fieldName + "[" + baseIndex + " + 5 * " + fieldName + "_tensorStride] = " + result + ".s5;\n" +
                "    " + fieldName + "[" + baseIndex + " + 6 * " + fieldName + "_tensorStride] = " + result + ".s6;\n" +
                "    " + fieldName + "[" + baseIndex + " + 7 * " + fieldName + "_tensorStride] = " + result + ".s7;\n" +
                "    " + fieldName + "[" + baseIndex + " + 8 * " + fieldName + "_tensorStride] = " + result + ".s8;\n" +
                "    " + fieldName + "[" + baseIndex + " + 9 * " + fieldName + "_tensorStride] = " + result + ".s9;\n" +
                "    " + fieldName + "[" + baseIndex + " + 10 * " + fieldName + "_tensorStride] = " + result + ".sA;\n" +
                "    " + fieldName + "[" + baseIndex + " + 11 * " + fieldName + "_tensorStride] = " + result + ".sB;\n" +
                "    " + fieldName + "[" + baseIndex + " + 12 * " + fieldName + "_tensorStride] = " + result + ".sC;\n" +
                "    " + fieldName + "[" + baseIndex + " + 13 * " + fieldName + "_tensorStride] = " + result + ".sD;\n" +
                "    " + fieldName + "[" + baseIndex + " + 14 * " + fieldName + "_tensorStride] = " + result + ".sE;\n" +
                "    " + fieldName + "[" + baseIndex + " + 15 * " + fieldName + "_tensorStride] = " + result + ".sF;\n"
      case CLPixel =>
        if (fieldLocal) {
          "    // Must set alpha channel to 1 (not visible to Cog apps).\n" +
            "    " + result + ".w = 1.0f;\n" +
            "    write_imagef(" + fieldName + ", (int2)(_column, _row), " + result + ");\n"
        } else {
          "    // Must set alpha channel to 1 (not visible to Cog apps).\n" +
            "    " + result + ".w = 1.0f;\n" +
            "    write_imagef(" + fieldName + ", (int2)(column, row), " + result + ");\n"
        }
      case x =>
        throw new RuntimeException("FieldFragment.write not supported " +
                "for " + tensorType)
    }  }


  /** Pointer calculation to determine offset in a field buffer for a given
    * tensor field.
    *
    * @param fieldType FieldType of the field to be accessed.
    * @param fieldName Name of the field.
    * @param local True implies (_layer, _row, _column) addressing, false
    *        implies the nonlocal (layer, row, column) addressing.
    * @return A string containing the pointer calculation for the starting
    *        offset of the addressed tensor in the field.
    */
  private def fieldOffset(fieldType: FieldType, fieldName: String, local: Boolean): String = {
    // We don't know the type of the field being written, although we can
    // assume it's not an image. We let the OpenCL compiler figure this out.
    val offset = new StringBuffer
    val layout = new FieldMemoryLayoutImpl(fieldType)
    val dimensions = fieldType.dimensions

    if (dimensions >= 3 && layout.layers > 1)
      offset append "%local%layer * %field%_layerStride + "
    if (dimensions >= 2 && layout.rows > 1)
      offset append "%local%row * %field%_rowStride + "
    if (dimensions >= 1 && layout.columns > 1)
      offset append "%local%column"
    else
      offset append "0"
    val localPrefix = if (local) "_" else ""
    offset.toString.replaceAll("%field%", fieldName).replaceAll("%local%", localPrefix)
  }

  /** Pointer calculation to determine offset of a tensor element from the
    * starting offset for the tensor element's field address. This may be
    * appended to `fieldOffset` to have a complete offset into a field buffer.
    *
    * @param fieldType FieldType of the field to be accessed.
    * @param fieldName Name of the field.
    * @param local True implies (_tensorElement) addressing, false
    *        implies the nonlocal (tensorElement) addressing.
    * @return A string containing the pointer calculation for the starting
    *        offset of the addressed tensor in the field.
    */
  private def tensorOffset(fieldType: FieldType, fieldName: String, local: Boolean): String = {
    val offset = " + %local%tensorElement * %field%_tensorStride"
    val localPrefix = if (local) "_" else ""
    offset.toString.replaceAll("%field%", fieldName).replaceAll("%local%", localPrefix)
  }

}