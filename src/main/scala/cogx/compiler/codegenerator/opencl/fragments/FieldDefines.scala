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

import cogx.platform.types.{FieldMemoryLayout, FieldType}
import cogx.platform.types.ElementTypes.Complex32

/** Creates a string containing multiple lines of #define statements that
  * define the field parameters for a field.
  *
  * author: Greg Snider
  */
private[cogx]
object FieldDefines {

  /** Use the form "const int foo = 0" over "#define foo 0" */
  val UseConsts = true
  /** Define a constant per the desired method based on `UseConsts` */
  private def define(identifier: String, value: Int) = {
    if (UseConsts)
      "    const int " + identifier + " = " + value + ";\n"
    else
      "#define " + identifier + " " + value + "\n"
  }

  /** Create a declaration for a field in an OpenCL kernel parameter list.
    *
    * @param name The name of the field.
    * @param fieldType Type of the field.
    * @return A string containing #define statements describing the field's
    *         geometric properties.
    */
  def apply(name: String, fieldType: FieldType): String = {
    val fieldDimensions = fieldType.fieldShape.dimensions
    val tensorDimensions = fieldType.tensorShape.dimensions
    val layout = new FieldMemoryLayout(fieldType)
    val buffer = new StringBuffer
    if (name == "")
      buffer.append("    // Work-group-determining field parameters " + fieldType + "\n")
    else
      buffer.append ("    // Field parameters for " + name + " " + fieldType + "\n")
    fieldDimensions match {
      case 3 =>
        buffer.append(define(name + "_layers", layout.layers))
        buffer.append(define(name + "_rows", layout.rows))
        buffer.append(define(name + "_columns", layout.columns))
      case 2 =>
        buffer.append(define(name + "_rows", layout.rows))
        buffer.append(define(name + "_columns", layout.columns))
      case 1 =>
        buffer.append(define(name + "_columns", layout.columns))
      case 0 =>
        // We must coerce 0D fields to 1D for OpenCL
        buffer.append(define(name + "_columns", 1))
    }
    buffer.append(define(name + "_tensorElements", fieldType.tensorShape.points))
    tensorDimensions match {
      case 2 =>
        buffer.append(define(name + "_tensorRows ",
          fieldType.tensorShape(0)))
        buffer.append(define(name + "_tensorColumns ",
          fieldType.tensorShape(1)))
      case 1 =>
        buffer.append(define(name + "_tensorColumns ",
          fieldType.tensorShape(0)))
      case 0 =>
    }
    // Don't need to output strides for the work-group-determining field
    // (name = "") because all indexing is performed using the strides
    // of some specific input field or the output field
    if (name != "") {
      buffer.append(define(name + "_layerStride ",
        layout.rows * layout.fieldRowStride))
      buffer.append(define(name + "_rowStride ",
        layout.fieldRowStride))
      tensorDimensions match {
        case 2 =>
          buffer.append(define(name + "_tensorStride ",
            layout.tensorStride))
        case 1 =>
          buffer.append(define(name + "_tensorStride ",
            layout.tensorStride))
        case 0 =>
            buffer.append(define(name + "_tensorStride", 0))
      }
      fieldType.elementType match {
        case Complex32 =>
          buffer.append(define(name + "_partStride ",
            layout.partStride))
        case _ =>
      }
    }
    buffer.append("\n")
    buffer.toString
  }

  /** Cleanup #define statements made by apply
    *
    * @param name The name of the field.
    * @param fieldType Type of the field.
    * @return A string containing #undef statements.
    */
  def cleanup(name: String, fieldType: FieldType): String = {
    if (UseConsts)
      ""
    else {
      val fieldDimensions = fieldType.fieldShape.dimensions
      val tensorDimensions = fieldType.tensorShape.dimensions
      val buffer = new StringBuffer
      fieldDimensions match {
        case 3 =>
          buffer.append("#undef " + name + "_layers \n")
          buffer.append("#undef " + name + "_rows \n")
          buffer.append("#undef " + name + "_columns \n")
        case 2 =>
          buffer.append("#undef " + name + "_rows \n")
          buffer.append("#undef " + name + "_columns \n")
        case 1 =>
          buffer.append("#undef " + name + "_columns \n")
        case 0 =>
          // We must coerce 0D fields to 1D for OpenCL
          buffer.append("#undef " + name + "_columns \n")

      }
      tensorDimensions match {
        case 2 =>
          buffer.append("#undef " + name + "_tensorRows \n")
          buffer.append("#undef " + name + "_tensorColumns \n")
        case 1 =>
          buffer.append("#undef " + name + "_tensorColumns \n")
        case 0 =>
      }
      buffer.append("#undef " + name + "_tensorElements \n")
      if (name != "") {
        buffer.append("#undef " + name + "_layerStride \n")
        buffer.append("#undef " + name + "_rowStride \n")
        buffer.append("#undef " + name + "_tensorStride \n")
        fieldType.elementType match {
          case Complex32 =>
            buffer.append("#undef " + name + "_partStride \n")
          case _ =>
        }
      }
      buffer.toString
    }
  }
}
