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

package cogx.compiler.codegenerator.opencl.cpukernels

import cogx.cogmath.algebra.complex.{Complex, ComplexVector}
import cogx.compiler.parser.op._
import cogx.platform.checkpoint.{ObjectRestorer, ObjectSaver}
import cogx.platform.cpumemory._
import cogx.platform.cpumemory.readerwriter.FieldReader
import cogx.platform.opencl.OpenCLFieldRegister
import cogx.platform.types.ElementTypes.{Complex32, Float32, Uint8Pixel}
import cogx.platform.types.{Pixel, Opcode, FieldType}
import cogx.cogmath.algebra.real.{Matrix, Vector}

/** Methods that are common to ConstantFieldKernel and RecurrenceKernel
  *
 * @author Dick Carter
 */
trait ConstantHelper {

  /** Initialize the constant field in the output buffer for this kernel. */
  private[cpukernels]
  def reset(register: OpenCLFieldRegister, fieldType: FieldType, opcode: Opcode): Unit = {
    val outputBuffer = register.master
    fieldType.elementType match {
      case Uint8Pixel =>
        val cpuMemory = outputBuffer.cpuMemory.asInstanceOf[ColorFieldMemory]
        cpuMemory.init(opcode.asInstanceOf[ConstantColorOp].value)
      case Float32 =>
        fieldType.tensorShape.dimensions match {
          case 0 =>
            val cpuMemory = outputBuffer.cpuMemory.asInstanceOf[ScalarFieldMemory]
            fieldType.fieldShape.dimensions match {
              case 0 =>
                cpuMemory.init(opcode.asInstanceOf[ConstantScalar0DOp].value)
              case 1 =>
                cpuMemory.init(opcode.asInstanceOf[ConstantScalar1DOp].value)
              case 2 =>
                cpuMemory.init(opcode.asInstanceOf[ConstantScalar2DOp].value)
              case 3 =>
                cpuMemory.init(opcode.asInstanceOf[ConstantScalar3DOp].value)
            }
          case 1 =>
            val cpuMemory = outputBuffer.cpuMemory.asInstanceOf[VectorFieldMemory]
            fieldType.fieldShape.dimensions match {
              case 0 =>
                cpuMemory.init(opcode.asInstanceOf[ConstantVector0DOp].value)
              case 1 =>
                cpuMemory.init(opcode.asInstanceOf[ConstantVector1DOp].value)
              case 2 =>
                cpuMemory.init(opcode.asInstanceOf[ConstantVector2DOp].value)
              case 3 =>
                cpuMemory.init(opcode.asInstanceOf[ConstantVector3DOp].value)
            }
          case 2 =>
            val cpuMemory = outputBuffer.cpuMemory.asInstanceOf[MatrixFieldMemory]
            fieldType.fieldShape.dimensions match {
              case 0 =>
                cpuMemory.init(opcode.asInstanceOf[ConstantMatrix0DOp].value)
              case 1 =>
                cpuMemory.init(opcode.asInstanceOf[ConstantMatrix1DOp].value)
              case 2 =>
                cpuMemory.init(opcode.asInstanceOf[ConstantMatrix2DOp].value)
              case 3 =>
                cpuMemory.init(opcode.asInstanceOf[ConstantMatrix3DOp].value)
            }
          case x =>
            throw new RuntimeException("not implemented yet for tensor order " + x)
        }
      case Complex32 =>
        fieldType.tensorShape.dimensions match {
          case 0 =>
            val cpuMemory = outputBuffer.cpuMemory.asInstanceOf[ComplexFieldMemory]
            fieldType.fieldShape.dimensions match {
              case 0 =>
                cpuMemory.init(opcode.asInstanceOf[ConstantComplex0DOp].value)
              case 1 =>
                cpuMemory.init(opcode.asInstanceOf[ConstantComplex1DOp].value)
              case 2 =>
                cpuMemory.init(opcode.asInstanceOf[ConstantComplex2DOp].value)
              case 3 =>
                cpuMemory.init(opcode.asInstanceOf[ConstantComplex3DOp].value)
            }
          case 1 =>
            val cpuMemory = outputBuffer.cpuMemory.asInstanceOf[ComplexVectorFieldMemory]
            fieldType.fieldShape.dimensions match {
              case 0 =>
                cpuMemory.init(opcode.asInstanceOf[ConstantComplexVector0DOp].value)
              case 1 =>
                cpuMemory.init(opcode.asInstanceOf[ConstantComplexVector1DOp].value)
              case 2 =>
                cpuMemory.init(opcode.asInstanceOf[ConstantComplexVector2DOp].value)
              case 3 =>
                cpuMemory.init(opcode.asInstanceOf[ConstantComplexVector3DOp].value)
            }
          case x =>
            throw new RuntimeException("not implemented yet for tensor order " + x)
        }
      case x =>
        throw new RuntimeException("not implemented for element type: " + x)
    }
    // At this point the initial data has been written to the CPU portion of
    // buffer. Force it to be copied to the GPU.
    outputBuffer.write
  }

  /** Save the current state of this field (as read by the fieldReader) using the 'saver'. */
  private[cpukernels]
  def saveData(saver: ObjectSaver, fieldReader: FieldReader, fieldType: FieldType, opcode: Opcode): Unit = {
    // Initial check for NullOp covers the case of a pipeline register associated with a pipelined sensor.
    // These registers do not need to save their current state.  A restore of the compute graph will re-ask
    // the sensor for this data.  If the sensor is playing out images from a file, the correct image will be
    // re-read.  If the sensor is a live video feed, the current state of the pipeline register will be lost,
    // and will be replaced by the live image at the time of the restore.
    if (opcode == NullOp) {
      fieldType.elementType match {
        case Uint8Pixel =>
          saver.writeIntArray("data", Array[Int]())
        case Float32 =>
          saver.writeFloatArray("data", Array[Float]())
        case Complex32 =>
          saver.writeFloatArray("data", Array[Float]())
        case x =>
          throw new RuntimeException("not implemented for element type: " + x)
      }
    }
    else {
      fieldType.elementType match {
        case Uint8Pixel =>
          val cpuMemory = fieldReader.asInstanceOf[ColorFieldMemory]
          require(fieldType.dimensions == 2 && fieldType.tensorShape.points == 3,
            "Expecting 2D ColorField, found " + fieldType)
          val arrayLength = fieldType.fieldShape.points
          val it = cpuMemory.iterator
          val data = Array.tabulate(arrayLength) { i =>
            require(it.hasNext, "Compiler internal error: unexpected length of ColorField data.")
            val red: Int = it.next() & 0xff
            val green: Int = it.next() & 0xff
            val blue: Int = it.next() & 0xff
            val packedInt = (red << 16) | (green << 8) | blue
            packedInt
          }
          require(!it.hasNext, "Compiler internal error: unexpected length of ColorField data.")
          saver.writeIntArray("data", data)
        case Float32 =>
          fieldType.tensorShape.dimensions match {
            case 0 =>
              val cpuMemory = fieldReader.asInstanceOf[ScalarFieldMemory]
              val arrayLength = fieldType.fieldShape.points
              val it = cpuMemory.iterator
              val data = Array.tabulate(arrayLength) { i =>
                require(it.hasNext, "Compiler internal error: unexpected length of ScalarField data.")
                it.next()
              }
              require(!it.hasNext, "Compiler internal error: unexpected length of ScalarField data.")
              saver.writeFloatArray("data", data)
            case 1 =>
              val cpuMemory = fieldReader.asInstanceOf[VectorFieldMemory]
              val vectorLength = fieldType.tensorShape.points
              val arrayLength = fieldType.fieldShape.points * vectorLength
              val data = new Array[Float](arrayLength)
              val v = new Vector(vectorLength)
              var outIndex = 0
              fieldType.fieldShape.dimensions match {
                case 0 =>
                  cpuMemory.read(v)
                  Array.copy(v.asArray, 0, data, 0, vectorLength)
                case 1 =>
                  for (col <- 0 until fieldType.columns) {
                    cpuMemory.read(col, v)
                    Array.copy(v.asArray, 0, data, outIndex, vectorLength)
                    outIndex += vectorLength
                  }
                case 2 =>
                  for (row <- 0 until fieldType.rows) {
                    for (col <- 0 until fieldType.columns) {
                      cpuMemory.read(row, col, v)
                      Array.copy(v.asArray, 0, data, outIndex, vectorLength)
                      outIndex += vectorLength
                    }
                  }
                case 3 =>
                  for (layer <- 0 until fieldType.layers) {
                    for (row <- 0 until fieldType.rows) {
                      for (col <- 0 until fieldType.columns) {
                        cpuMemory.read(layer, row, col, v)
                        Array.copy(v.asArray, 0, data, outIndex, vectorLength)
                        outIndex += vectorLength
                      }
                    }
                  }
              }
              saver.writeFloatArray("data", data)
            case 2 =>
              val cpuMemory = fieldReader.asInstanceOf[MatrixFieldMemory]
              val matrixSize = fieldType.tensorShape.points
              val arrayLength = fieldType.fieldShape.points * matrixSize
              val data = new Array[Float](arrayLength)
              val m = new Matrix(fieldType.tensorRows, fieldType.tensorColumns)
              var outIndex = 0
              fieldType.fieldShape.dimensions match {
                case 0 =>
                  cpuMemory.read(m)
                  Array.copy(m.asArray, 0, data, 0, matrixSize)
                case 1 =>
                  for (col <- 0 until fieldType.columns) {
                    cpuMemory.read(col, m)
                    Array.copy(m.asArray, 0, data, outIndex, matrixSize)
                    outIndex += matrixSize
                  }
                case 2 =>
                  for (row <- 0 until fieldType.rows) {
                    for (col <- 0 until fieldType.columns) {
                      cpuMemory.read(row, col, m)
                      Array.copy(m.asArray, 0, data, outIndex, matrixSize)
                      outIndex += matrixSize
                    }
                  }
                case 3 =>
                  for (layer <- 0 until fieldType.layers) {
                    for (row <- 0 until fieldType.rows) {
                      for (col <- 0 until fieldType.columns) {
                        cpuMemory.read(layer, row, col, m)
                        Array.copy(m.asArray, 0, data, outIndex, matrixSize)
                        outIndex += matrixSize
                      }
                    }
                  }
              }
              saver.writeFloatArray("data", data)
            case x =>
              throw new RuntimeException("not implemented yet for tensor order " + x)
          }
        case Complex32 =>
          fieldType.tensorShape.dimensions match {
            case 0 =>
              val cpuMemory = fieldReader.asInstanceOf[ComplexFieldMemory]
              val arrayLength = fieldType.fieldShape.points * 2
              val data = new Array[Float](arrayLength)
              var outIndex = 0
              fieldType.fieldShape.dimensions match {
                case 0 =>
                  val c = cpuMemory.read()
                  data(outIndex) = c.real
                  data(outIndex + 1) = c.imaginary
                  outIndex += 2
                case 1 =>
                  for (col <- 0 until fieldType.columns) {
                    val c = cpuMemory.read(col)
                    data(outIndex) = c.real
                    data(outIndex + 1) = c.imaginary
                    outIndex += 2
                  }
                case 2 =>
                  for (row <- 0 until fieldType.rows) {
                    for (col <- 0 until fieldType.columns) {
                      val c = cpuMemory.read(row, col)
                      data(outIndex) = c.real
                      data(outIndex + 1) = c.imaginary
                      outIndex += 2
                    }
                  }
                case 3 =>
                  for (layer <- 0 until fieldType.layers) {
                    for (row <- 0 until fieldType.rows) {
                      for (col <- 0 until fieldType.columns) {
                        val c = cpuMemory.read(layer, row, col)
                        data(outIndex) = c.real
                        data(outIndex + 1) = c.imaginary
                        outIndex += 2
                      }
                    }
                  }
              }
              saver.writeFloatArray("data", data)
            case 1 =>
              val cpuMemory = fieldReader.asInstanceOf[ComplexVectorFieldMemory]
              val vectorLength = fieldType.tensorShape.points
              val arrayLength = fieldType.fieldShape.points * vectorLength * 2
              val data = new Array[Float](arrayLength)
              val cv = new ComplexVector(vectorLength)
              var outIndex = 0
              fieldType.fieldShape.dimensions match {
                case 0 =>
                  cpuMemory.read(cv)
                  for (i <- 0 until vectorLength) {
                    data(outIndex) = cv.realAt(i)
                    data(outIndex + 1) = cv.imaginaryAt(i)
                    outIndex += 2
                  }
                case 1 =>
                  for (col <- 0 until fieldType.columns) {
                    cpuMemory.read(col, cv)
                    for (i <- 0 until vectorLength) {
                      data(outIndex) = cv.realAt(i)
                      data(outIndex + 1) = cv.imaginaryAt(i)
                      outIndex += 2
                    }
                  }
                case 2 =>
                  for (row <- 0 until fieldType.rows) {
                    for (col <- 0 until fieldType.columns) {
                      cpuMemory.read(row, col, cv)
                      for (i <- 0 until vectorLength) {
                        data(outIndex) = cv.realAt(i)
                        data(outIndex + 1) = cv.imaginaryAt(i)
                        outIndex += 2
                      }
                    }
                  }
                case 3 =>
                  for (layer <- 0 until fieldType.layers) {
                    for (row <- 0 until fieldType.rows) {
                      for (col <- 0 until fieldType.columns) {
                        cpuMemory.read(layer, row, col, cv)
                        for (i <- 0 until vectorLength) {
                          data(outIndex) = cv.realAt(i)
                          data(outIndex + 1) = cv.imaginaryAt(i)
                          outIndex += 2
                        }
                      }
                    }
                  }
              }
              saver.writeFloatArray("data", data)
            case x =>
              throw new RuntimeException("not implemented yet for tensor order " + x)
          }
        case x =>
          throw new RuntimeException("not implemented for element type: " + x)
      }
    }
  }

  /** Read the save data for this field using the 'restorer', converting it to a ConstantOp function. */
  private[cpukernels]
  def restoreData(restorer: ObjectRestorer, fieldType: FieldType): NulleryOpcode = {
    import fieldType._
    elementType match {
      case Uint8Pixel =>
        val arrayLength = fieldShape.points
        val data = restorer.readIntArray("data")
        if (data.length == 0)
          NullOp
        else {
          require(data.length == arrayLength, "Internal error: found field data of length " + data.length +
            ", expecting length " + arrayLength)
          ConstantColorOp((row, col) => {
            val packedInt = data(row * columns + col)
            val red = ((packedInt >> 16) & 0xff).toByte
            val green = ((packedInt >> 8) & 0xff).toByte
            val blue = (packedInt & 0xff).toByte
            // Save/restore of color fields does not include any alpha channel info.
            new Pixel(red, green, blue)
          })
        }
      case Float32 =>
        val data = restorer.readFloatArray("data")
        if (data.length == 0)
          NullOp
        else {
          tensorShape.dimensions match {
            case 0 =>
              val arrayLength = fieldShape.points
              require(data.length == arrayLength, "Internal error: found field data of length " + data.length +
                ", expecting length " + arrayLength)
              fieldShape.dimensions match {
                case 0 =>
                  ConstantScalar0DOp(() => data(0))
                case 1 =>
                  ConstantScalar1DOp((col) => data(col))
                case 2 =>
                  ConstantScalar2DOp((row, col) => data(row * columns + col))
                case 3 =>
                  ConstantScalar3DOp((layer, row, col) =>
                    data(layer * rows * columns + row * columns + col))
              }
            case 1 =>
              val vectorLength = tensorShape.points
              val arrayLength = fieldShape.points * vectorLength
              require(data.length == arrayLength, "Internal error: found field data of length " + data.length +
                ", expecting length " + arrayLength)
              fieldShape.dimensions match {
                case 0 =>
                  val vector = new Vector(vectorLength)
                  Array.copy(data, 0, vector.asArray, 0, vectorLength)
                  ConstantVector0DOp(() => vector)
                case 1 =>
                  val vectors = Array.tabulate(columns) { col => {
                    val v = new Vector(vectorLength)
                    Array.copy(data, vectorLength * col, v.asArray, 0, vectorLength)
                    v
                  }
                  }
                  ConstantVector1DOp((col) => vectors(col))
                case 2 =>
                  val vectors = Array.tabulate(rows, columns) { (row, col) => {
                    val v = new Vector(vectorLength)
                    Array.copy(data, vectorLength * (row * columns + col), v.asArray, 0, vectorLength)
                    v
                  }
                  }
                  ConstantVector2DOp((row, col) => vectors(row)(col))
                case 3 =>
                  val vectors = Array.tabulate(layers, rows, columns) { (layer, row, col) => {
                    val v = new Vector(vectorLength)
                    Array.copy(data, vectorLength * (layer * rows * columns + row * columns + col), v.asArray, 0, vectorLength)
                    v
                  }
                  }
                  ConstantVector3DOp((layer, row, col) => vectors(layer)(row)(col))
              }
            case 2 =>
              val matrixSize = tensorShape.points
              val arrayLength = fieldShape.points * matrixSize
              require(data.length == arrayLength, "Internal error: found field data of length " + data.length +
                ", expecting length " + arrayLength)
              // Warning: should the internal cogmath Matrix data layout depart from that assumed during the save
              // operation for fields, then the Array.copy shortcut to reading cannot be used.
              fieldShape.dimensions match {
                case 0 =>
                  val matrix = new Matrix(tensorRows, tensorColumns)
                  Array.copy(data, 0, matrix.asArray, 0, matrixSize)
                  ConstantMatrix0DOp(() => matrix)
                case 1 =>
                  val matrices = Array.tabulate(columns) { col => {
                    val m = new Matrix(tensorRows, tensorColumns)
                    Array.copy(data, matrixSize * col, m.asArray, 0, matrixSize)
                    m
                  }
                  }
                  ConstantMatrix1DOp((col) => matrices(col))
                case 2 =>
                  val matrices = Array.tabulate(rows, columns) { (row, col) => {
                    val m = new Matrix(tensorRows, tensorColumns)
                    Array.copy(data, matrixSize * (row * columns + col), m.asArray, 0, matrixSize)
                    m
                  }
                  }
                  ConstantMatrix2DOp((row, col) => matrices(row)(col))
                case 3 =>
                  val matrices = Array.tabulate(layers, rows, columns) { (layer, row, col) => {
                    val m = new Matrix(tensorRows, tensorColumns)
                    Array.copy(data, matrixSize * (layer * rows * columns + row * columns + col), m.asArray, 0, matrixSize)
                    m
                  }
                  }
                  ConstantMatrix3DOp((layer, row, col) => matrices(layer)(row)(col))
              }
            case x =>
              throw new RuntimeException("not implemented yet for tensor order " + x)
          }
        }
      case Complex32 =>
        val data = restorer.readFloatArray("data")
        if (data.length == 0)
          NullOp
        else {
          tensorShape.dimensions match {
            case 0 =>
              val arrayLength = fieldShape.points * 2
              require(data.length == arrayLength, "Internal error: found field data of length " + data.length +
                ", expecting length " + arrayLength)
              fieldShape.dimensions match {
                case 0 =>
                  val complex = new Complex(data(0), data(1))
                  ConstantComplex0DOp(() => complex)
                case 1 =>
                  val complexes = Array.tabulate(columns) { col =>
                    val baseIndex = col
                    Complex(data(2 * baseIndex), data(2 * baseIndex + 1))
                  }
                  ConstantComplex1DOp((col) => complexes(col))
                case 2 =>
                  val complexes = Array.tabulate(rows, columns) { (row, col) =>
                    val baseIndex = row * columns + col
                    Complex(data(2 * baseIndex), data(2 * baseIndex + 1))
                  }
                  ConstantComplex2DOp((row, col) => complexes(row)(col))
                case 3 =>
                  val complexes = Array.tabulate(layers, rows, columns) { (layer, row, col) =>
                    val baseIndex = layer * rows * columns + row * columns + col
                    Complex(data(2 * baseIndex), data(2 * baseIndex + 1))
                  }
                  ConstantComplex3DOp((layer, row, col) => complexes(layer)(row)(col))
              }
            case 1 =>
              val vectorLength = tensorShape.points
              val arrayLength = fieldShape.points * vectorLength * 2
              require(data.length == arrayLength, "Internal error: found field data of length " + data.length +
                ", expecting length " + arrayLength)
              fieldShape.dimensions match {
                case 0 =>
                  val complexVector = ComplexVector(vectorLength, (i) => Complex(data(2 * i), data(2 * i + 1)))
                  ConstantComplexVector0DOp(() => complexVector)
                case 1 =>
                  val complexVectors = Array.tabulate(columns) { col =>
                    val baseIndex = col
                    ComplexVector(vectorLength,
                      (i) => Complex(data(2 * (baseIndex * vectorLength + i)), data(2 * (baseIndex * vectorLength + i) + 1))
                    )
                  }
                  ConstantComplexVector1DOp((col) => complexVectors(col))
                case 2 =>
                  val complexVectors = Array.tabulate(rows, columns) { (row, col) =>
                    val baseIndex = row * columns + col
                    ComplexVector(vectorLength,
                      (i) => Complex(data(2 * (baseIndex * vectorLength + i)), data(2 * (baseIndex * vectorLength + i) + 1))
                    )
                  }
                  ConstantComplexVector2DOp((row, col) => complexVectors(row)(col))
                case 3 =>
                  val complexVectors = Array.tabulate(layers, rows, columns) { (layer, row, col) =>
                    val baseIndex = layer * rows * columns + row * columns + col
                    ComplexVector(vectorLength,
                      (i) => Complex(data(2 * (baseIndex * vectorLength + i)), data(2 * (baseIndex * vectorLength + i) + 1))
                    )
                  }
                  ConstantComplexVector3DOp((layer, row, col) => complexVectors(layer)(row)(col))
              }
            case x =>
              throw new RuntimeException("not implemented yet for tensor order " + x)
          }
        }
      case x =>
        throw new RuntimeException("not implemented for element type: " + x)
    }
  }
}
