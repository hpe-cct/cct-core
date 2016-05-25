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

import java.io._
import cogx.platform.types.FieldType
import cogx.platform.types.ElementTypes.Float32
import cogx.reference._
import cogx.cogmath.geometry.Shape
import cogx.cogmath.algebra.real.{Vector, Scalar}

/**
  * FieldIO trait contains functions for writing/reading ScalarFields to/form
  * java streams. The format for a file is the following:
  *
  *   1) a header that contains the field type in the following fashion:
  *        Int (N) that indicates the number of chars in the tensor type string
  *        N Chars which indicate the tensor type (e.g. "ScalarTensor")
  *        Int (M) that indicates the number of field dimensions
  *        M Ints containing the sizes of each dimension
  *        Int (L) that indicates the number of tensor dimensions
  *        L Ints containing the sizes of each dimensions
  *
  *    2) a data block which first contains a Long which indicates the tick at
  *        which the data were saved, followed by the data which are stored as
  *        Floats in order of field.fieldShape.indicies, that is, indexed by
  *        iterating over the inner dimensions first (e.g. in 2D, columns are in the inner loop).
  *
  * Note that currently only real ScalarField and VectorField IO are implemented
  * Note also that a java DataOutput(Input)Stream is used so files will always be
  *   little-endian regardless of system architecture
  * @author Matthew Pickett
  */
trait FieldIO {

  //store enough data to reconstruct FieldType, typically used as a header
  def writeFieldType(fieldType:FieldType, outStream:DataOutputStream) {
    //write tensor type
    //hard code tensor type as a string so it is easier for external
    //plotting/analysis program (e.g. Mathematica) to parse field type
    require(fieldType.elementType == Float32)
    val tensorString = fieldType.tensorOrder match {
      case 0 => "ScalarLattice"
      case 1 => "VectorLattice"
      case _ => throw new RuntimeException(fieldType.tensorOrder.toString + "-order tensor can not be exported")
    }
    val tensorChars = tensorString.length
    outStream.writeInt(tensorChars)
    outStream.writeChars(tensorString)

    //write field dimensions
    outStream.writeInt(fieldType.fieldShape.dimensions)
    fieldType.fieldShape.toArray.foreach( outStream.writeInt(_) )

    //write tensor dimensions
    outStream.writeInt(fieldType.tensorShape.dimensions)
    fieldType.tensorShape.toArray.foreach( outStream.writeInt(_) )
  }

  //reconstruct FieldType, typically from a bit of header data
  def readFieldType(inStream:DataInputStream):FieldType = {
    //read and determine tensor type
    val tensorChars = inStream.readInt
    val tensorString = Array.tabulate(tensorChars){
      (i) => inStream.readChar.toString
    }.reduce(_+_)
    val tensor: Int = tensorString match {
      case "ScalarLattice" => 0
      case "VectorLattice" => 1
      case _ => throw new RuntimeException(tensorString + " tensor can not be imported")
    }

    //read field shape
    val fieldDims = inStream.readInt()
    require(fieldDims>0, "bad field dimension in input stream")
    val fieldSizes = Array.tabulate(fieldDims){
     (i) => inStream.readInt()
    }
    require(fieldSizes.map(_>0).reduce(_&_), "bad field size in input stream")
    val fieldShape = Shape(fieldSizes: _*)

    //read tensor shape
    val tensorDims = inStream.readInt()
    require(tensorDims>=0, "bad tensors dimension in input stream")
    val tensorSizes = Array.tabulate(tensorDims){
       (i) => inStream.readInt()
    }
    if (tensorDims > 0)
      require(tensorSizes.map(_>0).reduce(_&_), "bad tensor size in input stream")
    val tensorShape = Shape(tensorSizes: _*)

    new FieldType(fieldShape, tensorShape, Float32)
  }

  ///////////////////
  // Scalar field IO
  ///////////////////

  //read scalar field data from a DataInputStream
  def readScalarFieldData(fieldType: FieldType, inStream: DataInputStream): RefScalarField = {
    require(fieldType.tensorOrder == 0, "TensorType must be ScalarTensor")
    require(fieldType.elementType == Float32)
    val dummy = inStream.readLong() //pop the timestamp off the front of the data set
    val theField = RefScalarField(fieldType.fieldShape)
    theField.fieldShape.indices.foreach(
      (i) => theField.data.write(new Scalar(inStream.readFloat()), i: _*)
    )
    theField
  }

  //write scalar field data to a DataOutputStream
  def writeScalarFieldData(theField:RefScalarField, outStream:DataOutputStream,
                                  timeStamp:Long = -1L) {
    outStream.writeLong(timeStamp)
    theField.fieldShape.indices.foreach((i)=> outStream.writeFloat(theField.read(i: _*)))
  }

  //reads the first scalar field stored in an exported file and returns a scalar field
  def readScalarFieldFromFile(theFile:File): RefScalarField = {
    val inStream = new DataInputStream( new FileInputStream( theFile))
    val fieldType = readFieldType(inStream)
    val theField = readScalarFieldData(fieldType, inStream)
    inStream.close()
    theField
  }
  //write both the header and the field data for a given field in one call.
  def writeScalarFieldToFile(theField:RefScalarField, theFile:File, timeStamp:Long = -1L){
    val outStream = new DataOutputStream(new FileOutputStream(theFile))
    writeFieldType(theField.fieldType, outStream)
    writeScalarFieldData(theField, outStream)
    outStream.close()
  }

  ///////////////////
  // Vector field IO
  ///////////////////
  //read vector field data from a DataInputStream
  def readVectorFieldData(fieldType: FieldType, inStream: DataInputStream): RefVectorField = {
    require(fieldType.tensorOrder == 1, "TensorType must be VectorTensor")
    require(fieldType.elementType == Float32)
    val dummy = inStream.readLong() //pop the timestamp off the front of the data set
    val theField = RefVectorField(fieldType.fieldShape, fieldType.tensorShape)
    val vecLen = fieldType.tensorShape(0)
    theField.fieldShape.indices.foreach(
      (i) => {
        val tensorArray = Array.tabulate(vecLen){
          (j) => inStream.readFloat()
        }
        theField.data.write(Vector(vecLen, (k)=> tensorArray(k)), i: _*)
      }
    )
    theField
  }
  //write vector field data to a DataOutputStream
  def writeVectorFieldData(theField: RefVectorField, outStream: DataOutputStream,
                                  timeStamp: Long = -1L) {
    outStream.writeLong(timeStamp)
    theField.fieldShape.indices.foreach(
      (i)=> theField.read(i: _*).toArray.foreach(
        outStream.writeFloat(_)
      )
    )
  }
  //read the first stored field in a field file into a field
  def readVectorFieldFromFile(theFile:File):RefVectorField = {
    val inStream = new DataInputStream( new FileInputStream( theFile))
    val fieldType = readFieldType(inStream)
    val theField = readVectorFieldData(fieldType, inStream)
    inStream.close()
    theField
  }
  //write a single field to a file
  def writeVectorFieldToFile(theField:RefVectorField, theFile:File, timeStamp:Long = -1L){
    val outStream = new DataOutputStream(new FileOutputStream(theFile))
    writeFieldType(theField.fieldType, outStream)
    writeVectorFieldData(theField, outStream)
    outStream.close()
  }

}
