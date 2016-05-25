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

//package cogdebugger.ui.fieldvisualizations.matrix
//
//import libcog._
//import scala.swing._
//
///**
// * A simple visual test case for the MatrixMemoryView. Creates a single
// * 3-dimension matrix field, builds a viewer for it, and installs that
// * viewer in a frame for display.
// *
// * Created by gonztobi on 2/24/14.
// */
//object TestMatrixMemoryView extends SimpleSwingApplication {
//  val fieldLayers = 3
//  val fieldRows = 9
//  val fieldCols = 16
//  val matrixRows = 9
//  val matrixCols = 16
//  val matrixField = MatrixFieldMemory(fieldLayers, fieldRows, fieldCols, buildMatrix)
//
//  def buildMatrix(fieldLayer: Int, fieldRow: Int, fieldCol: Int): Matrix = {
//    Matrix(matrixRows, matrixCols, (r, c) => {
//      fieldLayer * 10 + fieldRow + fieldCol + r + c
//    })
//  }
//
//  lazy val top = new MainFrame {
//    title = "Test MatrixMemoryView"
//    val view = new MatrixMemoryView(matrixField, matrixField.fieldShape, matrixField.tensorShape)
//    view.update(matrixField, matrixField, 0L)
//    contents = view
//  }
//}
