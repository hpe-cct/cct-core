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

package cogx.platform.types

/** Mixin that pulls out field parameters for a class containing a "FieldType".
  *
  * @author Greg Snider
  */
private[cogx]
trait FieldParameters {
  /** Field type from which to extract parameters. */
  val fieldType: FieldType

  /** Shape of the field. */
  val fieldShape = fieldType.fieldShape

  /** Shape of the tensors in the field. */
  val tensorShape = fieldType.tensorShape


  /** "Layers" in the field. See class description for meaning of layers. */
  val layers = fieldType.layers

  /** "Rows" in the field. See class description for meaning of rows. */
  val rows = fieldType.rows

  /** "Columns" in the field. See class description for meaning of columns. */
  val columns = fieldType.columns


  /** Dimensions of the field. */
  val dimensions = fieldShape.dimensions

  /** Order of the tensors in the field. */
  val tensorOrder = tensorShape.dimensions
}