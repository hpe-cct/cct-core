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
import cogx.cogmath.geometry.Shape


/** Many field operations must access "virtual" field elements that exist
  * outside of the finite field. StaticConvolution is a simple example of this.
  * Since different applications require different policies for handling of
  * non-existent elements, we encapsulate the generally most useful policies
  * here.
  *
  * @author Greg Snider
  */

private [cogx] sealed abstract class BorderPolicy {
  /** Compute the shape of a field resulting from convolution of an input field
    * with a filter given this border policy.
    *
    * The size of the output field of convolution can be the same size, larger
    * or smaller than the size of the input field, depending on the specific
    * border policy. "Valid" convolution implies a smaller result, "full"
    * implies a larger result, everything else implies a same size result.
    * Note that "valid" and "full" are Matlab names that we borrow here.
    *
    * @param inputShape Shape of the field which will be convolved with a filter.
    * @param filterSize Size of the filter. This is the same in all dimensions,
    *        so if inputShape is 2D, the filter has the shape
    *        Shape(filterSize, filterSize).
    * @return Shape of the result field where a filter of the specified size
    *        has been convolved with a field of shape `inputShape`.
    */
  def convolutionShape(inputShape: Shape, filterSize: Int): Shape
}

/** "Clamp" undefined elements to the nearest border element. */
private [cogx] case object BorderClamp extends BorderPolicy {
  /** Compute the shape of a field resulting from convolution of an input field
    * with a filter given this border policy.
    *
    * @param inputShape Shape of the field which will be convolved with a filter.
    * @param filterSize Size of the filter. This is the same in all dimensions,
    *        so if inputShape is 2D, the filter has the shape
    *        Shape(filterSize, filterSize).
    * @return Shape of the result field where a filter of the specified size
    *        has been convolved with a field of shape `inputShape`.
    */
  def convolutionShape(inputShape: Shape, filterSize: Int): Shape = inputShape
}

/** Assign zero to all undefined elements. */
private [cogx] case object BorderZero extends BorderPolicy {
  /** Compute the shape of a field resulting from convolution of an input field
    * with a filter given this border policy.
    *
    * @param inputShape Shape of the field which will be convolved with a filter.
    * @param filterSize Size of the filter. This is the same in all dimensions,
    *        so if inputShape is 2D, the filter has the shape
    *        Shape(filterSize, filterSize).
    * @return Shape of the result field where a filter of the specified size
    *        has been convolved with a field of shape `inputShape`.
    */
  def convolutionShape(inputShape: Shape, filterSize: Int): Shape = inputShape
}

/** Wrap the field like a torus so that all indexed elements are defined. */
private [cogx] case object BorderCyclic extends BorderPolicy {
  /** Compute the shape of a field resulting from convolution of an input field
    * with a filter given this border policy.
    *
    * @param inputShape Shape of the field which will be convolved with a filter.
    * @param filterSize Size of the filter. This is the same in all dimensions,
    *        so if inputShape is 2D, the filter has the shape
    *        Shape(filterSize, filterSize).
    * @return Shape of the result field where a filter of the specified size
    *        has been convolved with a field of shape `inputShape`.
    */
  def convolutionShape(inputShape: Shape, filterSize: Int): Shape = inputShape
}


/** Reject any points that spill over the edge of the signal.*/
private [cogx] case object BorderValid extends BorderPolicy {
  /** Compute the shape of a field resulting from convolution of an input field
    * with a filter given this border policy.
    *
    * The result field is always smaller than the input field.
    *
    * @param inputShape Shape of the field which will be convolved with a filter.
    * @param filterSize Size of the filter. This is the same in all dimensions,
    *        so if inputShape is 2D, the filter has the shape
    *        Shape(filterSize, filterSize).
    * @return Shape of the result field where a filter of the specified size
    *        has been convolved with a field of shape `inputShape`.
    */
  def convolutionShape(inputShape: Shape, filterSize: Int): Shape = {
    require(filterSize % 2 == 1, "filter size must be odd")
    inputShape.map(_ - filterSize + 1)
  }
}

/** Pad the border with zeros before convolution. Adjoint of valid convolution*/
private [cogx] case object BorderFull extends BorderPolicy {
  /** Compute the shape of a field resulting from convolution of an input field
    * with a filter given this border policy.
    *
    * The result field is always larger than the input field.
    *
    * @param inputShape Shape of the field which will be convolved with a filter.
    * @param filterSize Size of the filter. This is the same in all dimensions,
    *        so if inputShape is 2D, the filter has the shape
    *        Shape(filterSize, filterSize).
    * @return Shape of the result field where a filter of the specified size
    *        has been convolved with a field of shape `inputShape`.
    */
  def convolutionShape(inputShape: Shape, filterSize: Int): Shape = {
    require(filterSize % 2 == 1, "filter size must be odd")
    inputShape.map(_ + filterSize - 1)
  }
}
