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

/** Upsampling the input and downsampling the output of a convolution operation
  * are common combinations of operations which don't merge well without explicit
  * program design due to the field non-locality of the convolution operation.
  * These case classes are defined in order to provide the convolution function
  * with information on the sampling policy so that the functions can be merged.
  *
  * @author Matthew Pickett
  */
private [cogx] sealed abstract class ConvolutionSamplingPolicy

private [cogx] case object NoSamplingConvolution extends ConvolutionSamplingPolicy
private [cogx] case class UpsampleInputConvolution(step:Int) extends ConvolutionSamplingPolicy
private [cogx] case class DownsampleOutputConvolution(step:Int) extends ConvolutionSamplingPolicy