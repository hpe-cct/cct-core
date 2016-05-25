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

/** A parameter to allow our testing and performance suites to exercise the
  * convolve operator with both FFT and conventional convolution approaches.
  *
  * @author Dick Carter
  */
private [cogx] sealed abstract class ConvolutionFFTUsePolicy(val fftOK: Boolean, val stdConvolveOK: Boolean)

private [cogx] case object UseFFTNever extends ConvolutionFFTUsePolicy(false, true)
private [cogx] case object UseFFTAlways extends ConvolutionFFTUsePolicy(true, false)
private [cogx] case object UseFFTWhenBest extends ConvolutionFFTUsePolicy(true, true)