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

/** In order to use the same function for cross-correlation and convolution
  * an option must be set which indicates the orientation of the filter as it
  * is rastered across the input. Cross-Correlation rasters the filter in its
  * native orientation while convolution rasters the filter flipped about its
  * central point.
  *
  * @author Matthew Pickett
  */

private [cogx] sealed abstract class FilterOrientation
private [cogx] case object ConvolutionOrientation extends FilterOrientation
private [cogx] case object CrossCorrelationOrientation extends FilterOrientation
