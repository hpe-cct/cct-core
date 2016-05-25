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

package cogdebugger.ui.fieldvisualizations

/*
 * Created with IntelliJ IDEA.
 * User: gonztobi
 * Date: 6/26/13
 * Time: 5:00 PM
 */

/** An exception indicating that some operation is invalid for fields or
  * tensors of a certain dimension.
  * @param dimension The field or tensor dimension that is unsupported */
class UnsupportedDimensionException(dimension: Int)
    extends RuntimeException("Unsupported dimension: "+dimension)
