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

package cogx.compiler.gpu_operator.types

/** Designates one of several possible output fields, by index.
  *
  * @author Greg Snider 
  */
sealed abstract class OutFieldIndex(val index: Int)

case object _out0 extends OutFieldIndex(0)
case object _out1 extends OutFieldIndex(1)
case object _out2 extends OutFieldIndex(2)
case object _out3 extends OutFieldIndex(3)
case object _out4 extends OutFieldIndex(4)
case object _out5 extends OutFieldIndex(5)
case object _out6 extends OutFieldIndex(6)
case object _out7 extends OutFieldIndex(7)
case object _out8 extends OutFieldIndex(8)
case object _out9 extends OutFieldIndex(9)
