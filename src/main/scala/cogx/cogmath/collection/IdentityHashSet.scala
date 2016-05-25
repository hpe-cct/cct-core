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

package cogx.cogmath.collection

/** A set which uses object identity to determine equality of members, not
  * the "equals" and "hashCode" methods.
  *
  * We incur a slight overhead by introducing determinism in all cases.  This
  * would become important if the user invokes an iterator, foreach, toSeq, etc.
  *
  * @author Greg Snider and Dick Carter
  */

private [cogx] class IdentityHashSet[A <: AnyRef] extends IdentityHashSetDeterministic[A] {
    /** Copies all the elements from the specified set to this set. */
    def putAll(s: IdentityHashSet[A]) {
      this ++= s
    }

    /** Create a copy of this set. */
    def copy: IdentityHashSet[A] = {
      val newSet = new IdentityHashSet[A]()
      newSet.putAll(this)
      newSet
    }
}
