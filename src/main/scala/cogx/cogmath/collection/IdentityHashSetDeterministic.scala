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

import collection.mutable
import scala.collection.generic.CanBuildFrom

/** A set which uses object identity to determine equality of members, not
  * the "equals" and "hashCode" methods.
  *
  * IdentityHashSets are useful in places where the objects have potentially
  * supplied a slow value-based hashCode, but where object identity is an
  * acceptable definition of 'equals' from the standpoint of set membership.
  *
  * The danger though with IdentityHashSets is that the iterator over the
  * elements returns the elements in a non-deterministic order.  Sometimes this
  * non-determinism does not produce a different program output, but when it
  * does, it can make debugging and algorithm tuning difficult.
  *
  * This implementation of an IdentityHashSet adds determinism to the order
  * of the list elements returned by the iterator, and consequently to the order
  * of the list elements as seen by toArray(), foreach(), map(), etc.
  *
  * While the actual order returned by the iterator is not part of the contract
  * of this class, the current implementation returns the elements in the
  * order from which they were added to the set.
  *
  * @author Greg Snider and Dick Carter
  */

private [cogx] class IdentityHashSetDeterministic[A] extends mutable.LinkedHashSet[A] {

  override protected def elemEquals(key1: A, key2: A): Boolean = {
    key1 match {
      case obj1: AnyRef =>
        key2 match {
          case obj2: AnyRef => obj1 eq obj2
          case _ => false
        }
    }
  }

  override protected def elemHashCode(key: A): Int = {
    java.lang.System.identityHashCode(key)
  }

  // It's tricky to get map() to return an IdentityHashSetDeterministic.  By default, map will return
  // a LinkedHashSet with the identity property lost, which is probably not what the user wants.
  // Signal an error and suggest a simple work-around.
  override def map[B, That](f: A => B)(implicit bf: CanBuildFrom[mutable.LinkedHashSet[A], B, That]): That =
    throw new RuntimeException("map() not implemented. If you don't need Set semantics, try .toSeq.map() instead.")

}