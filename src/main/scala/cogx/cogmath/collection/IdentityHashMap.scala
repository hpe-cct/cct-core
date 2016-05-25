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

/** A map which uses object identity to determine equality of members, not
  * the "equals" and "hashCode" methods.
  *
  * This class should protect the user from calling map(), which will generate
  * a Map that has dropped the `identity` property.
  *
  * @author Greg Snider
  */

private [cogx] class IdentityHashMap[A, B] extends mutable.HashMap[A, B] {

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

}