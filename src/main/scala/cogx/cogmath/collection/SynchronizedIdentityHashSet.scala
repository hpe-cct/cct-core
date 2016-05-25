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
  * @author Greg Snider and Dick Carter
  */

private [cogx] class SynchronizedIdentityHashSet[A <: AnyRef] extends IdentityHashSet[A] {
  /** Add "value" to this set. */
  override def +=(value: A) = synchronized {
    require(value != null, "Cannot add 'null' to a set.")
    super.add(value)
    this
  }

  /** Add "value" to this set, reporting true if the element wasn't present. */
  override def add(value: A): Boolean = synchronized {
    require(value != null, "Cannot add 'null' to a set.")
    val newEntry = super.add(value)
    newEntry
  }

  /** Remove "value" from this set. */
  override def -= (value: A) = synchronized {
    require(value != null, "Cannot remove 'null' from a set.")
    super.remove(value)
    this
  }

  /** Remove all entries from this set. */
  override def clear = synchronized {
    super.clear()
    this
  }

  /** Check if "value" is contained in "this". */
  override def contains(value: A): Boolean = synchronized {
    super.contains(value)
  }

  /** Iterator over elements in "this". */
  override def iterator: Iterator[A] = synchronized {
    super.iterator
  }

  /** Copies all the elements from the specified set to this set. */
  def putAll(s: SynchronizedIdentityHashSet[A]): Unit = synchronized {
    this ++= s
  }

  /** Create a copy of this set. */
  override def copy: SynchronizedIdentityHashSet[A] = synchronized {
    val newSet = new SynchronizedIdentityHashSet[A]()
    newSet.putAll(this)
    newSet
  }

  /** Returns the number of entries in the set. */
  def length = synchronized {
    size
  }
}

