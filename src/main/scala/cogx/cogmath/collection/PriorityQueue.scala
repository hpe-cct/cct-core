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

import scala.collection.mutable.ArrayBuffer

/** A priority queue that allows changing priorities of elements already in
  * the queue.
  *
  * @author Greg Snider
  */
private [cogx] class PriorityQueue[T](priority: (T) => Double) {
  private val heap = new ArrayBuffer[T]
  private val indexOf = new IdentityHashMap[T, Int]
  private var elementCount = 0

  /** Enqueue `item` onto the priority queue. */
  def enqueue(item: T) {
    elementCount += 1
    while (heap.length <= elementCount)
      heap append null.asInstanceOf[T]
    heap(elementCount) = item
    indexOf(item) = elementCount
    siftUp(elementCount)
  }

  /** Dequeue the highest priority item from the queue. */
  def dequeue: T = {
    require(elementCount > 0)
    val highest = heap(1)
    heap(1) = heap(elementCount)
    elementCount -= 1
    siftDown(1)
    indexOf -= highest
    highest
  }

  def contains(item: T): Boolean = {
    try {
      indexOf(item)
      return true
    } catch {
      case e: NoSuchElementException =>
        return false
      case _: Exception =>
        require(false, "huh?")
        return false
    }
  }

  /** Change priority of `item` in the queue. */
  def priorityChanged(item: T) {
    val index = indexOf(item)
    if (index == -1) {
      throw new RuntimeException("PriorityQueue: item not in queue.")
    } else if (index == 1) {
      // Root of tree, so priority can only go down.
      siftDown(index)
    } else {
      val parentIndex = index / 2
      if (priority(heap(parentIndex)) < priority(heap(index)))
        siftUp(index)
      else
        siftDown(index)
    }
  }

  /** Return true of queue is empty. */
  def isEmpty: Boolean =
    elementCount == 0

  /** Return number of elements in the queue. */
  def size: Int = elementCount

  private def siftUp(childIndex: Int) {
    if (childIndex > 1) {
      val child = heap(childIndex)
      val parentIndex = childIndex / 2
      val parent = heap(parentIndex)
      if (priority(parent) < priority(child)) {
        // swap parent and child
        heap(childIndex) = parent
        indexOf(parent) = childIndex
        heap(parentIndex) = child
        indexOf(child) = parentIndex
        siftUp(parentIndex)
      }
    }
  }

  private def siftDown(parentIndex: Int) {
    val leftChildIndex = 2 * parentIndex
    // If next test fails, there are no children
    if (leftChildIndex <= elementCount) {
      var bestChildIndex = leftChildIndex
      var bestChild = heap(leftChildIndex)
      val rightChildIndex = 2 * parentIndex + 1
      if (rightChildIndex <= elementCount) {
        val rightChild = heap(rightChildIndex)
        if (priority(rightChild) > priority(bestChild)) {
          bestChildIndex = rightChildIndex
          bestChild = rightChild
        }
      }
      val parent = heap(parentIndex)
      val parentPriority = priority(parent)
      if (parentPriority < priority(bestChild)) {
        // swap
        heap(bestChildIndex) = parent
        indexOf(parent) = bestChildIndex
        heap(parentIndex) = bestChild
        indexOf(bestChild) = parentIndex

        // recurse
        siftDown(bestChildIndex)
      }
    }
  }
}
