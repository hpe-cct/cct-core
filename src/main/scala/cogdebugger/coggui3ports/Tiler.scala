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

package cogdebugger.coggui3ports

import org.interactivemesh.scala.swing.event.InternalFrameClosing
import org.interactivemesh.scala.swing.InternalFrame
import swing._

/**
 * User: gonzatob<br/>
 * Date: 2/8/12<br/>
 * Time: 2:51 PM<br/>
 * <p>
 * This is a utility class to help lay out frames on a desktop in an organized
 * fashion. It's based on a technique for packing small lightmaps into a single
 * larger texture; read about it here:
 * <p>
 * http://www.blackpawn.com/texts/lightmaps/default.html
 * <p>
 * Basically, we carve up the available screen real estate into smaller and
 * smaller rectangles as we add frames. These rectangles are organized in a
 * tree where each node contains a rectangle representing a portion of the
 * screen and also has a reference to the frame occupying that rectangle (or
 * null if the space is unoccupied). A null reference signals that that piece
 * of the screen is unoccupied.
 * <p>
 * This implementation is event based to avoid issues arising from frames not
 * knowing their final size immediately upon construction - they have to build
 * and pack any child components first, which often doesn't happen before the
 * frame's constructor returns.
 * <p>
 * In its current form it has several limitations: it doesn't respond to screen
 * resizing or merge rectangles as frames are closed. It's also limited to only
 * handling ProbeFrames, when it could conceivably be made more generic to
 * handle more generic InternalFrames. Lastly, it's possible that this should
 * have been implemented as a LayoutManager to be applied to a component. This
 * was quickest though.
 */
class Tiler(desktopSize: Dimension) extends scala.swing.Reactor {
  import java.awt.geom.Rectangle2D

  val verbose = false
  private def dbg_print(s: String) { if (verbose) print(s) }
  private def dbg_println(s: String) { dbg_print(s + "\n") }

  dbg_println("Tiler initialized with desktop dimension " + desktopSize)

  /** I tried to do all this with case classes and pattern matching but wasn't
   * clever enough to get it to work.  So here's the Java-y way. */
  private class Node(var left: Option[Node],
                     var right: Option[Node],
                     val rc: Rectangle2D,
                     var frame: InternalFrame) {
    def insert(newFrame: InternalFrame): Node = {
      if (left != None || right != None) {
        dbg_print("At an inner (non-leaf) node.")
        dbg_println("Trying left child...")
        val newNode = left.get.insert(newFrame)
        if (newNode != null) { return newNode }
        dbg_println("Trying right child...")
        return right.get.insert(newFrame)
      } else {
        if (frame != null) {
          dbg_println("This node is occupied.")
          return null
        }

        val nfSize = newFrame.size
        if (nfSize.width > rc.getWidth || nfSize.height > rc.getHeight) {
          dbg_println("Frame won't fit here (" + rc.getWidth + ", " + rc.getHeight + ")")
          return null
        }

        if (nfSize.width == rc.getWidth && nfSize.height == rc.getHeight) {
          // Perfect fit!
          dbg_println("Perfect fit!")
          frame = newFrame
          return this
        }

        // Subdivide this spot and put the frame in the first subdivision
        val dw = rc.getWidth - nfSize.width
        val dh = rc.getHeight - nfSize.height
        if (dw > dh) {
          dbg_println("Slicing vertically")
          left  = Some(new Node(None, None, new Rectangle2D.Double(rc.getX, rc.getY, nfSize.width, rc.getHeight), null))
          right = Some(new Node(None, None, new Rectangle2D.Double(rc.getX + nfSize.width, rc.getY, rc.getWidth - nfSize.width, rc.getHeight), null))
        } else {
          dbg_println("Slicing horizontally")
          left  = Some(new Node(None, None, new Rectangle2D.Double(rc.getX, rc.getY, rc.getWidth, nfSize.height), null))
          right = Some(new Node(None, None, new Rectangle2D.Double(rc.getX, rc.getY + nfSize.height, rc.getWidth, rc.getHeight - nfSize.height), null))
        }
        return left.get.insert(newFrame)
      }
    }

    def remove(frame:InternalFrame): Boolean = {
      if (this.frame == frame) { this.frame = null; return true }
      return (left != None && left.get.remove (frame)) || (right != None && right.get.remove(frame))
    }
  }

  private var tileTree: Node =
    new Node(None, None, new Rectangle(desktopSize), null)

  private def insert(frame: InternalFrame): Node = {
    dbg_println("[[ Inserting a probe frame into the tiler tree. ]]")
    dbg_println(" [ Frame has dimensions: " + frame.size + " ]")
    return tileTree.insert(frame)
  }

  private def remove(frame: InternalFrame) {
    tileTree.remove(frame)
    // TODO: Merge adjacent empty cells back together
  }

  reactions += {
    case FramePackedEvent(src: InternalFrame) =>
      val loc = insert(src)
      if (loc != null)
        src.location = (loc.rc.getX.toInt, loc.rc.getY.toInt)
    case InternalFrameClosing(src, param) =>
      remove(src.asInstanceOf[InternalFrame])
      deafTo(src)
  }

  /** Prints the tile tree to console.  Debugging function. */
  def printTree() {
    printTree(tileTree, 0)
  }

  private def printTree(node: Node, level: Int) {
    println(("  " * level) + "Node - rc:" + node.rc + ", frame: " + node.frame)
    if (node.left != None) printTree(node.left.get, level + 1)
    if (node.right != None) printTree(node.right.get, level + 1)
  }

  def retile(desktopSize: Dimension) {
    // TODO
    // Sort frames largest to smallest first; helps pack things more densely
    // Then just run through the sorted list inserting/placing the frames.
  }

  def reset(newSize: Dimension = this.desktopSize) {
    tileTree = new Node(None, None, new Rectangle(newSize), null)
  }

}

/**
 * This event is used to signal that a ProbeFrame has sized itself and is
 * ready for display. This is important for the
 * [[cogdebugger.coggui3ports.Tiler]], as it can't know where to place a
 * ProbeFrame until its final size is determined.
 */
case class FramePackedEvent(source: InternalFrame) extends scala.swing.event.UIEvent
