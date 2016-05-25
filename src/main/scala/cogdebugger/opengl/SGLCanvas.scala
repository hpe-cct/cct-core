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

package cogdebugger.opengl

import java.awt.event._
import javax.swing.JComponent

import scala.swing._
import scala.swing.event._

import com.jogamp.opengl.{GLProfile, GLCapabilities, GLEventListener}

/**
  * Scala Swing wrapper for a GLCanvas (a heavyweight AWT component). GLCanvas
  * has better rendering performance than GLPanel, but it doesn't always play
  * nicely with Swing. It's pretty easy to change a class to use one or the
  * other, so it may be a good idea to start with the this GLCanvas wrapper and
  * only switch to the GLPanel wrapper if you can't get the former to work in
  * your GUI.
  */
abstract class SGLCanvas(caps: GLCapabilities) extends Component with GLEventListener {

  def this(profile: GLProfile) = this(new GLCapabilities(profile))
  def this() = this(GLProfile.getDefault)
  
  val wrappedGlCanvas = new com.jogamp.opengl.awt.GLCanvas(caps)
  peer.setLayout(new java.awt.GridLayout(1, 1))
  wrappedGlCanvas.addGLEventListener(SGLCanvas.this)
  peer.add(wrappedGlCanvas)

  // Scala Swing's normal event publishers don't work because our peer is using
  // a "heavyweight" AWT component as a child instead of a "lightweight" Swing
  // component. Scala won't let us override Component's mouse object, so we
  // have to wire up our AWT canvas to the existing publishers. As long as the
  // events appear to be coming from 'peer,' things ought to work.

  override def focusable: Boolean = wrappedGlCanvas.isFocusable
  override def focusable_=(b: Boolean) { wrappedGlCanvas.setFocusable(b) }
  override def requestFocus() { wrappedGlCanvas.requestFocus() }
  override def requestFocusInWindow() = wrappedGlCanvas.requestFocusInWindow()
  override def hasFocus: Boolean = wrappedGlCanvas.isFocusOwner

  wrappedGlCanvas.addMouseListener(new MouseListener {
    def mouseEntered(e: java.awt.event.MouseEvent) {
      e.setSource(peer)
      mouse.moves.publish(new MouseEntered(e))
    }
    def mouseExited(e: java.awt.event.MouseEvent) {
      e.setSource(peer)
      mouse.moves.publish(new MouseExited(e))
    }
    def mouseClicked(e: java.awt.event.MouseEvent) {
      e.setSource(peer)
      mouse.clicks.publish(new MouseClicked(e))
    }
    def mousePressed(e: java.awt.event.MouseEvent) {
      e.setSource(peer)
      mouse.clicks.publish(new MousePressed(e))
    }
    def mouseReleased(e: java.awt.event.MouseEvent) {
      e.setSource(peer)
      mouse.clicks.publish(new MouseReleased(e))
    }
  })

  wrappedGlCanvas.addMouseMotionListener(new MouseMotionListener {
    def mouseMoved(e: java.awt.event.MouseEvent) {
      e.setSource(peer)
      mouse.moves.publish(new MouseMoved(e))
    }
    def mouseDragged(e: java.awt.event.MouseEvent) {
      e.setSource(peer)
      mouse.moves.publish(new MouseDragged(e))
    }
  })

  /* This may not be the correct implementation - it's not clear how it will
   * behave if this canvas is dropped into a scroll pane. Likely, it will
   * prevent mouse wheel movements from scrolling the pane.
   */
  wrappedGlCanvas.addMouseWheelListener(new MouseWheelListener {
    def mouseWheelMoved(e: MouseWheelEvent) {
      e.setSource(peer)
      mouse.wheel.publish(new MouseWheelMoved(e))
    }
  })

  wrappedGlCanvas.addKeyListener(new KeyListener {
    def keyPressed(e: java.awt.event.KeyEvent) {
      e.setSource(peer)
      publish(new KeyPressed(e))
    }
    def keyReleased(e: java.awt.event.KeyEvent) {
      e.setSource(peer)
      publish(new KeyReleased(e))
    }
    def keyTyped(e: java.awt.event.KeyEvent) {
      e.setSource(peer)
      publish(new KeyTyped(e))
    }
  })

  /* Publishes focus events of the wrapped heavyweight canvas instance. */
  wrappedGlCanvas.addFocusListener(new java.awt.event.FocusListener {
    def other(e: java.awt.event.FocusEvent) = e.getOppositeComponent match {
      case c: JComponent =>
        // I'd prefer to use "UIElement.cachedWrapper[Component](c)" as a
        // vanilla Scala-Swing Component does, but that's marked private. So we
        // do it by hand.
        Some(c.getClientProperty("scala.swingWrapper").asInstanceOf[Component])
      case _ => None
    }
    def focusGained(e: java.awt.event.FocusEvent) {
      e.setSource(peer)
      publish(FocusGained(SGLCanvas.this, other(e), e.isTemporary))
    }
    def focusLost(e: java.awt.event.FocusEvent) {
      e.setSource(peer)
      publish(FocusLost(SGLCanvas.this, other(e), e.isTemporary))
    }
  })

}
