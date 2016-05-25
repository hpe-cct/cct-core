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

package cogdebugger.ui.fieldvisualizations.vector

import libcog._
import cogdebugger.ui.fieldvisualizations.EventDrivenViewer
import cogdebugger.ui.components.ToolBar
import cogdebugger.opengl.SGLCanvas
import com.jogamp.opengl.util.glsl._
import com.jogamp.opengl.util.{PMVMatrix, GLArrayDataServer}
import com.jogamp.opengl.util.texture.{TextureData, Texture}
import com.jogamp.common.nio.Buffers
import java.nio.FloatBuffer
import java.awt.Dimension
import javax.swing.event.ChangeEvent
import com.jogamp.opengl._
import com.jogamp.opengl.GL._

/*
 * Created with IntelliJ IDEA.
 * User: gonztobi
 * Date: 3/19/13
 * Time: 2:24 PM
 */

/** A ColorFlowVectorPanel accelerated using OpenGL. "ColorFlow" here is a
  * mapping from a 2D geometric vector to a single pixel: the color of the
  * pixel indicates the vector's direction, and saturation denotes the vector's
  * magnitude.
  *
  * The 'maxVectorLength' variable within this class controls the saturation
  * of a pixel representing a vector. The saturation value of a pixel for a
  * given vector is determined by dividing the vector length by
  * maxVectorLength. 0-length vectors will make for white pixels, and vectors
  * with lengths meeting or exceeding maxVectorLength will produce fully
  * saturated pixels.
  *
  * Implementation notes:
  *
  * Cog VectorFields are stored in a planar format (as opposed to interleaved).
  * Since OpenGL doesn't have any built-in planar image formats, we slice the
  * VectorField into individual component planes and load each plane into a
  * separate texture. The fragment shader can then sample a single component
  * out of each texture, reassemble the vector, and encode it.
  *
  * All the real work is done in the fragment shader. All we're rendering is a
  * single full-screen (or rather full-canvas) quad, so there's not much to the
  * vertex shader; it really just passes its input through the pipeline.
  */
class ColorFlowGL(target: AnyRef, fieldShape: Shape, tensorShape: Shape)
  extends SGLCanvas
  with EventDrivenViewer
  //with ZoomProperty
{
  import VectorColorFlowGL._

  //peer.setLayout(new java.awt.FlowLayout())

  require(fieldShape.dimensions == 2, "[ColorFlowGL] Only 2D VectorFields are currently supported")
  require(tensorShape == Shape(2), "[ClorFlowGL] Only 2D vectors are supported.")

  val targets = Set(target)

  //private lazy val canvas = new GLCanvas(caps)
  //override lazy val peer = new JPanel() with SuperMixin { add(canvas) }

//  private var data: Option[VectorFieldMemory] = None
  val Rows = fieldShape(0)
  val Cols = fieldShape(1)

  // In this initial port for Cog 4, we don't have access to the underlying
  // raw buffer anymore and have to do some copying into a local array. No need
  // to account for padding then, since we may as well make our array as small
  // as possible.
  ///** Column count rounded up to nearest multiple of 16 to account for memory
  //  * layout padding. */
  //val RoundedCols = if (Cols % 16 == 0) Cols else Cols + 16 - (Cols % 16)
  val RoundedCols = Cols

  // Cog's row/column indexing maps to <-y, x> vectors. The negative sign is
  // handled in the (fragment) shader.
  private def ys = yPlane//data.sliceTensor(0).data.asRawArray
  private def xs = xPlane//data.sliceTensor(1).data.asRawArray

  val yPlane = new Array[Float](Rows * Cols)
  val xPlane = new Array[Float](Rows * Cols)

  private val pmvMatrix = new PMVMatrix()
  private val pmvMatrixUniform = new GLUniformData("pmvMatrix", 4, 4, pmvMatrix.glGetPMvMvitMatrixf()) // P, Mv, Mvi, and MVit

  private val vertexData = GLArrayDataServer.createGLSLInterleaved(4, GL_FLOAT, false, 4, GL_STATIC_DRAW)
  val vs = vertexData.addGLSLSubArray("vertices", 2, GL.GL_ARRAY_BUFFER)
  val ts = vertexData.addGLSLSubArray("texcoords", 2, GL.GL_ARRAY_BUFFER)

  private val st = new ShaderState()
  st.ownUniform(pmvMatrixUniform)
  st.ownAttribute(vertexData, true)

  private val xPlaneTex = new Texture(GL_TEXTURE_2D)
  private val yPlaneTex = new Texture(GL_TEXTURE_2D)
  private val xTexData = new TextureData(profile, GL_R32F, RoundedCols, Rows, 0, GL2ES2.GL_RED, GL_FLOAT, false, false, false, Buffers.newDirectFloatBuffer(Rows * RoundedCols), null)
  private val yTexData = new TextureData(profile, GL_R32F, RoundedCols, Rows, 0, GL2ES2.GL_RED, GL_FLOAT, false, false, false, Buffers.newDirectFloatBuffer(Rows * RoundedCols), null)

  private var _maxVectorLength = 1f
  def maxVectorLength = _maxVectorLength
  def maxVectorLength_=(newVal: Float) {
    _maxVectorLength = newVal
    vecLenUniform.setData(maxVectorLength)
    wrappedGlCanvas.display()
  }
  private val vecLenUniform = new GLUniformData("maxVectorLen", maxVectorLength)
  private var dataChanged = false // Flag indicated textures need updating

  st.ownUniform(pmvMatrixUniform)
  st.ownAttribute(vertexData, true)

  // Populate the textures with the initial data
  xTexData.getBuffer.asInstanceOf[FloatBuffer].put(xs).rewind()
  yTexData.getBuffer.asInstanceOf[FloatBuffer].put(ys).rewind()

  // Set the canvas size
  wrappedGlCanvas.setPreferredSize(new Dimension(Cols, Rows))

  def update(src: AnyRef, data: AbstractFieldMemory, time: Long) {
    (data.elementType, data.tensorShape.dimensions) match {
      case (Float32, 1) => update(src, data.asInstanceOf[VectorFieldReader])
      case _ => throw new RuntimeException("Viewer got unexpected data")
    }
    //FieldMemoryPool.release(data)
  }

  def update(src: AnyRef, data: VectorFieldReader) {
    val tmpVector = Vector(2, _ => 0f)
    for (r <- 0 until Rows; c <- 0 until Cols) {
      data.read(r, c, tmpVector)
      yPlane(r * Cols + c) = tmpVector(0)
      xPlane(r * Cols + c) = tmpVector(1)
    }
    dataChanged = true
    wrappedGlCanvas.display()
  }

  def init(drawable: GLAutoDrawable) {
    val gl = drawable.getGL.getGL4; import gl._
    drawable.setGL(new DebugGL4(gl))

    glClearColor(1f, 1f, 1f, 1f)      // White background
    //glClearColor(0.7f, 0f, 0f, 1f)    // Deep red background for debugging
    glDisable(GL_DEPTH_TEST)          // Not needed for 2D rendering
    glDisable(GL_BLEND)               // Don't need blending

    initShaders(gl)
    initTextures(gl)
    initVertexData(gl)
  }

  def dispose(drawable: GLAutoDrawable) {
    val gl = drawable.getGL.getGL4
    vertexData.destroy(gl)
    xPlaneTex.destroy(gl)
    xTexData.destroy()
    yPlaneTex.destroy(gl)
    yTexData.destroy()
    st.destroy(gl)
    // PvmMatrix.destroy() method is absent as of jogl 2.2.0.
//    pmvMatrix.destroy()
  }

  def display(drawable: GLAutoDrawable) {
    val gl = drawable.getGL.getGL4; import gl._

    // Shouldn't be necessary to glClear; we do a full redraw each time, so
    // just render right over top of the old stuff
    //glClear(GL.GL_COLOR_BUFFER_BIT)

    if (dataChanged) {
      dataChanged = false
      xTexData.getBuffer.asInstanceOf[FloatBuffer].put(xs).rewind()
      xPlaneTex.updateImage(gl, xTexData)
      yTexData.getBuffer.asInstanceOf[FloatBuffer].put(ys).rewind()
      yPlaneTex.updateImage(gl, yTexData)
    }

    st.uniform(gl, vecLenUniform)
    gl.glDrawArrays(GL.GL_TRIANGLE_STRIP, 0, vertexData.getElementCount)
  }

  def reshape(drawable: GLAutoDrawable, x: Int, y: Int, width: Int, height: Int) {
    val gl = drawable.getGL.getGL4
    pmvMatrix.glMatrixMode(com.jogamp.opengl.fixedfunc.GLMatrixFunc.GL_PROJECTION)
    pmvMatrix.glLoadIdentity()
    pmvMatrix.glOrthof(-1.0f, 1.0f, -1.0f, 1.0f, -1.0f, 1.0f)
    pmvMatrix.glMatrixMode(com.jogamp.opengl.fixedfunc.GLMatrixFunc.GL_MODELVIEW)
    pmvMatrix.glLoadIdentity()
    st.useProgram(gl, true)
    st.uniform(gl, pmvMatrixUniform)
  }

  private def initVertexData(gl: GL4) {
    // Vertex locations (just a quad) are interleaved with texel coords
    // A couple things to remember: One, we're using triangle strips (hence the
    // order of the vertices) and two, OpenGL's coordinate system is not the
    // same as Cog's!
    vertexData.putf(-1f); vertexData.putf(-1f) // Bottom left
    vertexData.putf( 0f); vertexData.putf( 0f)
    vertexData.putf(-1f); vertexData.putf( 1f) // Top Left
    vertexData.putf( 0f); vertexData.putf(1f)
    vertexData.putf( 1f); vertexData.putf(-1f) // Bottom right
    vertexData.putf(Cols.toFloat / RoundedCols); vertexData.putf( 0f)
    vertexData.putf( 1f); vertexData.putf( 1f) // Top Right
    vertexData.putf(Cols.toFloat / RoundedCols); vertexData.putf(1f)

    vertexData.seal(gl, true)
  }

  private def initTextures(gl: GL4) { import gl._
    glActiveTexture(GL_TEXTURE0)
    xPlaneTex.bind(gl)
    xPlaneTex.setTexParameteri(gl, GL_TEXTURE_MIN_FILTER, GL_NEAREST)
    xPlaneTex.setTexParameteri(gl, GL_TEXTURE_MAG_FILTER, GL_NEAREST)
    xPlaneTex.updateImage(gl, xTexData)

    glActiveTexture(GL_TEXTURE1)
    yPlaneTex.bind(gl)
    yPlaneTex.setTexParameteri(gl, GL_TEXTURE_MIN_FILTER, GL_NEAREST)
    yPlaneTex.setTexParameteri(gl, GL_TEXTURE_MAG_FILTER, GL_NEAREST)
    yPlaneTex.updateImage(gl, yTexData)
  }

  private def initShaders(gl: GL4) {
    val vp = ShaderCode.create(gl, GL2ES2.GL_VERTEX_SHADER,   this.getClass, shaderDir, shaderDir, shaderName, false)
    val fp = ShaderCode.create(gl, GL2ES2.GL_FRAGMENT_SHADER, this.getClass, shaderDir, shaderDir, shaderName, false)
    require(vp.compile(gl, Console.err) && fp.compile(gl, Console.err))
    val shader = new ShaderProgram
    shader.init(gl)
    shader.add(vp); shader.add(fp)
    shader.link(gl, Console.err)
    shader.validateProgram(gl, Console.err)
    st.attachShaderProgram(gl, shader, true)
    st.attachObject("pmvMatrix", pmvMatrix)
    st.uniform(gl, pmvMatrixUniform)
    st.uniform(gl, new GLUniformData("texture1", 0))
    st.uniform(gl, new GLUniformData("texture2", 1))
  }

  var zDelta = 1f

}

object ColorFlowGL {
  val profile = GLProfile.get(GLProfile.GL4)
  val caps = new GLCapabilities(profile)

  Log.i(s"Profile: $profile")
  Log.i(s"Capabilities: $caps")

  val shaderDir = "src/main/glsl"
  val shaderName = "colorFlowShader"

  object Log {
    val dbgLvl = 2
    def e(msg: String) { if (dbgLvl > 0) Console.err.println(msg) }
    def w(msg: String) { if (dbgLvl > 1) Console.err.println(msg) }
    def i(msg: String) { if (dbgLvl > 2) println(msg) }
    def d(msg: String) { if (dbgLvl > 3) println(msg) }
  }
}

import scala.swing._
import scala.swing.event._
object DummyProbeTarget

/** A wrapper for [[cogdebugger.ui.fieldvisualizations.vector.ColorFlowGL]]
  * that adds a few nice features, such as a hideable color key that shows the
  * mapping between angle and color. It's expected that this is the component
  * that'll be dropped into GUIs, rather than ColorFlowGL directly. */
class ColorFlowVectorPanelGL(target: AnyRef, fShape:Shape, tShape: Shape)
  extends BorderPanel
  with EventDrivenViewer {
  //with Zoomable {

  val targets = Set(target)

  val colorFlow = new VectorColorFlowGL(target, fShape, tShape)
  val key = {
    val keyField = ColorFlowVectorPanelGL.keyField//new ColorFlowGL(DummyProbe, ColorFlowVectorPanelGL.keyField)
    val keyVisual = new VectorColorFlowGL(DummyProbeTarget2, keyField.fieldType.fieldShape, keyField.fieldType.tensorShape)
    keyVisual.preferredSize = new Dimension(keyField.fieldType.fieldShape(1), keyField.fieldType.fieldShape(0))
    keyVisual.maximumSize = new Dimension(keyField.fieldType.fieldShape(1), keyField.fieldType.fieldShape(0))
    keyVisual.update(DummyProbeTarget2, keyField.asInstanceOf[VectorFieldReader])

    // Stretches really badly without being wrapped in some other panel type
    new FlowPanel(keyVisual)
  }

  val viewerTools = new ViewerTools

  layout(viewerTools) = BorderPanel.Position.North
  layout(colorFlow) = BorderPanel.Position.Center

  def update(src: AnyRef, data: AbstractFieldMemory, time: Long) {
    (data.elementType, data.tensorShape.dimensions) match {
      case (Float32, 1) => update(src, data.asInstanceOf[VectorFieldReader])
      case _ => throw new RuntimeException("Viewer got unexpected data")
    }
    //FieldMemoryPool.release(data)
  }

  def update(src: AnyRef, data: VectorFieldReader) {
    colorFlow.update(src, data)
  }

//  val zDelta = 1f
//  def zoomIn() { changeZoomLevel(zDelta) }
//  def zoomOut() { changeZoomLevel(-zDelta) }
//  def changeZoomLevel(delta: Float) {
//    println("Zoom not yet supported in GL based ColorFlow panel. Sorry.")
//  }

  class ViewerTools extends ToolBar("Color Flow Tools") {
    floatable = false

    val showKeyButton = new ToggleButton("Show key")

    val model = new javax.swing.SpinnerNumberModel(1, 0, 100, 1)
    val jSpinner = new javax.swing.JSpinner(model)
    jSpinner.addChangeListener(new javax.swing.event.ChangeListener() {
      def stateChanged(e: ChangeEvent) { colorFlow.maxVectorLength = jSpinner.getValue.asInstanceOf[Int] }
    })
    val spinner = Component.wrap(jSpinner)

    listenTo(showKeyButton)
    listenTo(colorFlow)
    reactions += {
      case ButtonClicked(`showKeyButton`) => toggleKeyVisibility()
    }

    val exportControls = Array(spinner, showKeyButton)

    def toggleKeyVisibility() {
      if (showKeyButton.selected)
        layout(key) = BorderPanel.Position.East
      else
        layout -= key
      revalidate()
    }

//    contents += Button("+") { zoomIn() }
//    contents += Button("-") { zoomOut() }
    contents ++= Seq(Swing.HStrut(10), new Label("Clamp to:"), Swing.HStrut(5), spinner)
    contents += Swing.HGlue
    contents += showKeyButton
  }

}

object ColorFlowVectorPanelGL {
  private val (keyRows, keyCols) = (64 | 1, 64 | 1)
  private val (cRow, cCol) = (keyRows / 2, keyCols / 2)
  private val maxLen = cRow min cCol
  private val normalize = 1f / maxLen
  val keyFieldType = new FieldType(Shape(keyRows, keyCols), Shape(2), Float32)
  val keyField = FieldMemory.direct(keyFieldType).asInstanceOf[VectorFieldMemory]

  for (r <- 0 until keyRows; c <- 0 until keyCols) {
    val v = new Vector((r - cRow) * normalize, (c - cCol) * normalize)
    if (v.normL2 > 1f)
      new Vector(0, 0)
    else
      v
    keyField.write(r, c, v)
  }
}