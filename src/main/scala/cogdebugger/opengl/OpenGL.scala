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

import com.jogamp.opengl._
import com.jogamp.opengl.glu.GLU

/**
 * Singleton object to init OpenGL and let us grab context from anywhere.
 * Also has useful utility functions for dealing with vectors and matrices.
 */
object OpenGL {

  val verbose = false

  // Docs recommended that this call be made before anything else in the
  // program. Apparently it helps prevent some errors on linux systems.
  GLProfile.initSingleton()

  val glu = new GLU
  val joglVersion = com.jogamp.opengl.JoglVersion.getInstance
  val profile = GLProfile.get(GLProfile.GL4)
  val caps = new GLCapabilities(profile)
  caps.setHardwareAccelerated(true)

  if (verbose) {
    println("[OpenGL] JOGL Version: " + joglVersion.getImplementationVersion + "\n" +
            "  Profile: " + profile + "\n" +
            "  Capabilities: " + caps)
  }

  /**Basic vertex shader. Can specify model, view, and projection matrices,
   * and vertex position and color (no alpha) */
  val basicVertexShader =
    "uniform mat4 projMatrix, viewMatrix, modelMatrix;                 \n " +
    "in vec4 position;                                                 \n" +
    "in vec3 color;                                                    \n" +
    "out vec3 Color;                                                   \n" +
    "void main() {                                                     \n" +
    "  Color = color;                                                  \n" +
    "  gl_Position = projMatrix * viewMatrix * modelMatrix * position; \n" +
    "}                                                                 \n"

  /**Basic fragment shader. Just copies RGB color from vertex shader. */
  val basicFragmentShader =
    "in vec3 Color;                              \n" +
    "void main() {                               \n" +
    "  gl_FragColor = vec4(Color, 1.0);          \n" +
    "}                                           \n"

  /**
   * Attaches the appropriate debugDrawable to the given drawable using the
   * composable pipeline. The debugDrawables call glGetError after every OpenGL
   * call and reports any errors found.
   */
  def setDebugDraw(drawable: GLAutoDrawable) {
    drawable.getGL match {
      case gl: GL4bc => drawable.setGL(new DebugGL4bc(gl))
      case gl: GL4 => drawable.setGL(new DebugGL4(gl))
      case gl: GL3bc => drawable.setGL(new DebugGL3bc(gl))
      case gl: GL3 => drawable.setGL(new DebugGL3(gl))
      case gl: GL2 => drawable.setGL(new DebugGL2(gl))
    }
  }

  def checkShaderLogInfo(gl: GL4, shaderId: Int) {
    import gl._
    val intValue = java.nio.IntBuffer.allocate(1)
    glGetShaderiv(shaderId, GL2ES2.GL_INFO_LOG_LENGTH, intValue)

    val lengthWithNull = intValue.get()

    if (lengthWithNull <= 1) return

    val infoLog = java.nio.ByteBuffer.allocate(lengthWithNull)

    intValue.flip()
    glGetShaderInfoLog(shaderId, lengthWithNull, intValue, infoLog)

    val actualLength = intValue.get()

    val infoBytes = new Array[Byte](actualLength)
    infoLog.get(infoBytes)
    println("[OpenGL] GLSL Shader (" + shaderId + ") Log >> " + new String(infoBytes))
  }

  def checkProgramLogInfo(gl: GL4, programId: Int) {
    import gl._
    val intValue = java.nio.IntBuffer.allocate(1)
    glGetProgramiv(programId, GL2ES2.GL_INFO_LOG_LENGTH, intValue)

    val lengthWithNull = intValue.get()
    if (lengthWithNull <= 1) return

    val infoLog = java.nio.ByteBuffer.allocate(lengthWithNull)

    intValue.flip()
    glGetProgramInfoLog(programId, lengthWithNull, intValue, infoLog)

    val actualLength = intValue.get()

    val infoBytes = new Array[Byte](actualLength)
    infoLog.get(infoBytes)
    println("[OpenGL] GLSL Program (" + programId + ") Log >> " + new String(infoBytes))
  }

  def checkError(gl: GL)(op: => Unit) {
    gl.glGetError() // Clear any pre-existing error flag
    op
    val err = gl.glGetError()
    if (err != GL.GL_NO_ERROR) Console.err.println("OpenGL Error: "+glu.gluErrorString(err))
  }

}
