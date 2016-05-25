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

package cogdebugger.ui.fieldvisualizations.complexvector

import libcog._
import cogdebugger.{ToolFactory, BooleanProperty, RestorableState, PropertyValueChanged}
import cogdebugger.ui.fieldvisualizations._
import cogdebugger.ui.components.{SquareIcon, DoubleBufferedImagePanel, WrapPanel}
import scala.swing._
import scala.xml.{Node, Elem}
import java.awt.image.BufferedImage
import java.awt.event.MouseEvent

/** A visualization for complex vector fields that renders the field by
  * treating each vector component or plane as a separate field of complex
  * numbers, which can in turn be rendered as a pair of grayscale images - one
  * for the real component and one for the imaginary.
  * 
  * This is very similar to the VectorComponentsPanel, which renders each
  * "plane" of the vector as a separate scalar field.
  *
  * This visualization can take a lot of space. Consider wrapping it in a
  * ScrollPane before installing it in a container panel.
  * 
  * @param fieldType The FieldType of the ComplexVectorField being visualized.
  * 
  * Created by gonztobi on 3/5/14.
  */
class ComplexVectorComponentsView(fieldType: FieldType)
    extends WrapPanel
    with EventDrivenViewer
    with ZoomProperty
    with RestorableState
    with ToolbarItems
{
  
  private val fieldShape = fieldType.fieldShape
  private val tensorShape = fieldType.tensorShape
  
  require(fieldShape.dimensions <= 2, "ComplexComponentsView currently only " +
          "supports fields of 2 dimensions or less.")
  require(tensorShape.dimensions == 1, "ComplexComponentsView is only " +
          "compatible with vectors (order 1 tensors)")
  
  private val (imgRows, imgCols) = fieldShape.dimensions match {
    case 0 => (1, 1)
    case 1 => (1, fieldShape(0))
    case 2 => (fieldShape(0), fieldShape(1))
  }
  private val vecLength = tensorShape(0)
 
  /** When true, shade display based on the min/max values in the latest data
    * only. When false, display based based on the min/max values since last
    * reset. */
  val FloatingMaxProperty = new BooleanProperty("Floating Max", false) // TODO Should rerender when this value changes
  
  /** When true, force the value zero to map to the exact center of the
    * grayscale spectrum - we push either the min or max away from zero until
    * both have the same absolute value, and then shade from there. When false,
    * the min and max used in shading correspond to the actual smallest and
    * largest values present in the field. */
  val BipolarProperty = new BooleanProperty("Bipolar", false) // TODO Should rerender when this value changes

  /** If true, the color white represents a minimum value and black a maximum.
    * If false, black is a minimum and white a maximum. */
  val InvertProperty = new BooleanProperty("Invert", false)

  /** Default zoom/unzoom increment */
  val zoomIncrement: Float = 1f

  listenTo(FloatingMaxProperty, BipolarProperty, InvertProperty, ZoomProperty)
  reactions += {
    case PropertyValueChanged(FloatingMaxProperty, oldValue, newValue) =>
      if (oldValue != newValue && lastData != null) render(lastData)
    case PropertyValueChanged(BipolarProperty, oldValue, newValue) =>
      if (oldValue != newValue && lastData != null) render(lastData)
    case PropertyValueChanged(InvertProperty, oldValue, newValue: Boolean) =>
      if (oldValue != newValue) {
        subPanels foreach { _.invertColorKey(newValue) }
        if (lastData != null)
          render(lastData)
      }
    case PropertyValueChanged(ZoomProperty, oldValue, newValue: Float) =>
      subPanels.foreach(_.setZoom(newValue))
  }
  properties ++= Seq(FloatingMaxProperty, BipolarProperty, InvertProperty, ZoomProperty)

  // Min/max values for each component tracked since reset
  private val absRealMin = Array.tabulate(vecLength) { i => Float.MaxValue }
  private val absRealMax = Array.tabulate(vecLength) { i => Float.MinValue }
  private val absImaginaryMin = Array.tabulate(vecLength) { i => Float.MaxValue }
  private val absImaginaryMax = Array.tabulate(vecLength) { i => Float.MinValue }

  // Min/max values for each component based on latest data only
  private val realMin = Array.tabulate(vecLength) { i => Float.MaxValue }
  private val realMax = Array.tabulate(vecLength) { i => Float.MinValue }
  private val imaginaryMin = Array.tabulate(vecLength) { i => Float.MaxValue }
  private val imaginaryMax = Array.tabulate(vecLength) { i => Float.MinValue }

  /** Controls for this visualization that container components may wish to
    * make available to users somewhere on the UI. */
  def toolbarComponents: Seq[ComponentGroup] =
    Seq(
      ComponentGroup(
        ToolFactory.toggle(FloatingMaxProperty),
        ToolFactory.toggle(BipolarProperty),
        ToolFactory.toggle(InvertProperty)
      )
    )

  private val subPanels = Array.tabulate(vecLength) { i =>
    val panel = new VectorComponentPanel(i, imgRows, imgCols)
    panel.label.text = "Component "+i
    panel
  }
  contents ++= subPanels

  // Retain data from last update so that the visualization can be redrawn if
  // any of `shadeUsingLatestDataOnly`, `bipolarMode`, or `invert` change.
  private var lastData: ComplexVectorFieldReader = null

  // Read methods on vector fields don't produce new Vector objects, but
  // instead fill an existing on that you provide. This is the vector we read
  // into when updating the min/max and rendering.
  private val tmpVector = ComplexVector(vecLength, _ => Complex(0f, 0f))

  def update(src: AnyRef, data: AbstractFieldMemory, simTime: Long) {
    this.update(data.asInstanceOf[ComplexVectorFieldReader], simTime)
  }

  /** Update the visualization with new data. */
  def update(data: ComplexVectorFieldReader, simTime: Long) {
    require(data.fieldType == this.fieldType)
    updateMinMax(data)
    render(data)
    lastData = data
  }

  /** Update the internally tracked min and max values for each component, but
    * do not update the visualization. */
  private def updateMinMax(data: ComplexVectorFieldReader) {
    def updateWith(datum: ComplexVector) {
      for (i <- 0 until vecLength) {
        val comp = datum(i)
        val realPart = datum.realAt(i)
        val imaginaryPart = datum.imaginaryAt(i)

        require(comp.real == datum.real(i))
        require(comp.imaginary == datum.imaginary(i))

        require(!realPart.isNaN)
        if (!realPart.isNaN) {

          if (realPart < absRealMin(i)) {
            absRealMin(i) = realPart
            realMin(i)    = realPart
          } else if (realPart < realMin(i))
            realMin(i)    = realPart

          if (realPart > absRealMax(i)) {
            absRealMax(i) = realPart
            realMax(i)    = realPart
          } else if (realPart > realMax(i))
            realMax(i)    = realPart
        }

        require(!imaginaryPart.isNaN)
        if (!imaginaryPart.isNaN) {

          if (imaginaryPart < absImaginaryMin(i)) {
            absImaginaryMin(i) = imaginaryPart
            imaginaryMin(i)    = imaginaryPart
          } else if (imaginaryPart < imaginaryMin(i))
            imaginaryMin(i)    = imaginaryPart

          if (imaginaryPart > absImaginaryMax(i)) {
            absImaginaryMax(i) = imaginaryPart
            imaginaryMax(i)    = imaginaryPart
          } else if (imaginaryPart > imaginaryMax(i))
            imaginaryMax(i)    = imaginaryPart
        }
      }
    }

    fieldType.dimensions match {
      case 0 =>
        data.read(tmpVector)
        updateWith(tmpVector)
      case 1 =>
        for (c <- 0 until fieldShape(0)) {
          data.read(c, tmpVector)
          updateWith(tmpVector)
        }
      case 2 =>
        for (r <- 0 until fieldShape(0); c <- 0 until fieldShape(1)) {
          data.read(r, c, tmpVector)
          updateWith(tmpVector)
        }
    }
  }

  /** Upudate the visualization (including the values in each sub-panel's
    * legend) based on the currently recorded min/max values. */
  private def render(data: ComplexVectorFieldReader) {

    // Capture the various viewer settings for the rest of the render
    // operation. This is important if we're rendering off the EDT - a user
    // could be manipulating buttons as we draw
    val shadeUsingLatestDataOnly = FloatingMaxProperty.value
    val bipolarMode = BipolarProperty.value
    val invertGrayscale = InvertProperty.value

    /** Gets the minimum value in the field for a particular vector component
      * index for either the current step or since reset, depending on the value
      * of `shadeUsingLatestDataOnly`.
      */
    def minForComponent(componentIdx: Int): Float = {
      if (shadeUsingLatestDataOnly)
        realMin(componentIdx) min imaginaryMin(componentIdx)
      else
        absRealMin(componentIdx) min absImaginaryMin(componentIdx)
    }

    /** Gets the maximum value in the field for a particular vector component
      * index for either the current step or since reset, depending on the value
      * of `shadeUsingLatestDataOnly`.
      */
    def maxForComponent(componentIdx: Int): Float = {
      if (shadeUsingLatestDataOnly)
        realMax(componentIdx) max imaginaryMax(componentIdx)
      else
        absRealMax(componentIdx) max absImaginaryMax(componentIdx)
    }

    /** Returns the grayscale shade (a value on the range [0, 255]) for the given
      * value. Respects the `bipolarMode` and `invert` settings. */
    def shadeValue(componentIdx: Int, value: Float) = {
      var min = minForComponent(componentIdx)
      var max = maxForComponent(componentIdx)

      if (bipolarMode) {
        if (Math.abs(min) > Math.abs(max))
          max = -min
        else
          min = -max
      }

      val shade =
        if ((max - min) == 0)
          0f
        else
          (value - min) / (max - min) * 255f

      //require(shade >= 0 && shade <= 255,
      //  "Bad shade value: "+shade+s" (value: $value, min: $min, max: $max)")

      if (invertGrayscale)
        255 - shade
      else
        shade
    }

    // Set min/max labels
    for ((panel, i) <- subPanels.zipWithIndex) {
      val min = minForComponent(i)
      val max = maxForComponent(i)
      if (bipolarMode) {
        if (Math.abs(min) > Math.abs(max))
          panel.updateMinMax(min, -min)
        else
          panel.updateMinMax(-max, max)
      } else {
        panel.updateMinMax(min, max)
      }
    }

    // Paint images
    fieldType.dimensions match {
      case 0 =>
        data.read(tmpVector)
        for (i <- 0 until vecLength) {
          subPanels(i).realImage.update(_.setSample(0,0,0, shadeValue(i, tmpVector.real(i))))
          subPanels(i).imaginaryImage.update(_.setSample(0,0,0, shadeValue(i, tmpVector.imaginary(i))))
        }
        
      // The drawing process for one and two-dimensional complex vector fields
      // is a little cumbersome. We must read out a whole vector at a time, but
      // each vector is split into components and drawn in multiple panels.
      
      case 1 =>
        val realRasters      = subPanels.map(_.realImage.back.getRaster)
        val imaginaryRasters = subPanels.map(_.imaginaryImage.back.getRaster)
        for (c <- 0 until fieldShape(0)) {
          data.read(c, tmpVector)
          for (i <- 0 until vecLength) {
            realRasters(i).setSample(c, 0, 0, shadeValue(i, tmpVector.real(i)))
            imaginaryRasters(i).setSample(c, 0, 0, shadeValue(i, tmpVector.imaginary(i)))
          }
        }
        // We've already updated the rasters with new images, but now we need
        // to actually get them to draw. This is accomplished by calling the
        // update method with a no-op - a side-effect of update is that it will
        // swap the front and back buffers and start a repaint.
        for (panel <- subPanels) {
          panel.realImage.update(_ => Unit)
          panel.imaginaryImage.update(_ => Unit)
        }
        
      case 2 =>
        val realRasters      = subPanels.map(_.realImage.back.getRaster)
        val imaginaryRasters = subPanels.map(_.imaginaryImage.back.getRaster)
        for (r <- 0 until fieldShape(0); c <- 0 until fieldShape(1)) {
          data.read(r, c, tmpVector)
          for (i <- 0 until vecLength) {
            realRasters(i).setSample(c, r, 0, shadeValue(i, tmpVector.real(i)))
            imaginaryRasters(i).setSample(c, r, 0, shadeValue(i, tmpVector.imaginary(i)))
          }
        }
        for (panel <- subPanels) {
          panel.realImage.update(_ => Unit)
          panel.imaginaryImage.update(_ => Unit)
        }
    }

  }

  /** "Clears" the min/max histories (historical minimum is reset to
    * Float.MaxValue and historical max to Float.minValue) for both the real
    * and imaginary parts of all vector components. */
  override def reset() {
    for (i <- 0 until vecLength) {
      absRealMin(i) = Float.MaxValue
      absRealMax(i) = Float.MinValue
      absImaginaryMin(i) = Float.MaxValue
      absImaginaryMax(i) = Float.MinValue
    }
  }

  def save: Elem = {
    <ComplexVectorComponents>
      { propertiesTag }
    </ComplexVectorComponents>
  }

  def restore(elem: Node) {
    val myTag = elem \ "ComplexVectorComponents"
    val propsTag = myTag \ "properties"
    propsTag foreach xmlToProperties
  }

  private val tmpVector2 = ComplexVector(vecLength, _ => Complex(0f, 0f))
  
  /** A subpanel that is responsible for the visualization of a single vector
    * component or plane. Contains a pair of images for showing the real and
    * imaginary parts of the component, as well as a label (for indicating the
    * component index) and color key. */
  class VectorComponentPanel(myCompIdx: Int, rows: Int, cols: Int)
      extends BorderPanel
      with Zoomable {
    val dim = new Dimension(imgCols, imgRows)

    val realImage = new DoubleBufferedImagePanel(dim, BufferedImage.TYPE_BYTE_GRAY) with PerElementTooltips {
      override def popupText(event: MouseEvent): Option[String] = {
        val row = math.floor(event.getY / zoomLevel).toInt
        val col = math.floor(event.getX / zoomLevel).toInt
        if (lastData != null) {
          lastData.fieldShape.dimensions match {
            case 0 =>
              lastData.read(tmpVector2)
              Some(s"(): ${tmpVector2.real(myCompIdx)}")
            case 1 =>
              lastData.read(col, tmpVector2)
              Some(s"($col): ${tmpVector2.real(myCompIdx)}")
            case 2 =>
              lastData.read(row, col, tmpVector2)
              Some(s"($row, $col): ${tmpVector2.real(myCompIdx)}")
          }
        } else None
      }
    }

    val imaginaryImage = new DoubleBufferedImagePanel(dim, BufferedImage.TYPE_BYTE_GRAY) with PerElementTooltips {
      override def popupText(event: MouseEvent): Option[String] = {
        val row = math.floor(event.getY / zoomLevel).toInt
        val col = math.floor(event.getX / zoomLevel).toInt
        if (lastData != null) {
          lastData.fieldShape.dimensions match {
            case 0 =>
              lastData.read(tmpVector2)
              Some(s"(): ${tmpVector2.imaginary(myCompIdx)}")
            case 1 =>
              lastData.read(col, tmpVector2)
              Some(s"($col): ${tmpVector2.imaginary(myCompIdx)}")
            case 2 =>
              lastData.read(row, col, tmpVector2)
              Some(s"($row, $col): ${tmpVector2.imaginary(myCompIdx)}")
          }
        } else None
      }
    }
    val label = new Label("")
    
    private val minKeyIcon = new SquareIcon(java.awt.Color.BLACK)
    private val maxKeyIcon = new SquareIcon(java.awt.Color.WHITE)
    private val minKeyLabel = new Label(" = ", minKeyIcon, Alignment.Right)
    private val maxKeyLabel = new Label(" = ", maxKeyIcon, Alignment.Right)
    private val minLabel = new Label("?")
    private val maxLabel = new Label("?")

    private val imgPanel = new FlowPanel()
    imgPanel.contents ++= Seq(realImage, imaginaryImage)
    private val legend = new FlowPanel()
    legend.contents ++= Seq(minKeyLabel, minLabel, maxKeyLabel, maxLabel)

    layout(label)    = BorderPanel.Position.North
    layout(imgPanel) = BorderPanel.Position.Center
    layout(legend)   = BorderPanel.Position.South

    def updateMinMax(newMin: Float, newMax: Float) {
      minLabel.text = "%6.3f".format(newMin)
      maxLabel.text = "%6.3f".format(newMax)
    }

    def invertColorKey(invert: Boolean) {
      if (invert) {
        minKeyIcon.color = java.awt.Color.WHITE
        maxKeyIcon.color = java.awt.Color.BLACK
      } else {
        minKeyIcon.color = java.awt.Color.BLACK
        maxKeyIcon.color = java.awt.Color.WHITE
      }
      minKeyLabel.repaint()
      maxKeyLabel.repaint()
    }

    /** Default zoom/unzoom increment */
    def zDelta = ComplexVectorComponentsView.this.zoomIncrement
    def setZoom(value: Float) {
      realImage.zoomLevel = value
      imaginaryImage.zoomLevel = value
    }
    def zoomIn() {
      realImage.zoomIn()
      imaginaryImage.zoomIn()
    }
    def zoomOut() {
      realImage.zoomOut()
      imaginaryImage.zoomOut()
    }
  }
    
}
