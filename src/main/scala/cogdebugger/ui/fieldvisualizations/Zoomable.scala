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

package cogdebugger.ui.fieldvisualizations

import cogdebugger.FloatProperty
import cogdebugger.ui.fieldvisualizations.ZoomableProperty.ZoomType

/*
 * Created with IntelliJ IDEA.
 * User: gonztobi
 * Date: 6/24/13
 * Time: 1:14 PM
 */

/** A trait for viewers that support zoom operations, but that don't or can't
  * track their own zoom level. This is mostly intended for higher-level panels
  * that have one or more Zoomable children, which individually track their own
  * magnification level. Case-in-point:
  * [[cogdebugger.ui.fieldvisualizations.ViewerSuperPanel]] has one of any
  * number of subpanels installed at a time, but each subpanel can respond to
  * zoom changes in its own way and/or maintain its own level of magnification.
  */
trait Zoomable {
  def zoomIn()
  def zoomOut()
}

/** A mixin for GUI classes that support zoom operations. This trait defines a
  * [[cogdebugger.FloatProperty]] `ZoomLevel` that clients can make use of to
  * track and save zoom level.
  *
  * There are two ways to make something happen in response to `ZoomLevel`
  * changing: one is to `listenTo` the property and add a reaction for
  * [[cogdebugger.PropertyValueChanged]] events. The other is to install a
  * Swing action on the property itself.
  */
trait ZoomProperty extends Zoomable {

  var disallowNegativeZoom = true

  val ZoomProperty = new FloatProperty("ZoomLevel", 1f)
  def zoomLevel = ZoomProperty.value
  def zoomLevel_=(value: Float) {
    if (disallowNegativeZoom && value < 0)
      return
    ZoomProperty.value = value
  }

  /** Controls how zDelta is applied to the current zoom level. In Additive, a
    * delta is added to the current zoom level; in Multiplicative mode, the
    * zoom level is multiplied by delta when zooming in, and by its reciprocal
    * when zooming out.
    *
    * Default zoom type is additive. If you change it to multiplicative, you
    * should probably ensure that the default zDelta is something other than
    * 1f, as multiplying by one probably won't do anything.
    */
  var zoomType: ZoomType = ZoomType.Additive

  /** Default zoom/unzoom increment used by `zoomIn` and `zoomOut`. When the
    * ZoomType is Additive, this value is added to or substracted from the
    * current `zoomLevel`. In multiplicative mode, `zoomLevel` is multipled by
    * `zDelta` on zooming in or by its reciprocal on zooming out. */
  def zoomIncrement: Float

  /** Increase zoom level by `zDelta`. */
  def zoomIn() = zoomType match {
    case ZoomType.Additive       => zoomLevel += zoomIncrement //changeZoomLevel(zDelta)
    case ZoomType.Multiplicative => zoomLevel *= zoomIncrement //changeZoomLevel(zDelta)
  }

  /** Decrease zoom level by `zDelta`. */
  def zoomOut() = zoomType match {
    case ZoomType.Additive       => zoomLevel -= zoomIncrement //changeZoomLevel(-zDelta)
    case ZoomType.Multiplicative => zoomLevel /= zoomIncrement //changeZoomLevel(1f / zDelta)
  }

//  /** Change zoom level by `delta`. It is left to the implementation to
//    * determine exactly what effect this has. E.g., some Zoomables may treat
//    * this change as additive (increase size of visuals by delta pixels),
//    * others may apply it multiplicately (make display delta times bigger). */
//  def changeZoomLevel(delta: Float) = zoomType match {
//    case ZoomType.Additive       => ZoomProperty.value += delta
//    case ZoomType.Multiplicative => ZoomProperty.value *= delta
//  }
}

object ZoomableProperty {
  sealed trait ZoomType
  object ZoomType {
    case object Additive extends ZoomType
    case object Multiplicative extends ZoomType
  }
}