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

package cogdebugger

import scala.xml.{Elem, Node, NodeSeq, Attribute, Null, Text}
import java.lang.reflect.InvocationTargetException
import scala.language.postfixOps

/*
 * Created with IntelliJ IDEA.
 * User: gonztobi
 * Date: 8/21/13
 * Time: 5:08 PM
 */

trait RestorableState {

  /** Encode the state of this object into an XML node. */
  def save: Elem

  /** Restore this object to the state described in the given XML node. */
  def restore(savedState: Node)

}

object RestorableState {

  def restoreInt(elem: Node)(op: Int => Unit) {
    attempt(op(java.lang.Integer.parseInt(elem.text)))
  }

  def restoreFloat(elem: Node)(op: Float => Unit) {
    attempt(op(java.lang.Float.parseFloat(elem.text)))
  }

  def restoreDouble(elem: Node)(op: Double => Unit) {
    attempt(op(java.lang.Double.parseDouble(elem.text)))
  }

  def restoreBoolean(elem: Node)(op: Boolean => Unit) {
    attempt(op(java.lang.Boolean.parseBoolean(elem.text)))
  }

  def restoreString(elem: Node)(op: String => Unit) {
    attempt(op(elem.text))
  }

  private def attempt(op: => Unit) {
    try { op } catch { case e: Throwable => "Caught exception: "+e.getMessage }
  }

  def layoutTag(pos: (Int, Int), size: (Int, Int)) =
    <layout>
      <position>
        <x>{ pos._1 }</x>
        <y>{ pos._2 }</y>
      </position>
      <size>
        <width>{ size._1 }</width>
        <height>{ size._2 }</height>
      </size>
    </layout>

  def layoutTag(rec: java.awt.Rectangle): Elem =
    layoutTag((rec.getX.toInt, rec.getY.toInt), (rec.getWidth.toInt, rec.getHeight.toInt))

  def readLayoutTag(tag: Node) = {
    var (x, y, width, height) = (0, 0, 0, 0)
    restoreInt(tag \ "position" \ "x" head)(value => x = value)
    restoreInt(tag \ "position" \ "y" head)(value => y = value)
    restoreInt(tag \ "size" \ "width" head)(value => width = value)
    restoreInt(tag \ "size" \ "height" head)(value => height = value)
    (x, y, width, height)
  }

  /** Uses reflection to get the value of `fieldName` on `obj`, and puts its
    * toString into an XML tag. */
  def getTagFor(obj: AnyRef, fieldName: String) = {
    val clazz = obj.getClass
    val getter = clazz.getMethod(fieldName)
    val typeString = getter.getReturnType match {
      case IntClass => "int"
      case FloatClass => "float"
      case DoubleClass => "double"
      case BooleanClass => "boolean"
      case StringClass => "string"
    }
    val ret = getter.invoke(obj)
    Elem(null, fieldName, Attribute(null, "type", typeString, Null), scala.xml.TopScope, true, Text(ret.toString))
  }

  /** Given an object and a tag produced by [[getTagFor]], attempts to restore
    * the value saved in that tag to the object, i.e., does the inverse of
    * getTagFor.
    *
    * Note that this will only work for Scala classes, because it assumes that
    * the setter for a var is that var's name appended with "_=". */
  def restoreTag(obj: AnyRef, tag: Node) {
    val clazz = obj.getClass
    val setterName = tag.label + "_$eq"
    val valType = tag.attribute("type").get.head.text
    try {
      val (parsed, setter) = valType match {
        case "int" => (java.lang.Integer.parseInt(tag.text), clazz.getMethod(setterName, IntClass))
        case "float" => (java.lang.Float.parseFloat(tag.text), clazz.getMethod(setterName, FloatClass))
        case "double" => (java.lang.Double.parseDouble(tag.text), clazz.getMethod(setterName, DoubleClass))
        case "boolean" => (java.lang.Boolean.parseBoolean(tag.text), clazz.getMethod(setterName, BooleanClass))
        case "string" => (tag.text, clazz.getMethod(setterName, StringClass))
      }
      setter.invoke(obj, parsed.asInstanceOf[AnyRef])
    } catch {
      case e: NoSuchMethodException => Console.err.println(e.getMessage)
      case e: InvocationTargetException => Console.err.println(e.getMessage)
    }
  }

  /** A convenience overload for restoreTag that takes a NodeSeq rather
    * than a single Node, but only uses the data stored in the first Node in
    * the Seq (if one exists at all). */
  def restoreTag(obj: AnyRef, tags: NodeSeq) {
    tags.headOption.foreach(restoreTag(obj, _))
  }

  protected val IntClass = classOf[Int]
  protected val FloatClass = classOf[Float]
  protected val DoubleClass = classOf[Double]
  protected val BooleanClass = classOf[Boolean]
  protected val StringClass = classOf[String]

}