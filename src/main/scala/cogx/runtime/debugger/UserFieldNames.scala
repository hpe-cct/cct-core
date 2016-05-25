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

package cogx.runtime.debugger

import cogx.compiler.parser.syntaxtree.Field
import cogx.utilities.ReflectionUtils._

/** Scans a tree of objects looking for Field instances and array of instances
  * and binds the user's 'field' name (discovered through reflection) to that
  * Field.
  *
  * There is a name conflict here since Field in Cog means something different
  * from Field in Scala. I'll use 'field' to refer to a Scala field and Field
  * to refer to a Cog field. The idea is that user Fields which have been
  * declared as val `fields` can be detected at runtime through reflection,
  * and the user's name for `field` can be bound to the Field. This is useful
  * for debuggers since it allows them to display Fields with a user's
  * programmatic name.
  *
  * @author Greg Snider
  */
object UserFieldNames {

  /** Extract and apply all 'field' names to user Fields in a hierarchy of
    * objects. This causes each discovered Field to be given the name of the
    * corresponding 'field'.
    *
    * @param root The top object from which to start the search for Fields.
    */
  def extract(root: Object, rootName: String = "") {
    // Look for fields in object
    val javaFields: Array[java.lang.reflect.Field] = discoverFields(root)
    javaFields.foreach {
      javaField => {
        javaField.setAccessible(true)
        val obj = javaField.get(root)
        // Ordering of cases in the match below is important since they
        // are considered in order.
        obj match {
          case f: Field =>
            bindNDFieldName(rootName, f, javaField, Array[Int]())
          case f: Array[Field] =>
            for (i0 <- 0 until f.length)
              bindNDFieldName(rootName, f(i0), javaField, Array[Int](i0))
          case f: Array[Array[Field]] =>
            for (i1 <- 0 until f.length)
              for (i0 <- 0 until f(i1).length)
                bindNDFieldName(rootName, f(i1)(i0), javaField, Array[Int](i1, i0))
          case f: Array[Array[Array[Field]]] =>
            for (i2 <- 0 until f.length)
              for (i1 <- 0 until f(i2).length)
                for (i0 <- 0 until f(i1).length)
                  bindNDFieldName(rootName, f(i2)(i1)(i0), javaField, Array[Int](i2, i1, i0))
          case o: Array[Array[Array[Object]]] =>
            for (i2 <- 0 until o.length)
              for (i1 <- 0 until o(i2).length)
                for (i0 <- 0 until o(i1).length) {
                  val newRootName = rootName +
                          objectNDName(rootName, o(i2)(i1)(i0), javaField,
                            Array[Int](i2, i1, i0))
                  extract(o(i2)(i1)(i0), newRootName)
                }
          case o: Array[Array[Object]] =>
            for (i1 <- 0 until o.length)
              for (i0 <- 0 until o(i1).length) {
                val newRootName = rootName +
                        objectNDName(rootName, o(i1)(i0), javaField, Array[Int](i1, i0))
                extract(o(i1)(i0), newRootName)
              }
          case o: Array[Object] =>
            for (i0 <- 0 until o.length) {
              val newRootName = rootName +
                        objectNDName(rootName, o(i0), javaField, Array[Int](i0))
              extract(o(i0), newRootName)
            }
          case null =>
            // Don't know why these pop up some time
          case x =>
            val className = x.getClass.getName
            val skip = className.startsWith("java.") ||
                    className.startsWith("scala.") ||
                    className.startsWith("sun.") ||
                    className.startsWith("akka.") ||
                    className.startsWith("com.typesafe.")
            if (!skip) {
              val fieldName = javaField.getName
              fieldName match {
                // Singleton object instance created for a scala 'object'
                // (as opposed to a class)
                case "MODULE$" =>
                // Bitmap of initialized lazy vals/vars for a class
                case "bitmap$0" =>
                // Shows up in inner classes, pointer to outer class
                case "$outer" =>
                // Interesting field
                case _ =>
                  extract(x, rootName + fieldName + ".")
              }
            }
        }
        javaField.setAccessible(false)
      }
    }
  }

  /** Bind the Scala name for a multidimensional Cog Field to that field.
    *
    * @param rootName Path name preceding this.
    * @param a The Cog Field to be named.
    * @param b The reflection field for the Field.
    * @param indices The indices of the field (length 0 for 0D Fields).
    */
  private def bindNDFieldName(rootName: String,
                           a: Field,
                           b: java.lang.reflect.Field,
                           indices: Seq[Int]) {
    var name = rootName + b.getName
    for (index <- indices)
      name += "(" + index +")"
    a.setPathName(name)
  }

  /** Deduce the Scala name for a field within an object (typically a user
    * module).
    *
    * @param rootName Path name preceding this.
    * @param a The object.
    * @param b The reflection field for the object.
    * @param indices The indices of the field (length 0 for 0D Fields).
    */
  private def objectNDName(rootName: String, a: Object,
                           b: java.lang.reflect.Field,
                           indices: Seq[Int]): String =
  {
    var name = rootName + b.getName
    for (index <- indices)
      name += "(" + index +")"
    name + "."
  }
}