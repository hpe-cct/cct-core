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

package cogx.utilities

import scala.language.existentials
import java.lang.reflect.Field
import scala.reflect.{ClassTag, classTag}

/** A collection of utility functions to do mass operations on all the fields
  * of a particular type in an object. Most of these functions were originally
  * defined as methods in [[cog.distributed.TaskContainer]] to help with Task
  * auto-naming. They've since proved useful for other things as well, and so
  * have been pulled out to where they're more easily accessible.
  *
  * TODO Move this to the core and refactor Modules to use the code here
  * instead of having two copies floating around.
  */
private [cogx] object ReflectionUtils {

  val Verbose = 0
  private object Log {
    def err(msg: String)  { if (Verbose >= 0) Console.err.println(msg) }
    def warn(msg: String) { if (Verbose > 0) Console.err.println(msg) }
    def info(msg: String) { if (Verbose > 1) println(msg) }
    def dbg(msg: String)  { if (Verbose > 2) println(msg) }
  }

  private val Excludes = Set(
    "MODULE$", // Singleton object instance created for a scala 'object' (as opposed to a class)
    "bitmap$0" // bitmap of initialized lazy vals/vars for a class
  )

  // Supposedly there's some syntactic sugar I could use to get rid of this.
  // It's just used below to help identify and skip over lambda functions.
  private val FunctionClasses = Set(classOf[Function0[_]],
    classOf[Function1[_, _]],
    classOf[Function2[_, _, _]],
    classOf[Function3[_, _, _, _]],
    classOf[Function4[_, _, _, _, _]],
    classOf[Function5[_, _, _, _, _, _]])

  /* It's not unlikely that several of the utility functions here will be
   * called in succession on a single object (indeed, a few of the functions
   * here are are just convenience functions that do exactly that). Several of
   * these functions require building up a list of all the fields in an object
   * of interest. To avoid unnecessary rebuilding of that list, we'll keep a
   * reference to the last inspected object to reduce . */
  private var cachedObject: AnyRef = null
  private var cachedFields: Array[Field] = null

  private def fields(obj: AnyRef) =
    if (!(obj eq cachedObject)) {
      cachedFields = discoverFields(obj)
      cachedObject = obj
      cachedFields
    } else {
      cachedFields
    }

  def discoverFields(obj: AnyRef): Array[Field] = {
    /* In Scala, all val/var declarations create a private field and accessor
     * methods for it. getClass.getFields doesn't find private fields - only
     * public. getClass.getDeclaredFields will find the private/protected
     * fields, but since it only finds those _declared_in a class, it doesn't
     * pick up the ones inherited from superclasses. So, we have to go up the
     * hierarchy, getting the fields declared at each level. */

    if (obj == null) {
      Log.warn("[ReflectionUtils] Warning: object was null")
      return Array[Field]()
    }

    var objFields = obj.getClass.getDeclaredFields
    var superclass = obj.getClass.getSuperclass
    while (superclass != null) {
      // Don't want to chase fields defined as parts of anonymous functions.
      if (FunctionClasses.forall(!_.isAssignableFrom(superclass)))
        objFields ++= superclass.getDeclaredFields
      superclass = superclass.getSuperclass
    }
    objFields = objFields filter (field => !Excludes.contains(field.getName))

    if (objFields.length == 0)
      Log.warn("[ReflectionUtils] Warning: Reflection can find no fields in class "+ cogx.utilities.ClassName(obj)+"!")

    Log.dbg("Found fields:")
    for (field <- objFields) Log.dbg("  "+field.getName)

    objFields
  }

  /* A note to intrepid source code explorers:
   *
   * The Java fields underlying Scala val declarations are all private final -
   * when you write 'val x = 1' in Scala, the equivalent Java is more in line
   * with:
   *   private final _x = 1;          // the hidden field
   *   public int x() { return _x };  // the getter
   * Scala vars are similar, but the hidden field isn't made final and the
   * scala compiler also emits a setter method.
   *
   * Thus, to access a declared field, we need to work around the fact that the
   * underlying field is private. We could _make_ it public first, or we can
   * try to find its getter and invoke it; the latter approach is taken here
   * for the getV and setV functions. */

  /** Gets the value of the named field by looking for and invoking its getter.
    * @param obj The object from which to get the field
    * @param fieldName Name of the field to examine
    * @return An Option containing the value of the field or None if we failed
    *         to find the field's getter (may be the case with constructor
    *         parameters)
    */
  def getV(obj: AnyRef, fieldName: String): Option[AnyRef] =
    obj.getClass.getMethods.find(_.getName == fieldName) match {
      case None =>
        Log.warn("Warning: Couldn't find a getter method for field "+fieldName+".")
        None
      case Some(method) => Some(method.invoke(obj))
    }

  /** Sets the named field to the given value by finding and invoking its
    * setter.
    * @param obj The object in which to set the field
    * @param fieldName Name of the field to be set
    * @param value Value to set field to
    * @return Boolean indicating success (true) or failure (false) in setting
    *         theif field.
    */
  def setV(obj: AnyRef, fieldName: String, value: Any): Boolean =
    obj.getClass.getMethods.find(_.getName == fieldName + "_$eq") match {
      case None =>
        Log.warn("Couldn't find a setter method for field "+fieldName+".")
        false
      case Some(method) =>
        method.invoke(obj, value.asInstanceOf[AnyRef])
        true
    }

  /** Searches this class for fields of type T or Arrays (up to 3D) of type T
    * and applies the function `f` to each instance of T it finds.
    * @param obj The object in which to look for Ts
    * @param f A function that takes a T and the Field that referenced it
    * @tparam T Type of the object on which to execute `f`
    */
  def forEachField[T: ClassTag](obj: AnyRef, f: (T, Field) => Unit) {
    forEachNonArrayField(obj, f)
    forEachNDArrayField(obj, 1, (t: T, field, idxs) => f(t, field))
    forEachNDArrayField(obj, 2, (t: T, field, idxs) => f(t, field))
    forEachNDArrayField(obj, 3, (t: T, field, idxs) => f(t, field))
  }

  /**
   * Looks for non-array fields of type T and applies f to them.
   * @param obj The object in which to look for Arrays of T
   * @param f A function taking an instance of T and the field containing said
   *          instance
   * @tparam T Type of fields to look for
   */
  def forEachNonArrayField[T: ClassTag](obj: AnyRef, f: (T, Field) => Unit) {
    val erasure = classTag[T].runtimeClass

    Log.dbg("Looking for arrays of type "+erasure.toString)
    //fields(obj) filter (field => erasure.isAssignableFrom(field.getType)) foreach (field => {

    for (field <- fields(obj)) {

      Log.dbg("  inspecting field "+field.getName)

      if (erasure.isAssignableFrom(field.getType)) {
        Log.dbg("    This field is a 1D array of the desired type.")

        // Getting the actual field, rather than invoking the getter. Should
        // prevent problems with duplicate method names.
        field.setAccessible(true)
        f(field.get(obj).asInstanceOf[T], field)
        field.setAccessible(false)

      } else {
        Log.dbg("    This field isn't the right type ("+field.getType.toString+").")
      }

      // Old implementation that invokes the getter.
      //      getV(field.getName) match {
      //        case None =>
      //        case Some(value: T) =>
      //          f(value, field)
      //      }
    }//)
  }


  /** Looks for arrays of arbitrary dimensionality that ultimately contain
    * objects of type T at the deepest level, and invokes `f` on each T found.
    * @param obj Object in which to to look for N-D arrays of T
    * @param arrayDim A value for N in N-D; the dimension of arrays to look for
    * @param f A function that takes a T, the field said T was found in, and a
    *          sequence of integers holding the indices in the array that said
    *          T was found at.
    */
  def forEachNDArrayField[T: ClassTag](obj: AnyRef, arrayDim: Int, f: (T, Field, Seq[Int]) => Unit) {

    var clazz: ClassTag[_] = classTag[T]
    for (i <- 0 until arrayDim) clazz = clazz.wrap
    val erasure = clazz.runtimeClass

    Log.dbg(s"Looking for $arrayDim-D arrays of type ${erasure.toString} in object :"+obj.toString)

    def helper(field: Field, arr: Array[_], depth: Int, indices: Int*) {
      if (depth == arrayDim) {
        for (i <- 0 until arr.length)
          f(arr(i).asInstanceOf[T], field, indices.toSeq)
      } else {
        for (i <- 0 until arr.length)
          helper(field, arr(i).asInstanceOf[Array[_]], depth + 1, (indices :+ i): _*)
      }
    }

    for (field <- fields(obj)) {
      Log.dbg("  Inspecting field "+field.getName)
      field.setAccessible(true)
      val got = field.get(obj)
      // Inspecting lazy vals is one way to get null values here
      if (got != null && erasure.isAssignableFrom(got.getClass)) {
        Log.dbg("    This field is of the desired type.")
        helper(field, field.get(obj).asInstanceOf[Array[_]], 1)
      }
      field.setAccessible(false)
    }

  }
}

private [utilities] object TestReflectionUtils {

  import ReflectionUtils.forEachNDArrayField

  class Obj1 {
    val arr1D = Array.tabulate(2) { i => 1 }
    val arr2D = Array.tabulate(2, 2) { (i, j) => 2 }
    val arr3D = Array.tabulate(2, 2, 2) { (i, j, k) => 3 }
    val arr4D = Array.tabulate(2, 2, 2, 2) { (i, j, k, l) => 4 }
    val arr5D = Array.tabulate(2, 2, 2, 2, 2) { (i, j, k, l, m) => 5 }
  }

  def main(args: Array[String]) {
    val obj1 = new Obj1
    forEachNDArrayField[Int](obj1, 1, (value, field, idxs) => { print(value) })
    println()
    forEachNDArrayField[Int](obj1, 2, (value, field, idxs) => { print(value) })
    println()
    forEachNDArrayField[Int](obj1, 3, (value, field, idxs) => { print(value) })
    println()
    forEachNDArrayField[Int](obj1, 4, (value, field, idxs) => { print(value) })
    println()
    forEachNDArrayField[Int](obj1, 5, (value, field, idxs) => { print(value) })
    println()
    forEachNDArrayField[Int](obj1, 6, (value, field, idxs) => { print(value) })
    println()
  }

}