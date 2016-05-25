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

import cogx.runtime.debugger.ProbedCircuit
import cogx.runtime.debugger.ProbedField
/*
 * Created with IntelliJ IDEA.
 * User: gonztobi
 * Date: 8/26/13
 * Time: 1:39 PM
 */

/** A tree describing the module hierarchy implied in a given Cog model's
  * definition. Supports lookups and traversals.
  *
  * Basically, any class that has Cog fields as members is called a Module. In
  * addition, any class that has a Module as a member is also called a Module,
  * and is considered a parent to all those member Modules. Any Module that
  * doesn't have a parent is attached as a child to a meta-Module that we just
  * call Root.
  *
  * The actual reflection/introspection code that finds all these module
  * relationships lives in the field auto-naming logic within ComputeGraph.
  * This class is actually determining the relationships by breaking apart
  * field names.
  *
  * Example:
  * {{{
  * class Bar { val scalarField = ScalarField( ... ) }
  * class Foo { val bar = new Bar() }
  * val cg = new ComputeGraph {
  *   val foo = new Foo()
  * }
  * }}}
  * In the above compute graph, a single scalar field is created that will be
  * named "foo.bar.scalarField." This name is generated using reflection:
  * the anonymous ComputeGraph has a Foo member named foo which in turn has a
  * Bar member named bar which in turn has a ScalarField member named
  * scalarField. The namer smashes those together to create a long name for the
  * field; this ModuleHierarchyTree class breaks the name back apart to
  * determine relationships between fields and parent objects (the so-called
  * Modules).
  **/
class ModuleHierarchyTree(circuit: ProbedCircuit) {

  /** The root of the module hierarchy. Usually (always?) corresponds to an
    * instance of [[cogx.runtime.ComputeGraph]]. */
  val moduleHierarchyRoot = new ModuleNode("Root", null)
  circuit.traversePreorder(buildPathFor(_))

  /** Returns a set of all ProbedFields below this node in the tree. */
  def allDescendantsFor(module: ModuleNode) = {
    val set = collection.mutable.Set[FieldNode]()
    postOrderTraverse(module, { case f: FieldNode => set += f; case _ => })
    set
  }

  /** Checks if 'node' is a descendant of the module 'ancestor' */
  def hasAncestor(node: ModuleTreeNode, ancestor: ModuleNode) = {
    var parent = node.parent
    while ((parent ne ancestor) && (parent ne moduleHierarchyRoot)) parent = parent.parent
    parent eq ancestor
  }

  def postOrderTraverse(op: (ModuleTreeNode) => Unit) { postOrderTraverse(moduleHierarchyRoot, op) }
  def postOrderTraverse(node: ModuleTreeNode, op: (ModuleTreeNode) => Unit) {
    node match {
      case m: ModuleNode =>
        for (child <- m.children) postOrderTraverse(child, op)
        op(m)
      case f: FieldNode => op(f)
    }
  }

  /** Adds the given field to the module tree, creating parent nodes for any
    * ancestor modules as needed. */
  private def buildPathFor(field: ProbedField) {
    var currentParent = moduleHierarchyRoot
    val path = field.name.slice(0, field.name.length - 1)
    for (module <- path)
      currentParent = currentParent.childModules.getOrElseUpdate(module, ModuleNode(module, currentParent))
    currentParent.fields += FieldNode(field, currentParent)
  }

  /** Looks for a FieldNode corresponding to the field with the given long
    * name.
    * @param name Long name of the field to look up (e.g. "path.to.a.field")
    */
  def get(name: String): Option[FieldNode] = {
    val parts = name.split("\\.")
    val path = parts.slice(0, parts.length - 1)
    val fieldShortName = parts(parts.length - 1)
    var module = moduleHierarchyRoot
    for (step <- path) {
      module.childModules.get(step) match {
        case Some(child) => module = child
        case None => return None
      }
    }
    module.fields.foreach { fieldNode =>
      if (fieldNode.wrappedField.simpleName == fieldShortName)
        return Some(fieldNode)
    }
    None
  }

  def get(field: ProbedField): Option[FieldNode] = get(field.name.mkString("."))

  def contains(name: String) = get(name).isDefined
  def contains(field: ProbedField) = get(field).isDefined

}

/** A node in the module tree. One of either [[cogdebugger.ModuleNode]] or
  * [[cogdebugger.FieldNode]]. */
protected[cogdebugger] sealed trait ModuleTreeNode {
  def parent: ModuleNode
  def hasAncestor(ancestor: ModuleNode) = {
    var parent = this.parent
    while ((parent ne ancestor) && parent != null)
      parent = parent.parent
    parent eq ancestor
  }
}

/** A node representing a module. ModuleNodes can contain both other
  * other ModuleNodes and FieldNodes as children. */
protected[cogdebugger] case class ModuleNode(name: String,
                                             parent: ModuleNode)
        extends ModuleTreeNode {
  val childModules = collection.mutable.Map[String, ModuleNode]()
  val fields = collection.mutable.Set[FieldNode]()
  def children = childModules.values ++ fields
  def fullName = { // <-- nasty hack; should just save the full name when built
    var name = this.name
    var parent = this.parent
    while (parent != null) {
      name = parent.name + "." + name
      parent = parent.parent
    }
    name
  }
}

/** A leaf node representing a (probed) field. */
protected[cogdebugger] case class FieldNode(wrappedField: ProbedField,
                                            parent: ModuleNode)
        extends ModuleTreeNode