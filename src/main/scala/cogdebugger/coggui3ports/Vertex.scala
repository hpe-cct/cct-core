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

/*
 * Created with IntelliJ IDEA.
 * User: gonztobi
 * Date: 2/20/13
 * Time: 12:29 PM
 */

/** A vertex that appears in the network view. Classes that don't care whether
  * the underlying graph implementation is built on JGraph or JUNG can use this
  * class and its immediate subclasses. */
sealed trait Vertex

///////////////////////////////////////////////////////////////////////////////
// JGraph Vertices ////////////////////////////////////////////////////////////

import com.mxgraph.model.{mxCell, mxGeometry}

/** A vertex that only appears in graphs implemented in JGraph. An attempt to
  * add some type safety where otherwise everything is just mxCell. */
sealed trait mxVertex extends mxCell with Vertex {
  setId(null)
  setVertex(true)
  setConnectable(true)
}

///////////////////////////////////////////////////////////////////////////////

/** An edge that appears in the network view. Classes that don't care whether
  * the underlying graph implementation is JGraph or JUNG based can use this
  * class and its immediate subclasses. */
sealed trait Edge

///////////////////////////////////////////////////////////////////////////////
// JGraph Edges ///////////////////////////////////////////////////////////////

/** An edge that only appears in graphs implemented in JGraph. An attempt to
  * add some type safety where otherwise everything is just mxCell. */
sealed trait mxEdge extends Edge

/** An edge with no label for use in JGraph graphs. Don't mind that it's a
  * singleton object; JGraph uses it more as a template from which to create a
  * new edge when connecting cells. */
object mxDummyEdge extends mxEdge
