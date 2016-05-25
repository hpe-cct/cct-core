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

package cogx.cogmath.hypergraph

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.FunSuite
import org.scalatest.MustMatchers
import scala.collection.mutable.ArrayBuffer
import scala.language.reflectiveCalls

/** Test code for the Hyperedge class.
  *
  * @author Greg Snider
  */
@RunWith(classOf[JUnitRunner])
class StronglyConnectedComponentsSpec extends FunSuite with MustMatchers {

  test("all") {
    /**
     * Creates a hypergraph with known connectivity and
     * results (hypergraph comes from the text mentioned at the beginning of the
     * class, section 22.5 (Strongly connected components) figure 22.9 (p. 553 in
     * the edition I have).
     */
    val graph = new Hypergraph {
      val a = new Hypernode(this)
      val b = new Hypernode(this)
      val c = new Hypernode(this)
      val d = new Hypernode(this)
      val e = new Hypernode(this)
      val f = new Hypernode(this)
      val g = new Hypernode(this)
      val h = new Hypernode(this)
      new Hyperedge(a, Array(b))
      new Hyperedge(e, Array(a))
      new Hyperedge(b, Array(e))
      new Hyperedge(b, Array(f))
      new Hyperedge(b, Array(c))
      new Hyperedge(f, Array(g))
      new Hyperedge(g, Array(f))
      new Hyperedge(c, Array(d))
      new Hyperedge(d, Array(c))
      new Hyperedge(g, Array(h))
      new Hyperedge(d, Array(h))
    }
    val clusters: Seq[ArrayBuffer[Hypernode]] =
      StronglyConnectedComponents.apply(graph)
    for (cluster <- clusters) {
      cluster.size match {
        case 3 =>
          require(cluster contains graph.a)
          require(cluster contains graph.b)
          require(cluster contains graph.e)
        case 1 =>
          require(cluster contains graph.h)
        case 2 =>
          if (cluster contains graph.f)
            require(cluster contains graph.g)
          else {
            require(cluster contains graph.c)
            require(cluster contains graph.d)
          }
      }

    }

  }
}