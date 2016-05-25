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

package cogx.compiler.optimizer

import cogx.cogmath.geometry.Shape
import cogx.platform.cpumemory.readerwriter.ScalarFieldReader
import cogx.runtime.ComputeGraph

import scala.language.reflectiveCalls
import org.scalatest.junit.JUnitRunner
import org.scalatest.FunSuite
import org.scalatest.MustMatchers
import org.junit.runner.RunWith
import cogx.api.CogFunctionAPI
import cogx.compiler.parser.syntaxtree.ScalarField

/** Test code for the reshape hyperkernel remover optimizer.
  *
  * @author Dick Carter
  */

@RunWith(classOf[JUnitRunner])
class ReshapeRemoverSpec
  extends FunSuite
  with MustMatchers
  with CogFunctionAPI
{
  /** Check that the 'cg' ComputeGraph has 'n' HyperKernels */
  def requireHyperKernelCount(cg: ComputeGraph, n: Int) {
    require(cg.hyperkernelCount == n,
      "Expecting " + n + " HyperKernels, found " + cg.hyperkernelCount)
  }

  test("Reshape kernel elimination") {
    val cg = new ComputeGraph(optimize = true) {
      // Constant field kernel that drives nothing
      val in = ScalarField(100, (c) => c)

      // The part of the compute graph that should not go away
      val out = in.flip.reshape(Shape(10,10), Shape()).flip + 1f
      probe(out)
    }
    import cg._
    withRelease {
      step
      // Only device kernels that remains are the 2 FlipKernels (the const kernel is a CPU kernel and the +1 is merged).
      if (ReshapeRemover.Enabled)
        requireHyperKernelCount(cg, 2)
      for(row <- 0 until 10; col <- 0 until 10)
        require(read(out).asInstanceOf[ScalarFieldReader].read(row, col) == 1f + 10 * row + col)
    }
  }


}