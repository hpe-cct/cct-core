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
import cogx.utilities.Random._

/** Single source of Random number generators from within the Cog core
  * software.  Outside the core, cog.Library.Random is generally used.
  *
  * Usage:
  *
  * new cogx.utilities.Random()   returns generator per the global policy on
  *                          determinism (see Random.Deterministic)
  *
  * new cogx.utilities.Random(seed)  returns seeded (i.e. deterministic) RNG
  *
  * Ultimately, all of the underlying java.util.Random rng's have their
  * seeds set.  The seeds are taken from the seedGenerator, itself a random
  * number generator.  When the Deterministic flag is set, the seedGenerator
  * is itself seeded, so all the other rng's have deterministic seeds.  When
  * the Deterministic flag is not set, the seedGenerator is not seeded and
  * so returns a random sequence of seeds to the various rng's that get created.
  *
  * Cog's Random here has extra hooks to help maintain randomly initialized
  * fields.  The idea is that such a field should look the same to multiple
  * observers, and also reset back to the same initial state.  A field's init
  * function is guaranteed to be called starting with the first field point
  * (with all-zero coordinates), plus Cog will ask for the field points contents
  * in a deterministic manner.
  *
  * User: Dick Carter
  * Date: 12/12/12
  */
@SerialVersionUID(7583272980775728570L)
class Random(val seed: Long) extends java.util.Random with Serializable
{

  def this() = this(nextSeed)
  def reset() = setSeed(seed)

  /** return nextFloat, but not until after the rng is reset if `resetFirst` */
  def nextFloatResetFirstIf(resetFirst: Boolean): Float = {
    if (resetFirst)
      reset()
    nextFloat
  }

  /** return nextInt, but not until after the rng is reset if `resetFirst` */
  def nextIntResetFirstIf(resetFirst: Boolean): Float = {
    if (resetFirst)
      reset()
    nextInt
  }

  reset()
}

object Random {
  val Seed = 8726387617121L
  val seedGenerator = new java.util.Random()

  def setDeterministic(): Unit = {
    seedGenerator.setSeed(Seed)
  }

  def nextSeed = synchronized {
    seedGenerator.nextLong()
  }

  def setGlobalSeed(seed: Long) { seedGenerator.setSeed(seed) }

}
