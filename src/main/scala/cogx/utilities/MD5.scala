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

import java.security.MessageDigest
import collection.mutable.HashMap

/** Calculate the MD5 digest of a body of text, returning it as a hex string.
  *
  * User: Dick Carter
  * Date: 11/16/12
  */

private [cogx] class MD5(textBody: String) {
  def digest: String = {
    val digester = MessageDigest.getInstance("MD5")
    digester.update(textBody.getBytes())
    val digestBytes = digester.digest()
    val digestText = digestBytes.map("%02x".format(_)).mkString
    digestText
  }
}

private [cogx] object MD5 {
  /** cache of previously looked-up digests to enhance performance. */
  private val digestCache = HashMap[String, String]()
  /** Get a unique, small integer ID for a string.
    *
    * @param string The string.
    * @return Unique ID for string--will return the same ID for multiple calls
    *        with the same string.
    */
  def apply(string: String) =
    digestCache.getOrElseUpdate(string, new MD5(string).digest)
}
