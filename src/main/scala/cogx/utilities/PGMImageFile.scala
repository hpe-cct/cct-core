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

import java.io._

/** Reader of PGM (portable gray map) image files.
  *
  * This will only handle "raw" PGM files where the pixels are unsigned bytes
  * and will only read the first image in the file. This could easily be
  * extended to read PGM files with multiple images in them.
  *
  * @author Greg Snider
  */
@deprecated("This functionality has been moved to cogio.imagefiles.GrayscaleImage.fromPGM and will be removed in cog 5.0", "4.1.11")
object PGMImageFile {
  /** Read the image in `filename` and return as a 2D array of pixels. */
  def apply(filename: String): Array[Array[Float]] =  {
    val file = new File(filename)
    require(file.exists, "Cannot find file " + filename)
    require(file.canRead, "Cannot read file " + filename)
    val input = new FileInputStream(file)

    // Read header
    require(readToken(input).equals("P5"), "missing P5 header");
    val widthString = readToken(input)
    val heightString = readToken(input)
    val maxPixelString = readToken(input)

    val columns = Integer.parseInt(widthString);
    val rows = Integer.parseInt(heightString);
    val MaxPixel = Integer.parseInt(maxPixelString);
    require(MaxPixel < 256, "Multibyte pixel in .pgm file.")
    val image = Array.fill(rows, columns)(0f)
    for (row <- 0 until rows; col <- 0 until columns)
      image(row)(col) = input.read.toFloat / 255f
    input.close
    image
  }

  /** Read a string token (delimited by whitespace) from the image file. */
  private def readToken(input: FileInputStream): String = {
    var token = ""
    // Strip leading white space
    var done = false
    while (!done) {
      val ch = input.read.toChar
      if (!Character.isWhitespace(ch)) {
        token += ch
        done = true
      }
    }

    // Grab token until we find more whitespace
    done = false
    while (!done) {
      val ch: Char = input.read.toChar
      if (Character.isWhitespace(ch))
        done = true
      else
        token = token + ch
    }
    token
  }
}

