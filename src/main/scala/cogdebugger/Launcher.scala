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

import java.awt._

/*
 * Created with IntelliJ IDEA.
 * User: gonztobi
 * Date: 6/10/13
 * Time: 12:52 PM
 */

/** Sketch of a standalone launcher for the CogDebugger application. Right now,
  * all it really does is pop up a splash screen (and that only if you use the
  * right run configuration).
  *
  * Eventually, will take the name of a .class file defining an extension of
  * ComputeGraph as an argument, and set up the browser for that app. */
object Launcher {
  def main(args: Array[String]) {

    val splash = Option(SplashScreen.getSplashScreen)
    splash foreach renderSplash

    if (args.length <= 0) {
      printUsage(); return
    } else {
      var argIdx = 0
      while (argIdx < args.length)
        argIdx = parseArgument(args, argIdx)
    }

    println("Launching Cog Browser.")

    // TODO - Actually launch the app!
  }

  def printUsage() {
    println("Cog Ex Machina v4 Browser")
    println("Usage:")
    println("-h | --help ........ prints this help message")
  }

  /** Parses the argument at argIdx from the given array of arguments. Returns
    * the index of the next argument to be parsed, assuming that parsing begins
    * with the 0th arg and goes up from there. */
  def parseArgument(args: Array[String], argIdx: Int): Int = {
    args(argIdx) match {
      case "-h" | "--help" => printUsage()
      case x => Console.err.println("Unrecognized argument: "+x)
    }
    argIdx + 1 // Default return value: The next argument index
  }

  /** Draws the text "Cog Ex Machina v4" on top of the splash screen image. */
  private def renderSplash(splash: SplashScreen) {

    // Note: Drawing code should avoid importing anything from javax.swing, as
    // that'll force that whole package to load before we can do _any_ drawing.

    val g = splash.createGraphics()
    val theString = "Cog Ex Machina v4"

    val font = new Font("arial", Font.ITALIC, 52)
    g.setFont(font)
    val metrics = g.getFontMetrics
    val strWidth = metrics.stringWidth(theString)
    val strHeight = metrics.getHeight

    val (w, h) = (strWidth + 10, strHeight + metrics.getLeading)
    val (x, y) = ((splash.getSize.width - w) / 2, 250)

    g.setColor(new Color(255, 255, 255, 255)) // White
    g.fillRect(x, y, w, h)
    g.setColor(new Color(0, 0, 0, 255))       // Black
    g.drawRect(x, y, w, h)

    g.drawString(theString, x, y + h - metrics.getLeading - metrics.getDescent)
    g.dispose()

    splash.update()
  }

}
