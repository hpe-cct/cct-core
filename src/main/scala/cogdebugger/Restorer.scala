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

/**
 * Created by Dick Carter on 3/5/15.
 *
 * Simple app for launching the Cog debugger on a model restored from an HDF5 file.
 * This will be obsoleted when the debugger implements a "Load" button.
 *
 * Use this by entering the filename in the IntelliJ IDEA run config under "Program arguments".
 * The path can be absolute or relative to the launch directory, which is the top-level project
 * directory.
 */

object Restorer extends App {
  require(args.length > 0, "You must add a filename to the runtime args.")
  object debugger extends CogDebuggerApp(args(0))
  debugger.main(args.drop(1))
}
