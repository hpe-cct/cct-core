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

import java.io.File

/** Silently ensure that a directory is absent. Use as in
  * DeleteDirectory("dirToNuke")
  *
  * User: Dick Carter
  * Date: 5/2/12
  */

private [cogx] object DeleteDirectory {
  
  private def deleteDir(dirFile: File) {
    if (dirFile.exists()) {
      dirFile.listFiles().foreach( file => {
        if (file.isDirectory)
          deleteDir(file)
        else
          file.delete()
      })
      dirFile.delete()
    }
  }
  
  def apply(dir: String) { deleteDir(new File(dir)) }

}
