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

/**
 * Created by Dick Carter on 10/2/14.
 *
 * Adds a string to the name of the thread as seen by profilers and other tools like jconsole
 */
object AnnotateThread {
  /** Add nameSuffix to the current thread name as seen in profilers */
  def apply(nameSuffix: String) {
    val thread = Thread.currentThread()
    val currentName = thread.getName()
    thread.setName(currentName + ": " + nameSuffix)
  }
}
