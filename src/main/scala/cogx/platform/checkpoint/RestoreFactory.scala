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

package cogx.platform.checkpoint

/**
  * Created by Dick Carter on 1/16/2015.
  *
  * For non-primitive objects that must be restored (i.e. not Ints, Floats, Strings, etc.),
  * this trait defines the requirements of the factory object doing the restoring.  Basically,
  * the object must create an object instance using the capabilities of the provided ObjectRestorer.
  */
trait RestoreFactory {
  def restore(restorer: ObjectRestorer): AnyRef
}
