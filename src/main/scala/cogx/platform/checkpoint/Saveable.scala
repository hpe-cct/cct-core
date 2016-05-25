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
  * For non-primitive objects that must be saved (i.e. not Ints, Floats, Strings, etc.),
  * this trait defines the requirements of the object.  Basically, the object must save
  * itself using the capabilities of the provided ObjectSaver.
  *
  * Unlike with Java-serialization, which stores all members by default (unless they are marked transient),
  * we assume here that Saveables with make explicit calls to write their essential fields.  This should be
  * partnered with a RestoreFactory that can recreate the object from those essential fields.
  */
trait Saveable {
  def save(saver: ObjectSaver)
}
