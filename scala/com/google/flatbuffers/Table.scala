/*
 * Copyright 2014 Google Inc. All rights reserved.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package com.google.flatbuffers

import scala.collection.immutable._
import java.nio.ByteBuffer
import java.nio.ByteOrder

/// @cond FLATBUFFERS_INTERNAL

/**
 * All tables in the generated code derive from this class, and add their own accessors.
 */
trait Table {
  protected val bbPos: Int
  val bb: ByteBuffer
  private val vtableStart: Int
  private val vtableSize: Int

  protected def __offset(vtableOffset: Int): Option[Int] =
    if (vtable_offset < vtable_size)
      Some(__get[Short](vtable_start + vtable_offset, bb)).filter(_ != 0)
    else
      None

  protected def sortTables(offsets: List[Int], byteBuffer: ByteBuffer): List[Int] =
    offsets.sortWith(keysCompare(_, _, byteBuffer))

  protected def keysCompare(o1: Int, o2: Int, byteBuffer: ByteBuffer): Int = 0
}

object Table {
  protected def compareStrings(offset1: Int, offset2: Int, bb: ByteBuffer): Int = {
    val indirect1 = __indirect(offset1, bb)
    val indirect2 = __indirect(offset2, bb)
    val len1 = __get[Int](indirect1, bb)
    val len2 = __get[Int](indirect2, bb)
    val startPos1 = indirect1 + sizeOfInt
    val startPos2 = indirect2 + sizeOfInt
    val len = len1.min(len2)
    (0.to(len))
      .find(i => bb.get(i + startPos1) != bb.get(i + startPos2))
      .map(bb.get(i + startPos1) - bb.get(i + startPos2))
      .getOrElse(len1 - len2)
  }
}

/// @endcond
