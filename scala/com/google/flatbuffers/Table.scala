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
  val bbPos: Int
  val bb: ByteBuffer
  val vtableStart: Int = bbPos - __get[Int](bbPos, bb)
  val vtableSize: Int = __get[Short](vtableStart, bb)

  protected def __offset(vtableOffset: Int): Option[Int] =
    if (vtableOffset < vtableSize)
      Some(__get[Short](vtableStart + vtableOffset, bb).toInt).filter(_ != 0)
    else
      None

  protected def sortTables(offsets: List[Int], byteBuffer: ByteBuffer): List[Int] =
    offsets.sortWith(keysCompare(_, _, byteBuffer) > 0)

  protected def keysCompare(o1: Int, o2: Int, byteBuffer: ByteBuffer): Int = 0

  def __vector[A](offset: Int, bb: ByteBuffer, elemSize: Int)(implicit getter: Getter[A]): Seq[A] =
    __offset(offset).map(_ + bbPos) match {
      case Some(vectorOffset) =>
        new Seq[A] {
          private val vectorLengthOffset = __indirect(vectorOffset, bb)
          private val vectorLength = __get[Int](vectorLengthOffset, bb)
          private val vectorDataOffset = vectorLengthOffset + sizeOfInt

          def apply(i: Int): A =
            if (i < 0 || i >= length)
              throw new IndexOutOfBoundsException(i)
            else
              __get[A](vectorDataOffset + i * elemSize, bb)
          def length: Int = vectorLength
          def iterator: Iterator[A] = Iterator.tabulate(vectorLength)(i => apply(i))
        }

      case None => Nil;
    }

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
    0.to(len)
      .find(i => bb.get(i + startPos1) != bb.get(i + startPos2))
      .map(i => bb.get(i + startPos1) - bb.get(i + startPos2))
      .getOrElse(len1 - len2)
  }
}

/// @endcond
