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

package com.google

import java.nio.ByteBuffer

package object flatbuffers {
  private val utf8: Utf8 = Utf8.getDefault

  val sizeOfInt: Int = 4;

  implicit val booleanGetter: Getter[Boolean] = (o, bb) => bb.get(o) != 0
  implicit val byteGetter: Getter[Byte] = (o, bb) => bb.get(o)
  implicit val shortGetter: Getter[Short] = (o, bb) => bb.getShort(o)
  implicit val intGetter: Getter[Int] = (o, bb) => bb.getInt(o)
  implicit val longGetter: Getter[Long] = (o, bb) => bb.getLong(o)
  implicit val floatGetter: Getter[Float] = (o, bb) => bb.getFloat(o)
  implicit val doubleGetter: Getter[Double] = (o, bb) => bb.getDouble(o)
  implicit val stringGetter: Getter[String] = (o, bb) => {
    val stringLengthOffset = __indirect(o)
    val stringLength = __get[Int](stringLengthOffset)
    val stringDataOffset = stringLengthOffset + sizeOfInt
    utf8.decodeUtf8(bb, stringDataOffset, stringLength)
  }

  def __get[A](offset: Int, bb: ByteBuffer)(implicit getter: Getter[A]): A =
    getter.get(offset, bb)

  def __indirect(offset: Int, bb: ByteBuffer): Int =
    offset + __get[Int](offset, bb)
}
