package org.msgpack.template

import org.msgpack.unpacker.Unpacker
import org.msgpack.packer.Packer

/**
 * Created by adrian on 04/01/14.
 */
class UnitTemplate extends AbstractTemplate[Unit] {
  def write(pk: Packer, v: Unit, required: Boolean): Unit = {
    pk.writeNil()
  }

  def read(u: Unpacker, to: Unit, required: Boolean): Unit = {
    u.readNil()
  }
}
