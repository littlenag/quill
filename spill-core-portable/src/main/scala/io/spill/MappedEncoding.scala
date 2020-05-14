package io.spill

case class MappedEncoding[I, O](f: I => O)
