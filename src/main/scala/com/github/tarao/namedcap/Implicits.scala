package com.github.tarao.namedcap

trait Implicits {
  implicit val patternToGroup: Pattern => Group = UnnamedGroup(_)

  implicit val patternInterpolation: StringContext => Interpolation =
    new Interpolation(_)
}
object Implicits extends Implicits
