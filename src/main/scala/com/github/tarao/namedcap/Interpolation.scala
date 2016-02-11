package com.github.tarao.namedcap

/** An interpolation for named patterns. */
class Interpolation(val sc: StringContext) extends AnyVal {
  /** Binds named patterns to their captured groups as a `Pattern`. */
  def pattern(args: Group*): Pattern = Pattern(sc.parts, args)
}
