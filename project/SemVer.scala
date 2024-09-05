object SemVer {
  def apply(version: String): (Int, Int, Int) =
    "(\\d+)\\.(\\d+)\\.(\\d+)(?:-.*)?".r.unapplySeq(version).map(xs => (xs(0).toInt, xs(1).toInt, xs(2).toInt)).get

  def matches(version: String)(f: PartialFunction[(Int, Int, Int), Boolean]): Boolean =
    f.lift(SemVer(version)).getOrElse(false)
}
