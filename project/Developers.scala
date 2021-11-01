import sbt._

object Developers {
  val adpi2: Developer = Developer(
    "adpi2",
    "Adrien Piquerez",
    "adrien.piquerez@gmail.com",
    url("https://github.com/adpi2/")
  )

  val ericpeters = Developer(
    "ericpeters",
    "Eric Peters",
    "eric@peters.org",
    url("https://github.com/er1c")
  )

  val tdudzik = Developer(
    "tdudzik",
    "Tomasz Dudzik",
    "tdudzik@virtuslab.com",
    url("https://github.com/tdudzik")
  )

  val list: List[Developer] = List(adpi2, ericpeters, tdudzik)
}
