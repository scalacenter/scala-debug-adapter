import java.nio.file._

Global / onLoad := { state =>
  val prePush = Paths.get(".git", "hooks", "pre-push")
  if (!scala.util.Properties.isWin && !Files.exists(prePush)) {
    import java.nio.file._
    Files.createDirectories(prePush.getParent)
    Files.write(
      prePush,
      """#!/bin/sh
        |set -eux
        |bin/scalafmt --diff --diff-branch main
        |git diff --exit-code --ignore-submodules=all
        |""".stripMargin.getBytes()
    )
    prePush.toFile.setExecutable(true)
  }
  state
}
