scalaVersion := "2.11.6"

fork in run := true

baseDirectory in run := baseDirectory.value / "working"

scalacOptions += "-target:jvm-1.7"

libraryDependencies += "org.scalaz" %% "scalaz-core" % "7.1.1"