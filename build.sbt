scalaVersion := "2.11.6"

fork in run := true

baseDirectory in run := baseDirectory.value / "working"

scalacOptions += "-target:jvm-1.7"

libraryDependencies ++= Seq(
  "org.scalaz" %% "scalaz-core" % "7.1.1",
//  "org.typelevel" %% "scalaz-spire" % "0.2",
  "com.twitter" %% "finagle-http" % "6.25.0",
  "org.spire-math" %% "spire" % "0.9.1" ,
  "com.typesafe.akka" %% "akka-actor" % "2.3.11")

libraryDependencies += "org.scala-lang" % "scala-reflect" % "2.11.1"
