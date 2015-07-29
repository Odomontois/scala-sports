scalaVersion := "2.11.7"

resolvers += Resolver.sonatypeRepo("releases")

fork in run := true

//baseDirectory in run := baseDirectory.value / "working"

libraryDependencies ++= Seq(
  "com.chuusai" %% "shapeless" % "2.2.3",
  "org.scalaz" %% "scalaz-core" % "7.1.1",
  "org.typelevel" %% "scalaz-spire" % "0.2",
  "com.twitter" %% "finagle-http" % "6.25.0",
  "org.spire-math" %% "spire" % "0.9.1" ,
  "com.typesafe.akka" %% "akka-actor" % "2.3.11",
  "com.lihaoyi" %% "ammonite-ops" % "0.3.2")

libraryDependencies += "org.scala-lang" % "scala-reflect" % "2.11.1"

libraryDependencies += "com.github.nscala-time" %% "nscala-time" % "2.0.0"

libraryDependencies += "com.github.melrief" %% "purecsv" % "0.0.2"