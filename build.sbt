name := "hokko"
version := "0.1-SNAPSHOT"
scalaVersion := "2.10.4"

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "2.2.1" % "test",
  "org.scalacheck" %% "scalacheck" % "1.12.1" % "test",
  "org.scalaz" %% "scalaz-core" % "7.1.0",
  "com.chuusai" % "shapeless_2.10.4" % "2.0.0"
)

scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature")
