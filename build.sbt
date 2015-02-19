name := "hokko root project"

isSnapshot := true

lazy val root = project.in(file(".")).
  aggregate(hokkoJS, hokkoJVM).
  settings(
    publish := {},
    publishLocal := {}
  )

lazy val hokko = crossProject.in(file(".")).
  settings(
    name := "hokko",
    version := "0.1-SNAPSHOT",
    organization := "com.github.tzbob",
    scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature")
  ).
  jvmSettings(
    // Add JVM-specific settings here
    scalaOrganization := "org.scala-lang.virtualized",
    scalaVersion := "2.10.2",
    libraryDependencies ++= Seq(
      compilerPlugin("org.scalamacros" % "paradise" % "2.1.0-M5" cross CrossVersion.full),
      "com.github.tzbob" %% "scala-js-2-js-scala" % "0.1-SNAPSHOT" changing(),
      "org.scala-js" %% "scalajs-stubs" % "0.6.0",
      "org.scalatest" %% "scalatest" % "2.2.1" % "test",
      "org.scalacheck" %% "scalacheck" % "1.12.1" % "test",
      "org.mockito" % "mockito-all" % "1.8.5" % "test"
    )
  ).
  jsSettings(
    // Add JS-specific settings here
    scalaVersion := "2.10.4"
  )

lazy val hokkoJVM = hokko.jvm
lazy val hokkoJS = hokko.js
