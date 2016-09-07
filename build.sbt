scalaVersion in ThisBuild := "2.11.8"

lazy val root = project.in(file("."))
  .aggregate(hokkoJS, hokkoJVM)
  .settings(
    publish := {},
    publishLocal := {}
  )

lazy val hokko = crossProject.in(file("."))
  .settings(
    scalafmtConfig in ThisBuild := Some(file(".scalafmt")),
    organization := "be.tzbob",
    name := "hokko",
    version := "0.2",

    autoCompilerPlugins := true,

    scalacOptions ++= Seq(
      "-encoding", "UTF-8",
      "-target:jvm-1.6",
      "-feature",
      "-deprecation",
      "-Xlint",
      "-Yinline-warnings",
      "-Yno-adapted-args",
      "-Ywarn-dead-code",
      "-Ywarn-numeric-widen",
      "-Ywarn-value-discard",
      "-Xfuture",
      "-language:higherKinds"
    ),
    libraryDependencies ++= Seq(
      "org.typelevel" %%% "cats" % "0.7.2",
      "org.scalatest" %%% "scalatest" % "3.0.0-M10" % "test"
    )
  )
  .jvmSettings(
    // Add JVM-specific settings here
    libraryDependencies ++= Seq(
    )
  )
  .jsSettings(
    // Add JS-specific settings here
    libraryDependencies ++= Seq()
  )

lazy val hokkoJVM = hokko.jvm
lazy val hokkoJS = hokko.js
