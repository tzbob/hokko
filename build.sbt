scalaVersion in ThisBuild := "2.11.8"

lazy val root = project
  .in(file("."))
  .aggregate(hokkoJS, hokkoJVM)
  .settings(
    publish := {},
    publishLocal := {}
  )

lazy val commonSettings = Seq(
  scalafmtConfig in ThisBuild := Some(file(".scalafmt.conf")),
  organization := "be.tzbob",
  version := "0.3.1-SNAPSHOT",
  autoCompilerPlugins := true,
  scalacOptions ++= Seq(
    "-encoding",
    "UTF-8",
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
    "-language:higherKinds",
    "-language:implicitConversions"
  ))

lazy val hokkoBase = crossProject
  .in(file("."))
  .settings(commonSettings: _*)
  .settings(
    name := "hokko",
    libraryDependencies ++= Seq(
      "org.typelevel" %%% "cats"      % "0.7.2",
      "org.scalatest" %%% "scalatest" % "3.0.0-M10" % "test"
    )
  )
  .jvmSettings(
    // Add JVM-specific settings here
    libraryDependencies ++= Seq(
      "org.scalacheck" %% "scalacheck" % "1.12.1" % "test",
      "org.mockito"    % "mockito-all" % "1.8.5"  % "test"
    )
  )
  .jsSettings(
    // Add JS-specific settings here
    libraryDependencies ++= Seq()
  )

lazy val hokkoJVM = hokkoBase.jvm
lazy val hokkoJS  = hokkoBase.js

lazy val hokkoCollectionBase = crossProject
  .crossType(CrossType.Pure)
  .in(file("collection"))
  .settings(commonSettings: _*)
  .settings(
    name := "hokko-collection"
  )
  .dependsOn(hokkoBase)

lazy val hokkoCollectionJVM = hokkoCollectionBase.jvm
lazy val hokkoCollectionJS = hokkoCollectionBase.js
