import sbt._
import Keys._

import org.scalajs.sbtplugin.cross.CrossProjectExtra

object HokkoBuild extends Build with CrossProjectExtra {

  override lazy val settings = super.settings ++ Seq(
    isSnapshot := true,
    organization := "hokko",
    version := "0.3-SNAPSHOT",

    scalaVersion := "2.11.2",

    resolvers ++= Seq(
      Resolver.sonatypeRepo("snapshots")
    ),

    scalacOptions ++= Seq(
      "-encoding", "UTF-8",
      "-target:jvm-1.6",
      "-feature",
      "-deprecation",
      "-Xlint",
      "-Yinline-warnings",
      "-Yno-adapted-args",
      "-Ywarn-dead-code",
      "-Ywarn-numeric-widen", // bad implicit widening somewhere
      "-Ywarn-value-discard", // will require a lot of work
      "-Xfuture"
    )
  )

  lazy val root = project.in(file("."))
    .aggregate(hokkoJS, hokkoJVM)

  lazy val hokko = crossProject.in(file("."))
    .jvmSettings(
    // Add JVM-specific settings here
    scalaOrganization := "org.scala-lang.virtualized",
    libraryDependencies ++= Seq(
      compilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full),
      "com.github.tzbob" %% "scala-js-2-js-scala" % "0.1-SNAPSHOT" changing(),
      "org.scala-js" %% "scalajs-stubs" % "0.6.0",
      "org.scalatest" %% "scalatest" % "2.2.1" % "test",
      "org.scalacheck" %% "scalacheck" % "1.12.1" % "test",
      "org.mockito" % "mockito-all" % "1.8.5" % "test"
    )
  )
    .jsSettings(
    // Add JS-specific settings here
    libraryDependencies ++= Seq(
      // This runtime has to be included whenever scala-js-2-js-scala is used
      "com.github.tzbob" % "scala-js-2-js-scala-runtime_sjs0.6_2.11" % "0.1-SNAPSHOT" changing()
    )
  )

  lazy val hokkoJVM = hokko.jvm
  lazy val hokkoJS = hokko.js
}
