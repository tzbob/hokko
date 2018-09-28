resolvers += "Sonatype OSS Snapshots" at
  "https://oss.sonatype.org/content/repositories/snapshots"

organization in ThisBuild := "be.tzbob"
scalaVersion in ThisBuild := "2.12.4"
crossScalaVersions in ThisBuild := Seq("2.11.12", "2.12.4")
version in ThisBuild := "0.4.9-SNAPSHOT"
isSnapshot in ThisBuild := true

scalacOptions in ThisBuild ++= Seq(
  "-encoding",
  "UTF-8",
  "-feature",
  "-deprecation",
  "-Xlint",
  "-Yno-adapted-args",
  "-Ywarn-dead-code",
  "-Ywarn-numeric-widen",
  "-Ywarn-value-discard",
  "-Xfuture",
  "-language:higherKinds",
  "-language:implicitConversions",
  "-Ypartial-unification"
)

lazy val root = project
  .in(file("."))
  .aggregate(hokkoJS, hokkoJVM, hokkoCollectionJS, hokkoCollectionJVM)

lazy val publishSettings = Seq(
  homepage := Some(url("https://github.com/Tzbob/hokko")),
  licenses := Seq(
    "MIT" -> url("https://opensource.org/licenses/mit-license.php")),
  publishMavenStyle := true,
  publishTo := {
    val nexus = "https://oss.sonatype.org/"
    if (isSnapshot.value)
      Some("snapshots" at nexus + "content/repositories/snapshots")
    else
      Some("releases" at nexus + "service/local/staging/deploy/maven2")
  },
  pomExtra :=
    <scm>
      <url>git@github.com:tzbob/hokko.git</url>
      <connection>scm:git:git@github.com:tzbob/hokko.git</connection>
    </scm>
      <developers>
        <developer>
          <id>tzbob</id>
          <name>Bob Reynders</name>
          <url>https://github.com/tzbob</url>
        </developer>
      </developers>
)

lazy val hokko = crossProject
  .in(file("core"))
  .settings(publishSettings: _*)
  .settings(
    name := "hokko",
    addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.4"),
    libraryDependencies ++= Seq(
      "org.typelevel" %%% "cats-core"   % "1.0.1",
      "org.typelevel" %%% "cats-effect" % "0.9",
      "org.scalatest" %%% "scalatest"   % "3.0.1" % "test",
      "biz.enef"      %%% "slogging"    % "0.5.3"
    )
  )
  .jvmSettings(
    libraryDependencies ++= Seq(
      "org.scalacheck" %% "scalacheck" % "1.13.4" % "test",
      "org.mockito"    % "mockito-all" % "1.8.5"  % "test"
    )
  )

lazy val hokkoJVM = hokko.jvm
lazy val hokkoJS  = hokko.js

lazy val hokkoCollection = crossProject
  .in(file("collection"))
  .settings(publishSettings: _*)
  .settings(
    name := "hokko-collection",
    libraryDependencies ++= Seq(
      "com.chuusai" %%% "shapeless" % "2.3.2"
    )
  )
  .dependsOn(hokko % "compile;test->test")

lazy val hokkoCollectionJVM = hokkoCollection.jvm
lazy val hokkoCollectionJS  = hokkoCollection.js
