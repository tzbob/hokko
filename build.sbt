scalaVersion in ThisBuild := "2.11.8"
version in ThisBuild := "0.4.0-SNAPSHOT"

scalafmtConfig in ThisBuild := Some(file(".scalafmt.conf"))

lazy val root = project
  .in(file("."))
  .aggregate(hokkoJS, hokkoJVM, hokkoCollectionJS, hokkoCollectionJVM)
  .settings(
    publish := {},
    publishLocal := {}
  )

lazy val publishSettings = Seq(
  homepage := Some(url("https://github.com/Tzbob/scalatags-hokko")),
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
      <url>git@github.com:Tzbob/scalatags-hokko.git</url>
      <connection>scm:git:git@github.com:Tzbob/scalatags-hokko.git</connection>
    </scm>
      <developers>
        <developer>
          <id>tzbob</id>
          <name>Bob Reynders</name>
          <url>https://github.com/Tzbob</url>
        </developer>
      </developers>
)

lazy val commonSettings = Seq(organization := "be.tzbob",
                              autoCompilerPlugins := true,
                              scalacOptions ++= Seq(
                                "-encoding",
                                "UTF-8",
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

lazy val hokko = crossProject
  .in(file("."))
  .settings(commonSettings: _*)
  .settings(publishSettings: _*)
  .settings(
    name := "hokko",
    version := "0.4.0-SNAPSHOT",
    libraryDependencies ++= Seq(
      "org.typelevel" %%% "cats"      % "0.7.2",
      "org.scalatest" %%% "scalatest" % "3.0.1" % "test"
    )
  )
  .jvmSettings(
    // Add JVM-specific settings here
    libraryDependencies ++= Seq(
      "org.scalacheck" %% "scalacheck" % "1.13.4" % "test",
      "org.mockito"    % "mockito-all" % "1.8.5"  % "test"
    )
  )
  .jsSettings(
    // Add JS-specific settings here
    libraryDependencies ++= Seq()
  )

lazy val hokkoJVM = hokko.jvm
lazy val hokkoJS  = hokko.js

lazy val hokkoCollection = crossProject
  .in(file("collection"))
  .settings(commonSettings: _*)
  .settings(
    name := "hokko-collection",
    version := "0.4.0-SNAPSHOT",
    libraryDependencies ++= Seq(
      "com.chuusai" %%% "shapeless" % "2.3.2"
    )
  )
  .dependsOn(hokko % "compile;test->test")

lazy val hokkoCollectionJVM = hokkoCollection.jvm
lazy val hokkoCollectionJS  = hokkoCollection.js

lazy val hokkoBench = crossProject
  .in(file("benchmark"))
  .settings(commonSettings: _*)
  .settings(
    name := "benchmark",
    version := "0.4.0-SNAPSHOT",
    libraryDependencies ++= Seq(
      "com.storm-enroute" %% "scalameter" % "0.7"
    ),
    testFrameworks += new TestFramework("org.scalameter.ScalaMeterFramework"),
    parallelExecution in Test := false
  )
  .dependsOn(hokko, hokkoCollection)

lazy val hokkoBenchJVM = hokkoBench.jvm
lazy val hokkoBenchJS = hokkoBench.js
