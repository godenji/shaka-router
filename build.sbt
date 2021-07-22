lazy val root = project
  .in(file("."))
  .enablePlugins(ScalaJSPlugin, ScalaJSJUnitPlugin)
  .settings(
    organization := "org.getshaka",
    name := "shaka-router",
    version := "0.2.2-SNAPSHOT",
    versionScheme := Some("early-semver"),
    scalaVersion := "3.0.1",
    libraryDependencies ++= Seq(
      "org.getshaka" %%% "shaka" % "0.3.1-SNAPSHOT",
      "org.scala-js" %%% "scalajs-dom" % "1.2.0-SNAPSHOT"
    ),

    // publishing settings
    homepage := Some(url("https://github.com/getshaka-org/shaka-router")),
    licenses += ("Apache-2.0", url("https://opensource.org/licenses/Apache-2.0")),
    scmInfo := Some(ScmInfo(
      url("https://github.com/getshaka-org/shaka-router"),
      "scm:git:git@github.com:getshaka-org/shaka-router.git",
      Some("scm:git:git@github.com:getshaka-org/shaka-router.git")
    )),
    developers := List(
      Developer(
        id = "augustnagro@gmail.com",
        name = "August Nagro",
        email = "augustnagro@gmail.com",
        url = url("https://augustnagro.com")
      )
    ),
    publishMavenStyle := true,
    Test / publishArtifact := false,
    pomIncludeRepository := { _ => false },
    publishTo := {
      val nexus = "https://s01.oss.sonatype.org/"
      if (isSnapshot.value)
        Some("snapshots" at nexus + "content/repositories/snapshots")
      else
        Some("releases" at nexus + "service/local/staging/deploy/maven2")
    },
    credentials += Credentials(Path.userHome / ".sbt" / ".credentials")
  )
