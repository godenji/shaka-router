lazy val root = project
  .in(file("."))
  .enablePlugins(ScalaJSPlugin, ScalaJSJUnitPlugin)
  .settings(
    organization := "org.getshaka",
    name := "shaka-router",
    version := "0.1.1",
    versionScheme := Some("early-semver"),

    scalaVersion := "3.0.0-RC1",
    scalacOptions ++= Seq(
      "-Ycheck-init",
      "-Yindent-colons"
    ),

    libraryDependencies ++= Seq(
      "org.getshaka" %%% "shaka" % "0.2.0"
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
    publishArtifact.in(Test) := false,
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