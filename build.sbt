lazy val root = project.in(file("."))
    .aggregate(scalismoad)
    .settings(commonSettings: _*)
    .settings(name := "scalismoad-root")
    .settings(publish := {})
    .settings(testSettings: _*)

lazy val scalismoad = project.in(file("scalismoad"))
    .settings(commonSettings: _*)
    .settings(name := "scalismoad")

lazy val example = project.in(file("example"))
    .dependsOn(scalismoad)
    .settings(name := "example")
    .settings(commonSettings: _*)

lazy val commonSettings = Seq(
    organization        := "ch.grigala",
    version             := "0.0.1-SNAPSHOT",
    scalaVersion        := "2.13.1",
    crossScalaVersions  := Seq("2.11.7"),
    scalacOptions       ++= commonScalacOptions,
    resolvers           ++= commonResolvers,
    libraryDependencies ++= commonLibraryDependencies
)

lazy val commonPublishSettings = Seq(
    licenses := Seq("Apache License Version 2.0" -> url("http://www.apache.org/licenses/LICENSE-2.0")),
    homepage := Some(url("http://kogecoo.github.com/scalaad")),
    publishMavenStyle := true,
    publishArtifact in Test := false,
    pomIncludeRepository := (_ => false),
    pomExtra := commonPomExtra,
    publishTo := version { (v: String) => choosePublishTo(v) }.value
)

lazy val commonScalacOptions = Seq(
    "-feature",
    "-deprecation",
    "-unchecked",
    "-language:postfixOps",
    "-Xlint",
    "-Ywarn-dead-code"
)

lazy val commonResolvers = Seq(
    "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots",
    "Sonatype release Repository" at "https://oss.sonatype.org/service/local/staging/deploy/maven2/",
    "Local Maven Repository" at "file://" + Path.userHome.absolutePath + "/.m2/repository/"
)

lazy val commonLibraryDependencies = Seq(
    "org.scalacheck" %% "scalacheck" % "1.14.1" % "test",
    "org.scalatest" %% "scalatest" % "3.2.5" % "test",
    "org.scalanlp" %% "breeze" % "1.0",
    "org.scalanlp" %% "breeze-natives" % "1.0",
    "ch.unibas.cs.gravis" % "scalismo-native-all" % "4.0.1",
    "ch.unibas.cs.gravis" %% "scalismo-ui" % "0.90.0",
)

lazy val commonPomExtra = {
    <scm>
        <url>git@github.com:kogecoo/scalaad.git</url>
        <connection>scm:git:git@github.com:kogecoo/scalaad.git</connection>
    </scm>
        <developers>
            <developer>
                <id>kogecoo</id>
                <name>kogecoo</name>
                <url>http://kogecoo.github.com</url>
            </developer>
        </developers>
}

lazy val testSettings = Seq(
    testOptions += Tests.Argument(TestFrameworks.ScalaCheck, "-verbosity", "2"),
    testOptions += Tests.Argument(TestFrameworks.ScalaTest, "-oDF")
)

def choosePublishTo(v: String) = {
    if (v.trim.endsWith("SNAPSHOT"))
        Some(Resolver.file("file", new File(Path.userHome.absolutePath + "/temporary")))
    else
        Some(Resolver.file("file", new File(Path.userHome.absolutePath + "/temporary")))
}
