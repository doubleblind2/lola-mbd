val nexus = "https://sourcecode.isp.uni-luebeck.de/nexus/"
val snapshots = "ISP Snapshots" at nexus + "content/repositories/snapshots"
val releases = "ISP Releases" at nexus + "content/repositories/releases"
val efficiosSnapshots = "efficios-snapshots" at "https://mvn.efficios.com/repository/snapshots"
val efficiosReleases = "efficios-releases" at "https://mvn.efficios.com/repository/releases"

name := "unaa-lola"

version := "0.3"

scalaVersion := "2.13.12"

organization := "de.uni_luebeck.isp"

resolvers ++= Seq(
  releases, snapshots, efficiosSnapshots, efficiosReleases
)

enablePlugins(Antlr4Plugin)

Antlr4 / antlr4Version := "4.13.1"
Antlr4 / antlr4GenListener := false
Antlr4 / antlr4GenVisitor := true
Antlr4 / antlr4PackageName := Some("unaa_lola.antlr4")

libraryDependencies ++= Seq(
  "de.uni_luebeck.isp" % "z3bundle" % "4.8.15",
  "org.scalatest" %% "scalatest" % "3.2.9" % "test",
  "org.scalanlp" %% "breeze" % "1.2",
  "org.scala-lang.modules" %% "scala-parser-combinators" % "2.0.0",
  "com.github.scopt" %% "scopt" % "4.0.1"
)

