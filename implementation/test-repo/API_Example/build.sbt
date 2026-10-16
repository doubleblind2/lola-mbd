name := "example-project"

version := "0.1"

scalaVersion := "2.13.12"

// Comment out the resolvers and libraryDependencies and uncomment unmanagedJars to use the assembled fat jar
//resolvers ++= Seq(
//  "ISP Releases"       at "https://sourcecode.isp.uni-luebeck.de/nexus/content/repositories/releases",
//  "ISP Snapshots"      at "https://sourcecode.isp.uni-luebeck.de/nexus/content/repositories/snapshots",
//  "efficios-releases"  at "https://mvn.efficios.com/repository/releases",
//  "efficios-snapshots" at "https://mvn.efficios.com/repository/snapshots"
//)
//libraryDependencies += "de.uni_luebeck.isp" %% "unaa-lola" % "0.3"

unmanagedJars in Compile += file("../../bin/unaa-lola.jar")
