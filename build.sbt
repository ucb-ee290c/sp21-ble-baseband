name := "baseband"
organization := "edu.berkeley.cs"
version := "0.0.1"

scalaVersion := "2.12.12"

scalacOptions := Seq("-deprecation", "-unchecked", "-Xsource:2.11", "-language:reflectiveCalls")
libraryDependencies ++= Seq("edu.berkeley.cs" %% "chisel3" % "3.4-SNAPSHOT" ,
                            "edu.berkeley.cs" %% "rocketchip" % "1.2-SNAPSHOT",
                            "edu.berkeley.cs" %% "chiseltest" % "0.3.1",
                            "org.scalatest" %% "scalatest" % "3.2.+" % "test")

resolvers ++= Seq(
  Resolver.sonatypeRepo("snapshots"),
  Resolver.sonatypeRepo("releases"),
  Resolver.mavenLocal)
