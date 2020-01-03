name := "cvmorph"

version := "0.2"

scalaVersion := "2.13.1"

libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.8" % "test"

mainClass in assembly := Some("cvmorph.ChiselVerilogMain")

