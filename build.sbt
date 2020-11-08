scalaVersion := "2.11.12"

// Set to false or remove if you want to show stubs as linking errors
nativeLinkStubs := true

enablePlugins(ScalaNativePlugin)

//libraryDependencies += "org.scala-lang.modules" %%% "scala-parser-combinators" % "1.1.2"
libraryDependencies += "tech.sparse" %%%  "cmark-scala" % "0.1.0-SNAPSHOT"
libraryDependencies += "com.lihaoyi" %%% "utest" % "0.7.2" % "test"
testFrameworks += new TestFramework("utest.runner.Framework")
