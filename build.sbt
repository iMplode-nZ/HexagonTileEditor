scalaVersion := "0.22.0"
libraryDependencies += "org.jline" % "jline" % "3.1.3"
scalaSource in Compile := baseDirectory.value / "src"
mainClass in (Compile, run) := Some("Main")