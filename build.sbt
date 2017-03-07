lazy val root = (project in file (".")).
settings (
    name := "ScalaPuzzles",
    libraryDependencies += "org.scala-lang" % "scala-swing" % "2.10+",
	ivyScala := ivyScala.value map { _.copy(overrideScalaVersion = true) }
)
