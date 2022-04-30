ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.12.15"

lazy val ast = RootProject(file("../simple-ast-gen"))

lazy val root = Project(id = "nested-cycles", base = file(".")).dependsOn(ast)
