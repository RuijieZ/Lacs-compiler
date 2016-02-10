scalaVersion := "2.11.7"

libraryDependencies += "org.scalatest" % "scalatest_2.11" % "2.2.5" % "test"

scalaSource in Compile := baseDirectory.value / "src"

scalaSource in Test := baseDirectory.value / "test" / "src"

artifactName in (Compile, packageSrc) := { (sv: ScalaVersion, module: ModuleID, artifact: Artifact) =>
    "for-marmoset.zip"
}

mappings in (Compile, packageSrc) := {
          (sources in Compile).value pair rebase(baseDirectory.value / "src", "src/")
}
