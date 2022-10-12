enablePlugins(ScalaJSPlugin)

lazy val commonSettings = Seq(
  scalaVersion := "3.1.3"
)

/*
Test / unmanagedSources / excludeFilter := HiddenFileFilter || {
  val testinputs = ((Test / sourceDirectory).value / "scala" / "testinputs").getCanonicalPath
  new SimpleFileFilter(_.getCanonicalPath startsWith testinputs)
}
*/

lazy val root = (project in file("."))
  .dependsOn(testinputs % "test")       // root/test depends on testinputs/compile
  .settings(commonSettings, Seq(
    name := "TASTy interpreter",

    scalaJSUseMainModuleInitializer := true,
    scalaJSLinkerConfig ~= { _.withModuleKind(ModuleKind.CommonJSModule) },

    // this is locally published from LAMP/scala-js/tasty-query
    libraryDependencies += "tasty-query" %%% "tasty-query" % "0.1-SNAPSHOT"
      from "file://" + baseDirectory.value.getPath() + "lib/tasty-query_sjs1_3.jar",
    libraryDependencies += "org.scalameta" %%% "munit" % "0.7.29" % Test,
  ))

lazy val testinputs = (project in file("src/test/resources/testinputs"))
  .settings(commonSettings, Seq(
    name := "test inputs for TASTy interpreter",
  ))
