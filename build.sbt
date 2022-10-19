enablePlugins(ScalaJSPlugin)

lazy val commonSettings = Seq(
  scalaVersion := "3.1.3"
)

lazy val root = (project in file("."))
  .settings(commonSettings, Seq(
    name := "TASTy interpreter",

    scalaJSUseMainModuleInitializer := true,
    scalaJSLinkerConfig ~= { _.withModuleKind(ModuleKind.CommonJSModule) },

    // this is locally published from LAMP/scala-js/tasty-query
    libraryDependencies += "tasty-query" %%% "tasty-query" % "0.1-SNAPSHOT"
      from "file://" + baseDirectory.value.getPath() + "/lib/tasty-query_sjs1_3.jar",
    libraryDependencies += "org.scalameta" %%% "munit" % "0.7.29" % Test,

    Test / sourceGenerators += Def.task {
      val file = (Test / sourceManaged).value / "generated" / "TestClasspaths.scala"
      val q = "\""
      val cpList = "List(" + (Compile / managedClasspath).value.seq
        .map( _.data.absolutePath)
        .map(s => s"${q}${s}${q}")
        .reduce((s1, s2) => s"${s1}, ${s2}") + ")"
      IO.write(file, s"""
package tastyinterpreter.generated

object TestClasspaths {
  val classpaths = ${cpList}
}
""")
      Seq(file)
    }
  ))
