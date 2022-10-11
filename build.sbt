enablePlugins(ScalaJSPlugin)

name := "TASTy interpreter"
scalaVersion := "3.1.3" // or any other Scala version >= 2.11.12

// This is an application with a main method
scalaJSUseMainModuleInitializer := true
scalaJSLinkerConfig ~= { _.withModuleKind(ModuleKind.CommonJSModule) }

// this is locally published from LAMP/scala-js/tasty-query
libraryDependencies += "tasty-query" %%% "tasty-query" % "0.1-SNAPSHOT"
