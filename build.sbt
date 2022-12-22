enablePlugins(ScalaJSPlugin)

val rtJarOpt = taskKey[Option[String]]("Path to rt.jar if it exists")
val javalibEntry = taskKey[String]("Path to rt.jar or \"jrt:/\"")

lazy val commonSettings = Seq(
  scalaVersion := "3.1.3",
  Global / onChangedBuildSource := ReloadOnSourceChanges
)

lazy val root = (project in file("."))
  .settings(commonSettings, Seq(
    name := "TASTy interpreter",

    scalaJSUseMainModuleInitializer := true,
    scalaJSLinkerConfig ~= { _.withModuleKind(ModuleKind.CommonJSModule) },

    libraryDependencies += "ch.epfl.scala" %%% "tasty-query" % "0.5.1+20-032ebf85-SNAPSHOT",
    libraryDependencies += "org.scalameta" %%% "munit" % "0.7.29" % Test,

    Test / sourceGenerators += Def.task {
      val file = (Test / sourceManaged).value / "generated" / "TestClasspaths.scala"
      val q = "\""
      val cpList = "List(" + (Compile / managedClasspath).value.seq
        .map(_.data.absolutePath)
        .+:((Test / javalibEntry).value)
        .map(s => s"${q}${s}${q}")
        .reduce((s1, s2) => s"${s1}, ${s2}") + ")"
      IO.write(file, s"""
package tastyinterpreter.generated

object TestClasspaths {
  val classpaths = ${cpList}
}
""")
      Seq(file)
    },

    Test / rtJarOpt := {
      for (bootClasspath <- Option(System.getProperty("sun.boot.class.path"))) yield {
        val rtJarOpt = bootClasspath.split(java.io.File.pathSeparatorChar).find { path =>
          new java.io.File(path).getName() == "rt.jar"
        }
        rtJarOpt match {
          case Some(rtJar) =>
            rtJar
          case None =>
            throw new AssertionError(s"cannot find rt.jar in $bootClasspath")
        }
      }
    },

    Test / javalibEntry := {
      val rtJar = (Test / rtJarOpt).value

      val s = streams.value
      val targetRTJar = target.value / "extracted-rt.jar"

      rtJar.getOrElse {
        if (!targetRTJar.exists()) {
          s.log.info(s"Extracting jrt:/modules/java.base/ to $targetRTJar")
          extractRTJar(targetRTJar)
        }
        targetRTJar.getAbsolutePath()
      }
    },
  ))


def extractRTJar(targetRTJar: File): Unit = {
  import java.io.{IOException, FileOutputStream}
  import java.nio.file.{Files, FileSystems}
  import java.util.zip.{ZipEntry, ZipOutputStream}

  import scala.jdk.CollectionConverters._
  import scala.util.control.NonFatal

  val fs = FileSystems.getFileSystem(java.net.URI.create("jrt:/"))

  val zipStream = new ZipOutputStream(new FileOutputStream(targetRTJar))
  try {
    val javaBasePath = fs.getPath("modules", "java.base")
    Files.walk(javaBasePath).forEach({ p =>
      if (Files.isRegularFile(p)) {
        try {
          val data = Files.readAllBytes(p)
          val outPath = javaBasePath.relativize(p).iterator().asScala.mkString("/")
          val ze = new ZipEntry(outPath)
          zipStream.putNextEntry(ze)
          zipStream.write(data)
        } catch {
          case NonFatal(t) =>
            throw new IOException(s"Exception while extracting $p", t)
        }
      }
    })
  } finally {
    zipStream.close()
  }
}
