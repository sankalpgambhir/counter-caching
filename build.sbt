val scala3Version = "3.2.2"

def chooseScalaZ3(scalaBinVersion: String): String = s"scalaz3-unix-64-3.jar"

Compile / unmanagedJars += {
      baseDirectory.value / "unmanaged" / chooseScalaZ3(scalaBinaryVersion.value)
    }

lazy val root = project
  .in(file("."))
  .settings(
    name := "counter-caching",
    version := "0.1.0-SNAPSHOT",

    scalaVersion := scala3Version,

    libraryDependencies += "org.scalameta" %% "munit" % "0.7.29" % Test,
  )
