import sbtassembly.AssemblyPlugin
import sbtassembly.AssemblyPlugin.autoImport._

enablePlugins(AssemblyPlugin)

assembly / mainClass := Some("cli.Main")
assembly / assemblyJarName := "ScalaCompiler.jar"

assembly / assemblyMergeStrategy := {
  case PathList("module-info.class") =>
    MergeStrategy.discard

  case PathList("META-INF", xs @ _*) =>
    MergeStrategy.discard

  case _ =>
    MergeStrategy.first
}
