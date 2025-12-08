val scala3Version = "3.7.3"

// Common settings for all subprojects
lazy val commonSettings = Seq(
  scalaVersion := scala3Version,
  scalacOptions ++= Seq("-feature", "-deprecation"),
  libraryDependencies ++= Seq(
    "org.scalameta" %% "munit" % "1.0.0" % Test,
    "org.scalatest" % "scalatest_3" % "3.2.10" % Test,
    "org.scalatest" % "scalatest-funsuite_3" % "3.2.10" % Test,
    "org.ow2.asm" % "asm" % "9.6"
  )
)

// ===== Subprojects =====

// Monad, lattice, parsec
lazy val generalUtils = project
  .in(file("generalUtils"))
  .settings(commonSettings, name := "generalUtils")

// AST
lazy val ast = project
  .in(file("ast"))
  .settings(commonSettings, name := "ast")
  .dependsOn(generalUtils)

// Lexer
lazy val lexer = project
  .in(file("lexer"))
  .settings(commonSettings, name := "lexer")
  .dependsOn(generalUtils)

// Parser
lazy val parser = project
  .in(file("parser"))
  .settings(
    commonSettings,
    name := "parser",
    libraryDependencies ++= Seq(
      "org.scalactic" %% "scalactic" % "3.2.10"
    )
  )
  .dependsOn(lexer, ast, generalUtils)

// Ir
lazy val ir = project
  .in(file("ir"))
  .settings(
    commonSettings,
    name := "ir",
    libraryDependencies ++= Seq(
      "org.ow2.asm" % "asm" % "9.6",
      "org.ow2.asm" % "asm-util" % "9.6",
      "org.ow2.asm" % "asm-tree" % "9.6" 
    )
  )
  .dependsOn(generalUtils, ast)

// Semantic
lazy val semantic = project
  .in(file("semantic"))
  .settings(commonSettings, name := "semantic")
  .dependsOn(generalUtils, ir)

// back backend
lazy val backend = project
  .in(file("backend"))
  .settings(commonSettings, name := "backend")
  .dependsOn(generalUtils, ir, semantic)

// interpreter
lazy val interpreter = project
  .in(file("interpreter"))
  .settings(commonSettings, name := "interpreter")
  .dependsOn(generalUtils, ir, ast)

// CLI (main entrypoint)
lazy val cli = project
  .in(file("cli"))
  .settings(commonSettings, name := "cli")
  .dependsOn(
    generalUtils,
    lexer,
    parser,
    ast,
    semantic,
    ir,
    backend,
    interpreter
  )

// Root aggregator
lazy val root = project
  .in(file("."))
  .aggregate(lexer, ast, parser, cli, ir, semantic, interpreter, backend)
  .settings(
    name := "ScalaCompiler",
    version := "0.1.0-SNAPSHOT"
  )
