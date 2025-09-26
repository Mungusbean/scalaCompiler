val scala3Version = "3.7.3"

// Common settings for all subprojects
lazy val commonSettings = Seq(
  scalaVersion := scala3Version,
  scalacOptions ++= Seq("-feature", "-deprecation"),
  libraryDependencies ++= Seq(
    "org.scalameta" %% "munit" % "1.0.0" % Test,
    "org.scalatest" % "scalatest_3" % "3.2.10" % Test,
    "org.scalatest" % "scalatest-funsuite_3" % "3.2.10" % Test
  )
)

// ===== Subprojects =====

// Monad 
lazy val generalUtils = project
  .in(file("generalUtils"))
  .settings(commonSettings, name := "generalUtils")

// Lexer
lazy val lexer = project
  .in(file("lexer"))
  .settings(commonSettings, name := "lexer")
  .dependsOn(generalUtils)

// AST
lazy val ast = project
  .in(file("ast"))
  .settings(commonSettings, name := "ast")
  .dependsOn(generalUtils)

// Parser
lazy val parser = project
  .in(file("parser"))
  .settings(commonSettings, name := "parser")
  .dependsOn(lexer, ast, generalUtils)

// CLI (main entrypoint)
lazy val cli = project
  .in(file("cli")) // fixed!
  .settings(commonSettings, name := "cli")
  .dependsOn(lexer, ast, parser)

// Root aggregator
lazy val root = project
  .in(file("."))
  .aggregate(lexer, ast, parser, cli)
  .settings(
    name := "ScalaCompiler",
    version := "0.1.0-SNAPSHOT"
  )
