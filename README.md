## sbt project compiled with Scala 3

# Introduction and Overview
This project implements a miniature compiler pipeline for a simplified programming language called SIMP, based on the labs originally provided in the Ccompiler construction modules.

However, the structure here is heavily refactored and modularised compared to the original project template. As we did not realise that the project was meant to be pulled and used as is.

The project now follows a multi-module SBT layout, where each logical subsystem (lexer, parser, interpreter, semantic analysis, IR generation, backend, etc.) lives in its own directory, with its own tests, dependencies, and namespaces.

This was done for clarity and make (hopefully) make how the entire compiler pipeline operates from tokenisation → parsing → interpretation → IR → JVM backend.

# Project Directory

This project's structure has been modified from the original Simp project. Below is the detailed modified tree that we created from the project. Comments have been added to indicate which files corresponds to each Lab submission. 

```
scalaCompiler (Root)
│
├───ast
│   └───src
│       ├───main
│       │   └───scala
│       │       └───astUtils
│       │           └───AST.scala
│       └───test
│           └───scala
│               └───MySuite.scala
│
├───backend
│   └───src
│       ├───main
│       │   └───scala
│       │       └───backendJVM
│       │           └───JVM.scala
│       └───test
│           └───scala
│               └───MySuite.scala
│
├───cli
│   └───src
│       ├───main
│       │   └───scala
│       │       └───cli
│       │           └───Main.scala
│       ├───test
│       │   └───scala
│       │       └───MySuite.scala
│       └───assembly.sbt
│
├───generalUtils
│   └───src
│       ├───main
│       │   └───scala
│       │       ├───lattice
│       │       │   └───CompleteLattice.scala
│       │       │   └───SignLattice.scala
│       │       ├───monad
│       │       │   ├───error
│       │       │   │   ├───EitherStringMonadError.scala
│       │       │   │   └───StateTMonadError.scala
│       │       │   ├───Applicative.scala
│       │       │   ├───Functor.scala
│       │       │   ├───Monad.scala
│       │       │   └───StateT.scala
│       │       └───parsec
│       │           ├───BacktrackParsec.scala // (Unused)
│       │           └───Parsec.scala
│       └───test
│           └───scala
│               └───MySuite.scala
│
├───interpreter // (Lab 2)
│   └───src
│       ├───main
│       │   └───scala
│       │       └───interpreter
│       │           ├───Paint.scala
│       │           └───SimpInt.scala
│       └───test
│           └───scala //(Lab 2 test cases for SimpInt)
│               ├───MySuite.scala
│               └───SimpIntSuite.scala
│
├───ir // (Lab 3)
│   └───src
│       ├───main
│       │   └───scala
│       │       └───ir
│       │           ├───CFG.scala
│       │           ├───DF.scala
│       │           ├───MaximalMunch.scala
│       │           ├───MMUpDown.scala
│       │           ├───PseudoAssembly.scala
│       │           ├───SSA.scala
│       │           └───Util.scala
│       └───test
│           └───scala
│               └───MySuite.scala // (Lab 3 test cases)
│
├───lexer // (Lab 1)
│   └───src
│       ├───main
│       │   └───scala
│       │       └───lexer // (This has been modified from the original project)
│       │           ├───Lexer.scala
│       │           ├───LexerError.scala
│       │           ├───Tokens.scala
│       │           └───TokensUtils.scala
│       └───test // (Lab 1 test cases for lexer)
│           └───scala
│               └───MySuite.scala
│
├───parser // (Lab 1)
│   └───src
│       ├───main
│       │   └───scala
│       │       └───parser
│       │           └───Parser.scala
│       └───test // (Lab 1 test cases for Parser)
│           └───scala
│               └───MySuite.scala
│
├───semantic // (Lab 2 & lab 3)
│   └───src
│       ├───main
│       │   └───scala
│       │       └───semantic
│       │           ├───LivenessAnalysis.scala 
│       │           ├───SignAnalysis.scala
│       │           ├───TypeInf.scala
│       │           └───Util.scala
│       └───test //(Lab 2 test cases)
│           └───scala
│               ├───MySuite.scala
│               └───TestLivenessAnalysis.scala (Lab 3)
├───project
├───target
├───.gitignore
├───build.sbt (modified from original project)
└───README.md
```

# Main Modifications (Lexer)

**Overview of changes**

Apart from the structural change one of the main modifications made is to the lexer component of the project. The original uses composable Parser combinators to parse the lexemes defined in the project. This modified version implements a deterministic finite automaton (DFA)–style maximal-munch lexer.

This means the lexer reads characters one by one, always extending the largest possible valid lexeme, and only emits a token once the next character would make the current string invalid.

Because of this design, adding new lexemes no longer requires writing any new Parser combinators.
Instead, all behaviour is driven by dictionary entries and token rules, making the lexer significantly easier to extend.

The DFA code snippet can be found below:

```scala
    def lexChar(using pe: ParserEnv[LEnv, Char]): Parser[LEnv, (Char, Int, Int)] =
      for {
        c  <- sat((c: Char) => isValidLexemeChar(c))
        ln <- get(lenv => pe.getLine(lenv))
        cl <- get(lenv => pe.getCol(lenv))
      } yield (c, ln, cl)

    def lex( acc: String = "", lastValid: Option[LToken] = None)(using pe: ParserEnv[LEnv, Char]): Parser[LEnv, List[LToken]] =
        given curr_acc:String = acc
        for {
          maybeC <- lookAhead(optional(item))
          tokens <- maybeC match {
            // valid: now actually consume
            case Right(c: Char) if isValidToken(c) => for {
                (c1, ln, cl) <- lexChar // consume
                rest <- lex(acc + c1, Some(emitToken(ln, cl, c1))) // update acc and store last valid token. Continue munching
              } yield rest

            // invalid extension
            case Right(c: Char) =>
              lastValid match {
                case None      => lexError("", (_, env) => UnexpectedChar(c, env)) // catastrophic failure // TODO find a way to get the LEnv into there for the current line
                case Some(UnResolvedTok(srcloc, lexeme, mkError)) => lexError(lexeme, mkError) // encountered an invalid exstension and our last token was unresolved TODO: find a way to change the error
                case Some(tok) => for { rest <- lex("", None) } yield tok :: rest // actually prepend the token
              }

            // EOF
            case Left(_) =>
              lastValid match {
                case None      => lexError("", (_, env) => UnexpectedChar('\u0000', env)) // catastrophic failure
                case Some(UnResolvedTok(srcloc, lexeme, mkError)) => lexError(lexeme, mkError) // we reached EOF and our last token couldn't be resolved
                case Some(tok) => empty(List(tok)) 
              }
          }
        } yield tokens
```

**Extending the lexer**

As mentioned earlier, unlike the original SIMP project, where tokens were recognised and lexed using parser combinators. This lexer uses a DFA, so instead of creating new parsers for a new lexeme the new steps to adding in new lexemes are as follows:

1. **Fixed Length Lexemes**

For parsing lexemes with a fixed length (operators, punctuation, keywords and delimiters). To add in a new symbol:

**Step 1**: Add token constructor in *LToken.scala*

'''scala
case PipeSign(src: SrcLoc)
'''
**Step 2**: Add it to the appropriate dictionary in *TokensUtils.scala*

The available dictionaries predefined are:
- Whitespaces
- Operators
- Keywords
- Delimiters
- Punctuation

```scala
val OPERATORS = Map(
    // ...
    "|" -> (src => PipeSign(src))
)
```

**Step 3**: Ensure the *isValidLexemeChar* is includes the new symbol

This can improved by automatically taking the union of all valid characters in the defined dictionaries instead of manually putting this in.

```scala
val isValidLexemeChar: Char => Boolean = (c: Char) =>
  c.isLetterOrDigit || // allow any letter or digit
  c == '_' || // allow underscore
  "+-*/%=<>!&|^~;,.?:(){}[]'\"\\".contains(c)|| // <- Update this! (but it can eb improved)
  WHITESPACES.contains(c)
```

2. Variable Length Lexemes

Variable length lexemes are handled by rules instead of parser combinators and dictionaries. To add a new type of literal or lexeme pattern, ddefine a new *LTokenRule* in *TokensUtils.scala*. 

Example: Adding a hexadecimal float literal

```scala
val hexFloatRule = LTokenRule(
  isPartial = s => s.matches("^0x[0-9A-Fa-f]*\\.?[0-9A-Fa-f]*p?[+-]?[0-9A-Fa-f]*$"),
  isResolved = s => s.matches("^0x[0-9A-Fa-f]+\\.[0-9A-Fa-f]+p[+-]?[0-9A-Fa-f]+$"),
  mkError = InvalidNumber.apply
)
```

Then register it:

```scala 
val LTokenRules: List[LTokenRule] = List(
  intRule,
  floatRule,
  stringRule,
  hexFloatRule     // new rule
)
```

# Testing the Project

To run the test cases, all that needs to be done is run the standard SBT command. This will run the test case for the following suites:

- Lexer (As we modified it from using the parser combinator to a recursive DFA)
- Parser
- Interpreter
- Semantic analysis
- IR
- Backend

```Bash
sbt test
```

# Running the Project

Step 1. Compile files
```
sbt compile
```

Step 2. Assemble cli
```
sbt cli/assembly
```

Step 3. Run the compiler on a test file. Example simp program source code can be found in the example folder.
```
// USAGE1: interpreter 
java -jar cli/target/scala-3.7.3/ScalaCompiler.jar -i source.simp <input>
// e.g. java -jar cli/target/scala-3.7.3/ScalaCompiler.jar -i example/fib.simp 2

// USAGE2: compiler
java -jar cli/target/scala-3.7.3/ScalaCompiler.jar -c example.simp
// e.g. java -jar cli/target/scala-3.7.3/ScalaCompiler.jar -c example/fib.simp
```

Running a GeneratedClass.class file. After compiling using step 3's command on a simp program source code, it produces a .class file which can be ran with:
```
java GeneratedClass <input>
```