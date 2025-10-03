// For more information on writing tests, see
// https://scalameta.org/munit/docs/getting-started.html
import parsec.Parsec.*
import lexer.*

class LexerSuite extends munit.FunSuite {

  private def runLex(input: String): List[LToken] = {
    val env = Lexer.LEnv(input.toList, 1, 0)
    val result = run(Lexer.lex())(env)

    result match {
      case Progress.Empty(Result.Ok((tokens, _)))    => tokens
      case Progress.Consumed(Result.Ok((tokens, _))) => tokens
      case Progress.Empty(Result.Failed(msg))    => throw new RuntimeException(s"Lexer failed:\n$msg")
      case Progress.Consumed(Result.Failed(msg)) => throw new RuntimeException(s"Lexer failed:\n$msg")
    }
  }

  test("lexes a single keyword (for)") {
    val tokens = runLex("for")
    assertEquals(tokens, List(LToken.ForKW(SrcLoc(1, 3))))
  }

  test("lexes a single keyword (while)") {
    val tokens = runLex("while")
    assertEquals(tokens, List(LToken.WhileKW(SrcLoc(1, 5))))
  }

  test("lexes an identifier with keyword in name") {
    val tokens = runLex("while_happy")
    assertEquals(tokens, List(LToken.IdTok(SrcLoc(1, 11), "while_happy")))
  }

  test("lexes a keyword multiple whitespaces and an identifier") {
    val tokens = runLex("for    i")
    assertEquals(tokens, List(
      LToken.ForKW(SrcLoc(1, 3)),
      LToken.WhiteSpace(SrcLoc(1,4),' '),
      LToken.WhiteSpace(SrcLoc(1,5),' '),
      LToken.WhiteSpace(SrcLoc(1,6),' '),
      LToken.WhiteSpace(SrcLoc(1,7),' '),
      LToken.IdTok(SrcLoc(1,8), "i")
    ))
  }

  test("lexes an identifier (foobar)") {
    val tokens = runLex("foobar")
    assertEquals(tokens, List(LToken.IdTok(SrcLoc(1, 6), "foobar")))
  }

  test("lexes an identifier (Ten_thousand)") {
    val tokens = runLex("Ten_thousand")
    assertEquals(tokens, List(LToken.IdTok(SrcLoc(1, 12), "Ten_thousand")))
  }

  test("lexes an integer literal") {
    val tokens = runLex("123")
    assertEquals(tokens, List(LToken.IntTok(SrcLoc(1, 3), 123)))
  }

  test("lexes an integer exp (123+321)") {
    val tokens = runLex("123+321")
    assertEquals(tokens, List(
      LToken.IntTok(SrcLoc(1, 3), 123),
      LToken.PlusSign(SrcLoc(1,4)),
      LToken.IntTok(SrcLoc(1, 7), 321)
    ))
  }

  test("lexes an integer float exp (123+3.5463245342 * (5-2))") {
    val tokens = runLex("123+3.5463245342 * (5-2)")
    assertEquals(tokens, List(
      LToken.IntTok(SrcLoc(1, 3),123),
      LToken.PlusSign(SrcLoc(1,4)),
      LToken.FloatTok(SrcLoc(1, 16),3.5463245342),
      LToken.WhiteSpace(SrcLoc(1,17),' '),
      LToken.AsterixSign(SrcLoc(1,18)),
      LToken.WhiteSpace(SrcLoc(1,19),' '),
      LToken.LParen(SrcLoc(1,20)),
      LToken.IntTok(SrcLoc(1,21),5),
      LToken.MinusSign(SrcLoc(1,22)),
      LToken.IntTok(SrcLoc(1,23),2),
      LToken.RParen(SrcLoc(1,24))
    ))
  }

  test("lexes a float literal (3.146782345)") {
    val tokens = runLex("3.146782345")
    assertEquals(tokens, List(LToken.FloatTok(SrcLoc(1, 11), 3.146782345)))
  }

  test("lexes an operator") {
    val tokens = runLex("==")
    assertEquals(tokens, List(LToken.DEqSign(SrcLoc(1, 2))))
  }

  test("lexes delimiters and punctuation") {
    val tokens = runLex("(x,y)")
    assertEquals(tokens, List(
      LToken.LParen(SrcLoc(1,1)),
      LToken.IdTok(SrcLoc(1,2), "x"),
      LToken.CommaTok(SrcLoc(1,3)),
      LToken.IdTok(SrcLoc(1,4), "y"),
      LToken.RParen(SrcLoc(1,5))
    ))
  }

  test("lexes boolean literals") {
    val tokens = runLex("true false")
    assertEquals(tokens, List(
      LToken.TrueKW(SrcLoc(1,4)),
      LToken.WhiteSpace(SrcLoc(1,5), ' '),
      LToken.FalseKW(SrcLoc(1,10))
    ))
  }

  test("lexes across newlines with line/col tracking") {
    val tokens = runLex("foo\nbar")
    assertEquals(tokens, List(
      LToken.IdTok(SrcLoc(1,3), "foo"),
      LToken.WhiteSpace(SrcLoc(2,1), '\n'),
      LToken.IdTok(SrcLoc(2,4), "bar")
    ))
  }

  test("Spamming white space") {
    val tokens = runLex("     ")
    assertEquals(tokens, List(
      LToken.WhiteSpace(SrcLoc(1,1), ' '),
      LToken.WhiteSpace(SrcLoc(1,2), ' '),
      LToken.WhiteSpace(SrcLoc(1,3), ' '),
      LToken.WhiteSpace(SrcLoc(1,4), ' '),
      LToken.WhiteSpace(SrcLoc(1,5), ' '),
    ))
  }

  test("string literal: plain text") {
    val tokens = runLex("\"hello\"")
    assertEquals(tokens.head, LToken.StringTok(SrcLoc(1,7), "hello"))
  }

  test("string literal: empty string") {
    val tokens = runLex("\"\"")
    assertEquals(tokens.head, LToken.StringTok(SrcLoc(1,2), ""))
  }

  test("string literal: with spaces") {
    val tokens = runLex("\"hello world\"")
    assertEquals(tokens.head, LToken.StringTok(SrcLoc(1,13), "hello world"))
  }

  test("string literal: with punctuation") {
    val tokens = runLex("\"hello, world!\"")
    assertEquals(tokens.head, LToken.StringTok(SrcLoc(1,15), "hello, world!"))
  }

  test("lexer should fail on unexpected character") {
    val ex = intercept[RuntimeException] {
      runLex("foo_bar\n(123874)$") // $ not a valid char
    }
    println(ex.getMessage) 
  }

  test("lexer should fail on unexpected character") {
    val ex = intercept[RuntimeException] {
      runLex("$foo_bar") // $ not valid start for identifier
    }
    println(ex.getMessage) 
  }

  test("lexer should fail on unterminated string") {
    val ex = intercept[RuntimeException] {
      runLex("\"hello") // no closing quote
    }
    println(ex.getMessage) 
  }

  test("lexer should fail on invalid float literal") {
    val ex = intercept[RuntimeException] {
      runLex("123.") // not resolved float, EOF afterwards
    }
    println(ex.getMessage) 
  }

  test("lexer should fail on invalid binary literal") {
    val ex = intercept[RuntimeException] {
      runLex("0b2101") // invalid digit '2' in binary
    }
    println(ex.getMessage) 
  }

}

