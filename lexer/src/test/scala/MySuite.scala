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
      case Progress.Empty(Result.Failed(msg))        => sys.error(s"Lexer failed: $msg")
      case Progress.Consumed(Result.Failed(msg))     => sys.error(s"Lexer failed: $msg")
    }
  }

  test("lexes a single keyword") {
    val tokens = runLex("for")
    assertEquals(tokens, List(LToken.ForKW(SrcLoc(1, 3))))
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

  test("lexes an integer exp") {
    val tokens = runLex("123+321")
    assertEquals(tokens, List(
      LToken.IntTok(SrcLoc(1, 3), 123),
      LToken.PlusSign(SrcLoc(1,4)),
      LToken.IntTok(SrcLoc(1, 7), 321)
    ))
  }

  test("lexes a float literal") {
    val tokens = runLex("3.14")
    assertEquals(tokens, List(LToken.FloatTok(SrcLoc(1, 4), 3.14)))
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

  test("Test") {
  val tokens = runLex("%%%%%")
  assertEquals(tokens, List(
    LToken.WhiteSpace(SrcLoc(1,1), ' ')
  ))
  }

}

