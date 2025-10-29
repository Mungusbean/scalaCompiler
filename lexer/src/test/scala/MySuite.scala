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

  // Integrated Course test cases
  test("lexes simple assignments: y = 1; x = x + 1;") {
    val src = "y = 1; x = x + 1;"
    val expected = List(
      LToken.IdTok(SrcLoc(1,1), "y"),
      LToken.WhiteSpace(SrcLoc(1,2), ' '),
      LToken.EqSign(SrcLoc(1,3)),
      LToken.WhiteSpace(SrcLoc(1,4), ' '),
      LToken.IntTok(SrcLoc(1,5), 1),
      LToken.SemiColon(SrcLoc(1,6)),
      LToken.WhiteSpace(SrcLoc(1,7), ' '),
      LToken.IdTok(SrcLoc(1,8), "x"),
      LToken.WhiteSpace(SrcLoc(1,9), ' '),
      LToken.EqSign(SrcLoc(1,10)),
      LToken.WhiteSpace(SrcLoc(1,11), ' '),
      LToken.IdTok(SrcLoc(1,12), "x"),
      LToken.WhiteSpace(SrcLoc(1,13), ' '),
      LToken.PlusSign(SrcLoc(1,14)),
      LToken.WhiteSpace(SrcLoc(1,15), ' '),
      LToken.IntTok(SrcLoc(1,16), 1),
      LToken.SemiColon(SrcLoc(1,17))
    )

    val tokens = runLex(src)
    assertEquals(tokens, expected)
  }

  test("lexes assignment with identifier input: y = input;") {
    val src = "y = input;"
    val expected = List(
      LToken.IdTok(SrcLoc(1,1), "y"),
      LToken.WhiteSpace(SrcLoc(1,2), ' '),
      LToken.EqSign(SrcLoc(1,3)),
      LToken.WhiteSpace(SrcLoc(1,4), ' '),
      LToken.IdTok(SrcLoc(1,9), "input"),
      LToken.SemiColon(SrcLoc(1,10))
    )

    val tokens = runLex(src)
    assertEquals(tokens, expected)
  }

  test("lexes multiline program with while loop and return (normal int rep)") {
  val src = """
x = input;
s = 0;
c = 0;
while c < x {
    s = c + s;
    c = c + 1;
}
return s;
        """.filter(_ != '\r')

    val expected = List(
      LToken.WhiteSpace(SrcLoc(2,1), '\n'),
      LToken.IdTok(SrcLoc(2,2), "x"),
      LToken.WhiteSpace(SrcLoc(2,3), ' '),
      LToken.EqSign(SrcLoc(2,4)),
      LToken.WhiteSpace(SrcLoc(2,5), ' '),
      LToken.IdTok(SrcLoc(2,10), "input"),
      LToken.SemiColon(SrcLoc(2,11)),

      LToken.WhiteSpace(SrcLoc(3,1), '\n'),
      LToken.IdTok(SrcLoc(3,2), "s"),
      LToken.WhiteSpace(SrcLoc(3,3), ' '),
      LToken.EqSign(SrcLoc(3,4)),
      LToken.WhiteSpace(SrcLoc(3,5), ' '),
      LToken.IntTok(SrcLoc(3,6), 0),
      LToken.SemiColon(SrcLoc(3,7)),

      LToken.WhiteSpace(SrcLoc(4,1), '\n'),
      LToken.IdTok(SrcLoc(4,2), "c"),
      LToken.WhiteSpace(SrcLoc(4,3), ' '),
      LToken.EqSign(SrcLoc(4,4)),
      LToken.WhiteSpace(SrcLoc(4,5), ' '),
      LToken.IntTok(SrcLoc(4,6), 0),
      LToken.SemiColon(SrcLoc(4,7)),

      LToken.WhiteSpace(SrcLoc(5,1), '\n'),
      LToken.WhileKW(SrcLoc(5,6)),
      LToken.WhiteSpace(SrcLoc(5,7), ' '),
      LToken.IdTok(SrcLoc(5,8), "c"),
      LToken.WhiteSpace(SrcLoc(5,9), ' '),
      LToken.LThanSign(SrcLoc(5,10)),
      LToken.WhiteSpace(SrcLoc(5,11), ' '),
      LToken.IdTok(SrcLoc(5,12), "x"),
      LToken.WhiteSpace(SrcLoc(5,13), ' '),
      LToken.LBrace(SrcLoc(5,14)),

      LToken.WhiteSpace(SrcLoc(6,1), '\n'),
      LToken.WhiteSpace(SrcLoc(6,2), ' '),
      LToken.WhiteSpace(SrcLoc(6,3), ' '),
      LToken.WhiteSpace(SrcLoc(6,4), ' '),
      LToken.WhiteSpace(SrcLoc(6,5), ' '),
      LToken.IdTok(SrcLoc(6,6), "s"),
      LToken.WhiteSpace(SrcLoc(6,7), ' '),
      LToken.EqSign(SrcLoc(6,8)),
      LToken.WhiteSpace(SrcLoc(6,9), ' '),
      LToken.IdTok(SrcLoc(6,10), "c"),
      LToken.WhiteSpace(SrcLoc(6,11), ' '),
      LToken.PlusSign(SrcLoc(6,12)),
      LToken.WhiteSpace(SrcLoc(6,13), ' '),
      LToken.IdTok(SrcLoc(6,14), "s"),
      LToken.SemiColon(SrcLoc(6,15)),

      LToken.WhiteSpace(SrcLoc(7,1), '\n'),
      LToken.WhiteSpace(SrcLoc(7,2), ' '),
      LToken.WhiteSpace(SrcLoc(7,3), ' '),
      LToken.WhiteSpace(SrcLoc(7,4), ' '),
      LToken.WhiteSpace(SrcLoc(7,5), ' '),
      LToken.IdTok(SrcLoc(7,6), "c"),
      LToken.WhiteSpace(SrcLoc(7,7), ' '),
      LToken.EqSign(SrcLoc(7,8)),
      LToken.WhiteSpace(SrcLoc(7,9), ' '),
      LToken.IdTok(SrcLoc(7,10), "c"),
      LToken.WhiteSpace(SrcLoc(7,11), ' '),
      LToken.PlusSign(SrcLoc(7,12)),
      LToken.WhiteSpace(SrcLoc(7,13), ' '),
      LToken.IntTok(SrcLoc(7,14), 1),
      LToken.SemiColon(SrcLoc(7,15)),

      LToken.WhiteSpace(SrcLoc(8,1), '\n'),
      LToken.RBrace(SrcLoc(8,2)),

      LToken.WhiteSpace(SrcLoc(9,1), '\n'),
      LToken.RetKW(SrcLoc(9,7)),
      LToken.WhiteSpace(SrcLoc(9,8), ' '),
      LToken.IdTok(SrcLoc(9,9), "s"),
      LToken.SemiColon(SrcLoc(9,10)),

      LToken.WhiteSpace(SrcLoc(10,1), '\n'),
      LToken.WhiteSpace(SrcLoc(10,2), ' '),
      LToken.WhiteSpace(SrcLoc(10,3), ' '),
      LToken.WhiteSpace(SrcLoc(10,4), ' '),
      LToken.WhiteSpace(SrcLoc(10,5), ' '),
      LToken.WhiteSpace(SrcLoc(10,6), ' '),
      LToken.WhiteSpace(SrcLoc(10,7), ' '),
      LToken.WhiteSpace(SrcLoc(10,8), ' '),
      LToken.WhiteSpace(SrcLoc(10,9), ' ')
    )

    val tokens = runLex(src)

    // // Debug for out lexer's output
    // println("==== OBTAINED TOKENS ====")
    // tokens.zipWithIndex.foreach { case (tok, i) =>
    //   println(f"$i%3d: $tok")
    // }
    // println("=========================\n")

    assertEquals(tokens, expected)
  }

  test("lexes multiline program with while loop and return (binary int rep)") {
  val src = """
x = input;
s = 0;
c = 0;
while c < x {
    s = c + s;
    c = c + 0b0001;
}
return s;
        """.filter(_ != '\r')

    val expected = List(
      LToken.WhiteSpace(SrcLoc(2,1), '\n'),
      LToken.IdTok(SrcLoc(2,2), "x"),
      LToken.WhiteSpace(SrcLoc(2,3), ' '),
      LToken.EqSign(SrcLoc(2,4)),
      LToken.WhiteSpace(SrcLoc(2,5), ' '),
      LToken.IdTok(SrcLoc(2,10), "input"),
      LToken.SemiColon(SrcLoc(2,11)),

      LToken.WhiteSpace(SrcLoc(3,1), '\n'),
      LToken.IdTok(SrcLoc(3,2), "s"),
      LToken.WhiteSpace(SrcLoc(3,3), ' '),
      LToken.EqSign(SrcLoc(3,4)),
      LToken.WhiteSpace(SrcLoc(3,5), ' '),
      LToken.IntTok(SrcLoc(3,6), 0),
      LToken.SemiColon(SrcLoc(3,7)),

      LToken.WhiteSpace(SrcLoc(4,1), '\n'),
      LToken.IdTok(SrcLoc(4,2), "c"),
      LToken.WhiteSpace(SrcLoc(4,3), ' '),
      LToken.EqSign(SrcLoc(4,4)),
      LToken.WhiteSpace(SrcLoc(4,5), ' '),
      LToken.IntTok(SrcLoc(4,6), 0),
      LToken.SemiColon(SrcLoc(4,7)),

      LToken.WhiteSpace(SrcLoc(5,1), '\n'),
      LToken.WhileKW(SrcLoc(5,6)),
      LToken.WhiteSpace(SrcLoc(5,7), ' '),
      LToken.IdTok(SrcLoc(5,8), "c"),
      LToken.WhiteSpace(SrcLoc(5,9), ' '),
      LToken.LThanSign(SrcLoc(5,10)),
      LToken.WhiteSpace(SrcLoc(5,11), ' '),
      LToken.IdTok(SrcLoc(5,12), "x"),
      LToken.WhiteSpace(SrcLoc(5,13), ' '),
      LToken.LBrace(SrcLoc(5,14)),

      LToken.WhiteSpace(SrcLoc(6,1), '\n'),
      LToken.WhiteSpace(SrcLoc(6,2), ' '),
      LToken.WhiteSpace(SrcLoc(6,3), ' '),
      LToken.WhiteSpace(SrcLoc(6,4), ' '),
      LToken.WhiteSpace(SrcLoc(6,5), ' '),
      LToken.IdTok(SrcLoc(6,6), "s"),
      LToken.WhiteSpace(SrcLoc(6,7), ' '),
      LToken.EqSign(SrcLoc(6,8)),
      LToken.WhiteSpace(SrcLoc(6,9), ' '),
      LToken.IdTok(SrcLoc(6,10), "c"),
      LToken.WhiteSpace(SrcLoc(6,11), ' '),
      LToken.PlusSign(SrcLoc(6,12)),
      LToken.WhiteSpace(SrcLoc(6,13), ' '),
      LToken.IdTok(SrcLoc(6,14), "s"),
      LToken.SemiColon(SrcLoc(6,15)),

      LToken.WhiteSpace(SrcLoc(7,1), '\n'),
      LToken.WhiteSpace(SrcLoc(7,2), ' '),
      LToken.WhiteSpace(SrcLoc(7,3), ' '),
      LToken.WhiteSpace(SrcLoc(7,4), ' '),
      LToken.WhiteSpace(SrcLoc(7,5), ' '),
      LToken.IdTok(SrcLoc(7,6), "c"),
      LToken.WhiteSpace(SrcLoc(7,7), ' '),
      LToken.EqSign(SrcLoc(7,8)),
      LToken.WhiteSpace(SrcLoc(7,9), ' '),
      LToken.IdTok(SrcLoc(7,10), "c"),
      LToken.WhiteSpace(SrcLoc(7,11), ' '),
      LToken.PlusSign(SrcLoc(7,12)),
      LToken.WhiteSpace(SrcLoc(7,13), ' '),
      LToken.IntTok(SrcLoc(7,19), 1),
      LToken.SemiColon(SrcLoc(7,20)),

      LToken.WhiteSpace(SrcLoc(8,1), '\n'),
      LToken.RBrace(SrcLoc(8,2)),

      LToken.WhiteSpace(SrcLoc(9,1), '\n'),
      LToken.RetKW(SrcLoc(9,7)),
      LToken.WhiteSpace(SrcLoc(9,8), ' '),
      LToken.IdTok(SrcLoc(9,9), "s"),
      LToken.SemiColon(SrcLoc(9,10)),

      LToken.WhiteSpace(SrcLoc(10,1), '\n'),
      LToken.WhiteSpace(SrcLoc(10,2), ' '),
      LToken.WhiteSpace(SrcLoc(10,3), ' '),
      LToken.WhiteSpace(SrcLoc(10,4), ' '),
      LToken.WhiteSpace(SrcLoc(10,5), ' '),
      LToken.WhiteSpace(SrcLoc(10,6), ' '),
      LToken.WhiteSpace(SrcLoc(10,7), ' '),
      LToken.WhiteSpace(SrcLoc(10,8), ' '),
      LToken.WhiteSpace(SrcLoc(10,9), ' ')
    )

    val tokens = runLex(src)
    assertEquals(tokens, expected)
  }

}

