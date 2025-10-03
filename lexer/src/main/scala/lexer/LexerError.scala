package lexer

import LToken.*
import Lexer.LEnv
import parsec.Parsec.*
import parsec.Parsec.Result.* 
import parsec.Parsec.Progress.*

val RESET  = "\u001b[0m"
val RED    = "\u001b[31m"
val YELLOW = "\u001b[33m"
val CYAN   = "\u001b[36m"

sealed trait LexError(using pe: ParserEnv[LEnv, Char]) {
  def lexeme: String
  def lenv: LEnv
  def msg: String

  protected def line: Int = pe.getLine(lenv)
  protected def col: Int  = pe.getCol(lenv)
  protected def tokens: List[Char] = pe.getTokens(lenv)

  protected def highlight(startCol: Int, length: Int): String = {
    val lineStr = tokens.mkString
    val underline = " " * (startCol - 1) + "^" * (if length > 0 then length else 1)
    s"$lineStr\n$underline"
  }
}

case class UnexpectedChar(c: Char, lenv: LEnv)(using pe: ParserEnv[LEnv, Char]) extends LexError {
  def lexeme: String = c.toString
  def msg: String =
    s"${Console.RED}error:${Console.RESET} unexpected character ${Console.RED}'$c'${Console.RESET} " +
    s"at line ${Console.YELLOW}$line${Console.RESET}, col ${Console.YELLOW}${col+1}${Console.RESET}\n" +
    highlight(col+1, 1)
}

case class UnterminatedString(lexeme: String, lenv: LEnv)(using pe: ParserEnv[LEnv, Char]) extends LexError {
  def msg: String =
    s"${Console.RED}error:${Console.RESET} unterminated string literal " +
    s"at line ${Console.YELLOW}$line${Console.RESET}, col ${Console.YELLOW}$col${Console.RESET}\n" +
    s"${Console.RED}$lexeme${Console.RESET}" +
    highlight(col - lexeme.length + 1, lexeme.length)
}

case class InvalidNumber(lexeme: String, lenv: LEnv)(using pe: ParserEnv[LEnv, Char]) extends LexError {
  def msg: String =
    s"${Console.RED}error:${Console.RESET} invalid number ${Console.RED}'$lexeme'${Console.RESET} " +
    s"at line ${Console.YELLOW}$line${Console.RESET}, col ${Console.YELLOW}$col${Console.RESET}\n" +
    s"${Console.RED}$lexeme${Console.RESET}" +
    highlight(col - lexeme.length + 1, lexeme.length)
}


def lexError(lexeme: String, build: (String, LEnv) => LexError)(using pe: ParserEnv[LEnv, Char]): Parser[LEnv, List[LToken]] =
    Parser(env => 
      Empty(Failed(build(lexeme, env).msg))
    )

