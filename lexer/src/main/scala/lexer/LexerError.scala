package lexer

import LToken.*
import Lexer.LEnv
import parsec.Parsec.*
import parsec.Parsec.Result.* 
import parsec.Parsec.Progress.*
import monad.Monad.MonadError

sealed trait LexError {
  def msg: Error
  def ln: Int
  def cl: Int
}
case class UnexpectedChar(c: Char, ln: Int, cl: Int) extends LexError {
    def msg = s"Unexpected character '$c' at line $ln, col $cl"
}

case class UnterminatedString(ln: Int, cl: Int) extends LexError {
  def msg: String = s"Unterminated string literal at line $ln, col $cl"
}

case class InvalidNumber(lexeme: String, ln: Int, cl: Int) extends LexError {
  def msg: String = s"Invalid number '$lexeme' at line $ln, col $cl"
}

def lexError(err: LexError): Parser[LEnv, List[LToken]] =
    Parser(env => Empty(Failed(err.msg))
    )

