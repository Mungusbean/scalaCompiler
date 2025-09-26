package lexer

import LToken.*
import parsec.Parsec.*
import monad.Monad.MonadError

object Lexer {

    case class LEnv(tokens: List[Char], ln: Int, cl: Int)

    def eof(lenv:LEnv):Boolean = lenv match {
        case LEnv(Nil, _, _) => true
        case _ => false
    }

    given lenvParserEnv:ParserEnv[LEnv, Char] = new ParserEnv[LEnv, Char] {
        override def getTokens(env: LEnv): List[Char] = env match {
            case LEnv(toks, ln, cl) => toks
        }
        override def getCol(env: LEnv): Int = env match {
            case LEnv(toks, ln, cl) => cl
        }
        override def getLine(env: LEnv): Int = env match {
            case LEnv(toks, ln, cl) => ln
        }
        override def setTokens(ts: List[Char])(env: LEnv): LEnv = env match {
            case LEnv(toks, ln, cl) => LEnv(ts, ln, cl)
        }
        override def setLine(l: Int)(env: LEnv): LEnv = env match {
            case LEnv(toks, _, cl) => LEnv(toks, l, cl)
        }

        override def setCol(c: Int)(env: LEnv): LEnv = env match {
            case LEnv(toks, ln, _) => LEnv(toks, ln, c)
        }
    }

    // def lexOne(using pe:ParserEnv[LEnv, Char])(using acc: String):Parser[LEnv, LToken] = for {
    //     c <- sat((c:Char) => isValidLexemeChar(c))
    //     ln <- get(lenv => pe.getLine(lenv))
    //     cl <- get(lenv => pe.getCol(lenv))
    // } yield emitToken(ln, cl, c)

    def lexChar(using pe: ParserEnv[LEnv, Char]): Parser[LEnv, (Char, Int, Int)] =
      for {
        c  <- sat((c: Char) => isValidLexemeChar(c))
        ln <- get(lenv => pe.getLine(lenv))
        cl <- get(lenv => pe.getCol(lenv))
      } yield (c, ln, cl)

    def lex( acc: String = "", lastValid: Option[LToken] = None)(using pe: ParserEnv[LEnv, Char]): Parser[LEnv, List[LToken]] =
        for {
          maybeC <- lookAhead(optional(item))
          tokens <- maybeC match {
            case Right(c: Char) if isValidToken(c)(using acc) =>
              // valid: now actually consume
              for {
                (c1, ln, cl) <- lexChar // consume
                tok = emitToken(ln, cl, c1)(using acc)
                rest <- lex(acc + c1, Some(tok))
              } yield rest

            case Right(_) =>
              // invalid extension
              lastValid match {
                case Some(tok) => for { rest <- lex("", None) } yield tok :: rest
                case None      => empty(Nil)
              }

            case Left(_) =>
              // EOF
              lastValid match {
                case Some(tok) => empty(List(tok))
                case None      => empty(Nil)
              }
          }
        } yield tokens
    
}