package lexer

import LToken.*
import parsec.Parsec.*

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
                case Some(UnResolvedTok(srcloc, lexeme)) => lexError(InvalidNumber(lexeme, srcloc.ln, srcloc.cl)) // encountered an invalid exstension and our last token was unresolved TODO: find a way to change the error
                case Some(tok) => for { rest <- lex("", None) } yield tok :: rest // actually prepend the token
                case None      => lexError(UnexpectedChar(c, 1, 1)) // catastrophic failure // TODO find a way to get the LEnv into there for the current line
              }

            // EOF
            case Left(_) =>
              lastValid match {
                case Some(UnResolvedTok(srcloc, lexeme)) => lexError(InvalidNumber(lexeme, srcloc.ln, srcloc.cl)) // we reached EOF and our last token couldn't be resolved
                case Some(tok) => empty(List(tok)) 
                case None      => lexError(UnexpectedChar('\u0000', 1, 1)) // catastrophic failure
              }
          }
        } yield tokens
    
}