package lexer

import scala.compiletime.ops.float
import scala.compiletime.ops.double
import lexer.Lexer.LEnv

case class SrcLoc(val ln: Int, val cl: Int)

type Id = String

enum LToken {

    // ========================= Operator Tokens =========================
    case EqSign(src:SrcLoc)
    case DEqSign(src:SrcLoc)
    case PlusSign(src:SrcLoc)
    case MinusSign(src:SrcLoc)
    case AsterixSign(src:SrcLoc)
    case FSlashSign(src:SrcLoc)
    case DfslashSign(src:SrcLoc)
    case LThanSign(src:SrcLoc)
    case LThanEqSign(src:SrcLoc)
    case GThanSign(src:SrcLoc)
    case GThanEqSign(src:SrcLoc)
    case TildaSign(src:SrcLoc)
    case CarrotSign(src:SrcLoc)
    case AmpSign(src:SrcLoc)
    // case LArrSign(src:SrcLoc)
    // case RArrSign(src:SrcLoc)
    case ExcMrkSign(src:SrcLoc)
    
    // ========================= Delimiter Tokens =========================
    case LBrace(src:SrcLoc)
    case RBrace(src:SrcLoc)
    case LParen(src:SrcLoc)
    case RParen(src:SrcLoc)
    case SemiColon(src:SrcLoc)

    // ========================= Keywords =========================

    // [Return]
    case RetKW(src:SrcLoc)

    // [Declarations]
    case DefKW(src:SrcLoc)

    // [Control flow]
    case IfKW(src:SrcLoc) 
    case ElseKW(src:SrcLoc) 

    // [Loops]
    case WhileKW(src:SrcLoc)
    case ForKW(src:SrcLoc)
    
    // [True/ False]
    case NopKW(src:SrcLoc) // no-op?
    case TrueKW(src:SrcLoc)
    case FalseKW(src:SrcLoc)

    // ========================= Variable length tokens =========================
    // These are tokens that have variable length or require special parsing rules

    // [Identifier]
    case IdTok(src:SrcLoc, value:Id)

    // [Punctuation]
    case SQuoteTok(src:SrcLoc) 
    // case DQuoteTok(src:SrcLoc)
    case BSlashTok(src:SrcLoc)
    case DotTok(src:SrcLoc)
    case CommaTok(src:SrcLoc)
    case ColonTok(src:SrcLoc)
    case LSqBrace(src:SrcLoc)
    case RSqBrace(src:SrcLoc)

    // [Literals]
    case IntTok(src:SrcLoc, v:Int)
    case FloatTok(src:SrcLoc, v:Double)
    case StringTok(src:SrcLoc, v:String)

    // [Unresolved] 
    /* 
    * This token is emitted when a semi complete token is encountered.
    * A semi complete token is a token that only partially matches a correct variable length token but must terminate correctly.
    * OR
    * Must fufill some predicate or rule but can only be resolved later.
    */
    case UnResolvedTok(src:SrcLoc, v:String, mkError: (String, LEnv) => LexError)

    // ========================= WhiteSpaces ===================================
    // [\n, \t, \s]
    case WhiteSpace(src:SrcLoc, c:Char)

}

final case class LTokenRule(
    isPartial: String => Boolean,
    isResolved: String => Boolean,
    mkError: (String, LEnv) => LexError
)