package lexer

import scala.compiletime.ops.float
import scala.compiletime.ops.double

case class SrcLoc(val ln: Int, val cl: Int)

type Id = String

enum LToken {

    // ================ Operator Tokens ================
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
    
    // ================ Delimiter Tokens ================
    case LBrace(src:SrcLoc)
    case RBrace(src:SrcLoc)
    case LParen(src:SrcLoc)
    case RParen(src:SrcLoc)
    case SemiColon(src:SrcLoc)

    // ================ Keywords ================
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

    // ================ Other tokens ================
    // [Identifier]
    case IdTok(src:SrcLoc, value:Id)

    // [Punctuation]
    case SQuoteTok(src:SrcLoc) 
    case DQuoteTok(src:SrcLoc)
    case BSlashTok(src:SrcLoc)
    case DotTok(src:SrcLoc)
    case CommaTok(src:SrcLoc)
    case ColonTok(src:SrcLoc)
    case TildaTok(src:SrcLoc)
    case CarrotTok(src:SrcLoc)
    case AmpTok(src:SrcLoc)
    case LArrTok(src:SrcLoc)
    case RArrTok(src:SrcLoc)
    case ExcMrkTok(src:SrcLoc)
    case LSqBrace(src:SrcLoc)
    case RSqBrace(src:SrcLoc)

    // [Values]
    case IntTok(src:SrcLoc, v:Int)
    case FloatTok(src:SrcLoc, v:Double)

    // ================ WhiteSpaces ================
    // [\n, \t, \s]
    case WhiteSpace(src:SrcLoc, c:Char)
    }

import LToken.*
def srcLoc(tok: LToken):SrcLoc = tok match {
    // ================ Operator Tokens ================
    case EqSign(src) => src
    case DEqSign(src) => src
    case PlusSign(src) => src
    case MinusSign(src) => src
    case AsterixSign(src) => src
    case FSlashSign(src) => src
    case DfslashSign(src) => src
    case LThanSign(src) => src
    case LThanEqSign(src) => src
    case GThanSign(src) => src
    case GThanEqSign(src) => src
    
    // ================ Delimiter Tokens ================
    case LBrace(src) => src
    case RBrace(src) => src
    case LParen(src) => src
    case RParen(src) => src
    case SemiColon(src) => src

    // ================ Keywords ================
    // [Return]
    case RetKW(src) => src

    // [Declarations]
    case DefKW(src) => src
    
    // [Control flow]
    case IfKW(src) => src
    case ElseKW(src) => src

    // [Loops]
    case WhileKW(src) => src
    case ForKW(src) => src
    
    // [True/ False]
    case NopKW(src) => src 
    case TrueKW(src) => src
    case FalseKW(src) => src

    // ================ Other tokens ================
    // [Identifier]
    case IdTok(src, value) => src

    // [PUNCTUATION]
    case SQuoteTok(src) => src
    case DQuoteTok(src) => src
    case BSlashTok(src) => src
    case DotTok(src) => src
    case CommaTok(src) => src
    case ColonTok(src) => src
    case TildaTok(src) => src
    case CarrotTok(src) => src
    case AmpTok(src) => src
    case LArrTok(src) => src
    case RArrTok(src) => src
    case ExcMrkTok(src) => src
    case LSqBrace(src) => src
    case RSqBrace(src) => src


    // [Values]
    case IntTok(src, value) => src
    case FloatTok(src, value) => src

    // ================ WhiteSpaces ================
    // [\n, \t, \r, \f, " "]
    case WhiteSpace(src:SrcLoc, c:Char) => src
}


// ================ Syntax Maps ==================
// val WHITESPACES = Set('\t', '\r', '\n', ' ', '\f')
val WHITESPACES = Map(
    '\t' -> (src => WhiteSpace(src, '\t')), 
    '\r' -> (src => WhiteSpace(src, '\r')), 
    ' '  -> (src => WhiteSpace(src, ' ')), 
    '\f' -> (src => WhiteSpace(src, '\f')), 
    '\n' -> (src => WhiteSpace(src, '\n')) 
)

val OPERATORS: Map[String, SrcLoc => LToken] = Map(
    "<"     -> (src => LThanSign(src)), 
    ">"     -> (src => GThanSign(src)), 
    "<="    -> (src => LThanEqSign(src)), 
    "=>"    -> (src => GThanEqSign(src)), 
    "="     -> (src => EqSign(src)),
    "=="    -> (src => DEqSign(src)),
    "+"     -> (src => PlusSign(src)),
    "-"     -> (src => MinusSign(src)),
    "*"     -> (src => AsterixSign(src)),
    "/"     -> (src => FSlashSign(src)),
    "//"    -> (src => DfslashSign(src))
)

val KEYWORDS: Map[String, SrcLoc => LToken] = Map(
  "return"  -> (src => RetKW(src)),
  "while"   -> (src => WhileKW(src)),
  "for"     -> (src => ForKW(src)),
  "if"      -> (src => IfKW(src)),
  "else"    -> (src => ElseKW(src)),
  "nop"     -> (src => NopKW(src)),
  "true"    -> (src => TrueKW(src)),
  "false"   -> (src => FalseKW(src)),
  "def"     -> (src => DefKW(src))
)

val DELIMITERS: Map[String, SrcLoc => LToken] = Map(
    "{"     -> (src => LBrace(src)),
    "}"     -> (src => RBrace(src)),
    "("     -> (src => LParen(src)),
    ")"     -> (src => RParen(src)),
    ";"     -> (src => SemiColon(src)),
)

val PUNCT: Map[String, SrcLoc => LToken] = Map(
    "\""    -> (src => DQuoteTok(src)),
    "'"     -> (src => SQuoteTok(src)),
    "\\"    -> (src => BSlashTok(src)),
    "."     -> (src => DotTok(src)),
    ","     -> (src => CommaTok(src)),
    ":"     -> (src => ColonTok(src)),
    "~"     -> (src => TildaTok(src)),
    "^"     -> (src => CarrotTok(src)),
    "&"     -> (src => AmpTok(src)),
    "<"     -> (src => LArrTok(src)),
    ">"     -> (src => RArrTok(src)),
    "!"     -> (src => ExcMrkTok(src)),
    "["     -> (src => LSqBrace(src)),
    "]"     -> (src => RSqBrace(src)),

)

// ============ Helpers returns if a boolean if valid lexemes ==================== 
val isIdentifier: String => Boolean = (s: String) => 
    "^[a-zA-Z_][a-zA-Z0-9_]*$".r.matches(s)

val isIntLiteral: String => Boolean = s =>
  s.matches("""^[0-9]+$""") ||              // decimal
  s.matches("""^0[xX][0-9a-fA-F]+$""") ||   // hex
  s.matches("""^0[bB][01]+$""")             // binary

val isFloatLiteral: String => Boolean = s =>
  s.matches("""^[0-9]*\.[0-9]+([eE][+-]?[0-9]+)?$""") || // with decimal point
  s.matches("""^[0-9]+[eE][+-]?[0-9]+$""")               // scientific w/o decimal


val isValidLexemeChar: Char => Boolean = (c: Char) =>
  c.isLetterOrDigit || // allow any letter or digit
  c == '_' || // allow underscore
  "+-*/%=<>!&|^~;,.?:(){}[]'\"\\".contains(c)||
  WHITESPACES.contains(c)



def isValidToken(c: Char)(using acc: String): Boolean = {
    if (!isValidLexemeChar(c)) return false

    val accStr = acc + c

    if (WHITESPACES.contains(c) && (acc.length() == 0)) return true
    if (OPERATORS.contains(accStr)) return true
    if (DELIMITERS.contains(accStr)) return true
    if (PUNCT.contains(accStr)) return true

    // Identifiers & literals
    if (isIdentifier(accStr)) return true
    if (isIntLiteral(accStr)) return true
    if (isFloatLiteral(accStr)) return true

    false
}

def emitToken(ln: Int, cl: Int, c: Char)(using acc: String): LToken = {
  val srcloc   = SrcLoc(ln, cl)
  val curr_acc = acc + c

  // Dictionaries first
  if (OPERATORS.contains(curr_acc))  return OPERATORS(curr_acc)(srcloc)
  if (DELIMITERS.contains(curr_acc)) return DELIMITERS(curr_acc)(srcloc)
  if (PUNCT.contains(curr_acc))      return PUNCT(curr_acc)(srcloc)
  if (KEYWORDS.contains(curr_acc))   return KEYWORDS(curr_acc)(srcloc)

  // Identifiers
  if (isIdentifier(curr_acc)) return IdTok(srcloc, curr_acc)

  // Literals
  if (isIntLiteral(curr_acc))   return IntTok(srcloc, curr_acc.toInt)
  if (isFloatLiteral(curr_acc)) return FloatTok(srcloc, curr_acc.toDouble)

  // Whitespace
  if (curr_acc.length == 1 && WHITESPACES.contains(curr_acc.head))
    // return WhiteSpace(srcloc, curr_acc.head)
    return WHITESPACES(curr_acc.head)(srcloc)

  // Final fallback
  sys.error(s"emitToken: unrecognized token '$curr_acc' at $ln:$cl")
}