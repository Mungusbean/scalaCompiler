package lexer

import scala.compiletime.ops.float
import scala.compiletime.ops.double

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
    case LArrSign(src:SrcLoc)
    case RArrSign(src:SrcLoc)
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
    case UnResolvedTok(src:SrcLoc, v:String)

    // ========================= WhiteSpaces ===================================
    // [\n, \t, \s]
    case WhiteSpace(src:SrcLoc, c:Char)

}

final case class LTokenRule(
    isPartial: String => Boolean,
    isResolved: String => Boolean
)

import LToken.*
def srcLoc(tok: LToken):SrcLoc = tok match {

    // ========================= Operator Tokens =========================
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
    case TildaSign(src) => src
    case CarrotSign(src) => src
    case AmpSign(src) => src
    case LArrSign(src) => src
    case RArrSign(src) => src
    case ExcMrkSign(src) => src
    
    // ========================= Delimiter Tokens =========================
    case LBrace(src) => src
    case RBrace(src) => src
    case LParen(src) => src
    case RParen(src) => src
    case SemiColon(src) => src

    // ========================= Keywords =========================
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

    // ========================= Punctuation =========================

    // [PUNCTUATION]
    case SQuoteTok(src) => src
    // case DQuoteTok(src) => src
    case BSlashTok(src) => src
    case DotTok(src) => src
    case CommaTok(src) => src
    case ColonTok(src) => src
    case LSqBrace(src) => src
    case RSqBrace(src) => src

    // ========================= Variable length tokens =========================
    // These are tokens that have variable length or require special parsing rules

    // [Identifier]
    case IdTok(src, value) => src

    // [Literals]
    case IntTok(src, value) => src
    case FloatTok(src, value) => src
    case StringTok(src, value) => src

    // [Unresolved] 
    /* 
    * This token is emitted when a semi complete token is encountered.
    * A semi complete token is a token that only partially matches a correct variable length token but must terminate correctly.
    * OR
    * Must fufill some predicate or rule but can only be resolved later.
    */
    case UnResolvedTok(src, value) => src

    // ========================= WhiteSpaces ===================================
    // [\n, \t, \r, \f, " "]
    case WhiteSpace(src, c) => src
}


// ================ Syntax Maps ==================
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
    "//"    -> (src => DfslashSign(src)),
    "~"     -> (src => TildaSign(src)),
    "^"     -> (src => CarrotSign(src)),
    "&"     -> (src => AmpSign(src)),
    "<"     -> (src => LArrSign(src)),
    ">"     -> (src => RArrSign(src)),
    "!"     -> (src => ExcMrkSign(src))
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
    // "\""    -> (src => DQuoteTok(src)),
    "'"     -> (src => SQuoteTok(src)),
    "\\"    -> (src => BSlashTok(src)),
    "."     -> (src => DotTok(src)),
    ","     -> (src => CommaTok(src)),
    ":"     -> (src => ColonTok(src)),
    "["     -> (src => LSqBrace(src)),
    "]"     -> (src => RSqBrace(src)),

)


// ============ Rules for special tokens ====================

val isIdentifier: String => Boolean = (s: String) => 
    "^[a-zA-Z_][a-zA-Z0-9_]*$".r.matches(s)

val intRule = LTokenRule(
  isPartial = s =>
    s.matches("""^[0-9]+$""") ||              // plain decimal 
    s.matches("""^0x[0-9a-fA-F]*$""") ||      // hex, may still extend with hex digits
    s.matches("""^0b[01]*$"""),               // binary, may still extend with 0/1
  isResolved = s =>
    s.matches("""^[0-9]+$""") ||              // decimal
    s.matches("""^0x[0-9a-fA-F]+$""") ||      // hex (must have at least one digit)
    s.matches("""^0b[01]+$""")                // binary (must have at least one digit)
)

val floatRule = LTokenRule(
  isPartial = s =>
    s.matches("""^[0-9]*\.$""") ||                       // just after decimal point (needs digits)
    s.matches("""^[0-9]*\.[0-9]+$""") ||                 // digits after dot, can still extend
    s.matches("""^[0-9]*\.[0-9]+E$""") ||                // waiting for exponent sign/digits
    s.matches("""^[0-9]*\.[0-9]+E[+-]?$""") ||           // after sign, waiting for digits
    s.matches("""^[0-9]+E$""") ||                        // integer with E, waiting for exponent
    s.matches("""^[0-9]+E[+-]?$"""),                     // integer with E+ or E-, waiting for digits
  isResolved = s =>
    s.matches("""^[0-9]*\.[0-9]+(E[+-]?[0-9]+)?$""") ||  // decimal float with optional exponent
    s.matches("""^[0-9]+E[+-]?[0-9]+$""")                // scientific notation without decimal
)

val stringRule = LTokenRule(
  isPartial = s =>
    s.matches("""^"[^"]*"?$"""),       // opened quote, not yet closed
  isResolved = s =>
    s.matches("""^"[^"]*"$""")       // properly closed quote
)

val LTokenRules: List[LTokenRule] = List(
    intRule,
    floatRule,
    stringRule
)


val isValidLexemeChar: Char => Boolean = (c: Char) =>
  c.isLetterOrDigit || // allow any letter or digit
  c == '_' || // allow underscore
  "+-*/%=<>!&|^~;,.?:(){}[]'\"\\".contains(c)||
  WHITESPACES.contains(c)



def isValidToken(c: Char)(using acc: String): Boolean = {
    if (!isValidLexemeChar(c)) return false

    val accStr = acc + c

    // Fixed length tokens
    if (WHITESPACES.contains(c) && (acc.length() == 0)) return true
    if (OPERATORS.contains(accStr)) return true
    if (DELIMITERS.contains(accStr)) return true
    if (PUNCT.contains(accStr)) return true

    // Variable length tokens: Identifiers & literals
    if (isIdentifier(accStr)) return true
    if (LTokenRules.exists(_.isPartial(accStr))) return true

    false
}

def emitToken(ln: Int, cl: Int, c: Char)(using acc: String): LToken = {
    val srcloc   = SrcLoc(ln, cl)
    val curr_acc = acc + c
    //println(s"[DEBUG] Checking string acc='$curr_acc' | partial=${stringRule.isPartial(curr_acc)} | resolved=${stringRule.isResolved(curr_acc)}")
    // Dictionaries first
    if (OPERATORS.contains(curr_acc))  return OPERATORS(curr_acc)(srcloc)
    if (DELIMITERS.contains(curr_acc)) return DELIMITERS(curr_acc)(srcloc)
    if (PUNCT.contains(curr_acc))      return PUNCT(curr_acc)(srcloc)
    if (KEYWORDS.contains(curr_acc))   return KEYWORDS(curr_acc)(srcloc)

    // Identifiers
    if (isIdentifier(curr_acc)) return IdTok(srcloc, curr_acc)

    // Literals
    if (intRule.isResolved(curr_acc))    return IntTok(srcloc, curr_acc.toInt)
    if (floatRule.isResolved(curr_acc))  return FloatTok(srcloc, curr_acc.toDouble)
    if (stringRule.isResolved(curr_acc)) return StringTok(srcloc, curr_acc.substring(1, curr_acc.length - 1))

    // Whitespace
    if (curr_acc.length == 1 && WHITESPACES.contains(curr_acc.head)) return WHITESPACES(curr_acc.head)(srcloc)

    // Valid UnResolved states
    if (LTokenRules.exists(_.isPartial(curr_acc))) 
        return UnResolvedTok(srcloc, curr_acc)

    // Final fallback
    sys.error(s"emitToken: unrecognized token '$curr_acc' at $ln:$cl")
}