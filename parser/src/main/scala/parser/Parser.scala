package parser

import org.scalactic.Bool
import parsec.Parsec.*
import ast.AST.*
// import lexer.Lexer.*
import lexer.*

object Parser {
    /**
     * S ::= X = E ; | return X ; | nop | if E { \overline{S} } else { \overline{S} } | while E { \overline{S} } 
     * E ::= E Op E | X | C | (E)
     * \overline{S} ::= S | S \overline{S}
     * Op ::= + | - | *  
     * C ::= 1 | 2 | ... | true | false 
     * X ::= a | b | c | d 
     * */


    import Stmt.*
    import Exp.*
    import Const.* 
    
    import LToken.*
    import Progress.*
    import Result.*

    case class PEnv(toks: List[LToken])

    /**
      * check whether the parsing is done based on the list of tokens left.
      *
      * @param env
      * @return boolean
      */
    def done(env:PEnv):Boolean = env match {
        case PEnv(Nil) => true
        case _ => false
    }

    /**
      * type class instance of ParserEnv[PEnv, LToken]
      */
    given penvParserEnv: ParserEnv[PEnv, LToken] = new ParserEnv[PEnv, LToken] {
        override def getTokens(env: PEnv): List[LToken] = env match {
            case PEnv(toks) => toks
        }
        override def getCol(env: PEnv): Int = env match {
            case PEnv(Nil) => -1
            case PEnv(tok :: toks) =>
                srcLoc(tok) match {
                    case SrcLoc(ln, col) => col
                }
        }
        override def getLine(env: PEnv): Int = env match {
            case PEnv(Nil) => -1
            case PEnv(tok :: toks) =>
                srcLoc(tok) match {
                    case SrcLoc(ln, col) => ln
                }
        }
        override def setTokens(ts: List[LToken])(env: PEnv): PEnv = env match {
            case PEnv(_) => PEnv(ts)
        }

    }
    /**
      * The top level parser
      */
    def parse:Parser[PEnv, List[Stmt]] = p_stmts

    /**
      * Parsing a sequence of statements, 
      * we skip the preceeding and the proceeding white spaces for each statement.
      * for individual statement parser, 
      * we only need to skip the whitespace in between.
      *
      * @return
      */
    def p_stmts:Parser[PEnv, List[Stmt]] = {
        def p_one:Parser[PEnv, Stmt] = for {
            _ <- p_spaces
            s <- p_stmt
            _ <- p_spaces
        } yield s
        many(p_one)
    } 

    /**
      * Parsing a statement
      *
      * @return
      */
    def p_stmt:Parser[PEnv, Stmt] = choice(p_assign)(
        choice(p_ret)(
            choice(p_nop)(
                choice(p_ifelse)(p_while)
            )
        )
    )

    /**
      * Parsing a Nop statement
      *
      * @return
      */
    def p_nop:Parser[PEnv, Stmt] = for {
        _ <- sat((tok:LToken) => tok match {
            case NopKW(src) => true 
            case _ => false 
        })
        _ <- p_spaces
        _ <- p_semicolon
    } yield Nop

    /**
      * Parsing an assignment statement
      *
      * @return
      */
    def p_assign:Parser[PEnv, Stmt] = for {
        x <- p_var
        _ <- p_spaces
        _ <- p_equal
        _ <- p_spaces
        e <- p_exp
        _ <- p_spaces
        _ <- p_semicolon
    } yield Assign(x, e)

    /**
      * Parsing a return statement
      *
      * @return
      */
    def p_ret:Parser[PEnv, Stmt] = for {
        _ <- p_returnKW
        _ <- p_spaces
        x <- p_var
        _ <- p_spaces
        _ <- p_semicolon
    } yield Ret(x)

    /**
      * Parsing an if-else statement
      *
      * @return
      */
    def p_ifelse:Parser[PEnv, Stmt] = for {
        _ <- p_ifKW
        _ <- p_spaces
        e <- p_exp
        _ <- p_spaces
        _ <- p_lbrace
        _ <- p_spaces
        s1 <- p_stmts
        _ <- p_rbrace
        _ <- p_spaces 
        _ <- p_elseKW
        _ <- p_spaces
        _ <- p_lbrace
        _ <- p_spaces
        s2 <- p_stmts
        _ <- p_rbrace
    } yield If(e, s1, s2)

    /**
      * Parsing a while statement
      *
      * @return
      */
    def p_while:Parser[PEnv, Stmt] = for {
        _ <- p_whileKW
        _ <- p_spaces
        e <- p_exp
        _ <- p_spaces
        _ <- p_lbrace
        s <- p_stmts
        _ <- p_rbrace
    } yield While(e, s)

    /** Lab 1 Task 1.1
      * parsing / skipping whitespaces
      *
      * @return
      */

    def p_space:Parser[PEnv, LToken] = sat(ltoken => ltoken match {
        case WhiteSpace(_, _) => true
        case _ => false
    })
    
    def p_spaces:Parser[PEnv, List[LToken]] = many(p_space) 

    /** Lab 1 Task 1.1 end */ 
    // DONE


    /** Lab 1 Task 1.2 
      * Parsing an expression
      * Note that 
      *   E ::= E Op E | X | C | (E) contains left recursion
      * @return
      */

    /** New non-left recursive grammar
      * 
      * First create precedence for the operations and also make it non-left recursive (my reduction will be right associative will see how to remedy this)
      *   Split op ::= + | - | * | ... into:
      *   op0 ::= == | != | < | > | <= | >= (lowest precedence)
      *   op1 ::= + | - (second lowest precedence)
      *   op2 ::= * | / | % (highest precedence)
      * 
      *   E ::= E'| E' op0 E'
      *   E'::= T | T op1 E'
      *   T ::= F | F op2 T
      *   F ::= C | X | (E)
      *   
      *   we have to implement these expressions
      *   case Plus(e1:Exp, e2:Exp) done
      *   case Minus(e1:Exp, e2:Exp) done
      *   case Mult(e1:Exp, e2:Exp) done
      *   case Div(e1:Exp, e2:Exp) done
      *   case DEqual(e1:Exp, e2:Exp) done
      *   case LThan(e1:Exp, e2:Exp) done
      *   case ConstExp(l:Const) done
      *   case VarExp(v:Var) done
      *   case ParenExp(e:Exp) done
      * @return
      */

    // def p_op0: Parser[PEnv, LToken] = choice(p_dequal)(
    //     choice(p_nequal)(
    //         choice(p_lthan)(
    //             choice(p_lequal)(
    //                 choice(p_gthan)(p_gequal)
    //             )
    //         )
    //     )
    // )
    // def p_op1: Parser[PEnv, LToken] = choice(p_plus)(p_minus)
    // def p_op2: Parser[PEnv, LToken] = choice(p_mult)(p_div)

    // def p_exp:Parser[PEnv, Exp] = for {
    //     _ <- p_spaces
    //     e <- choice(attempt(p_Ep_op0_Ep))(p_Ep)
    // } yield e

    // def p_Ep: Parser[PEnv, Exp] = for {
    //     _  <- p_spaces
    //     ep <- choice(attempt(p_T_op1_Ep))(p_T)
    // } yield ep

    // def p_T: Parser[PEnv, Exp] = for {
    //     _ <- p_spaces
    //     t <- choice(attempt(p_F_op2_T))(p_F)
    // } yield t

    // def p_F: Parser[PEnv, Exp] = for {
    //     f <- choice(p_const.map(ConstExp(_)))( // For some reason the parser does not accept +A so we just "cast" it: edit nvm im an idiot i was too tired and didnt see the difference
    //             choice(p_var.map(VarExp(_)))(p_parenExp.map(identity))
    //         )
    // } yield f

    // def p_Ep_op0_Ep: Parser[PEnv, Exp] = for {
    //     ep1 <- p_Ep
    //     _   <- p_spaces
    //     op0 <- p_op0
    //     _   <- p_spaces
    //     ep2 <- p_Ep
    // } yield op0 match {
    //     case DEqSign(src)     => DEqual(ep1, ep2)
    //     case NEqSign(src)     => NEqual(ep1, ep2)
    //     case LThanSign(src)   => LThan(ep1, ep2)
    //     case LThanEqSign(src) => LEqual(ep1, ep2)
    //     case GThanSign(src)   => GThan(ep1, ep2)
    //     case GThanEqSign(src) => GEqual(ep1, ep2)
    //     case _                => sys.error("Error: unexpected operator token, expected ==, !=, >, <, >= or <=")
    // }

    // def p_T_op1_Ep: Parser[PEnv, Exp] = for {
    //     t   <- p_T
    //     _   <- p_spaces
    //     op1 <- p_op1
    //     _   <- p_spaces
    //     e   <- p_exp
    // } yield op1 match {
    //     case PlusSign(src)  => Plus(t, e)
    //     case MinusSign(src) => Minus(t, e)
    //     case _              => sys.error("Error: unexpected operator token, expected + or -.")
    // }

    // def p_F_op2_T: Parser[PEnv, Exp] = for {
    //     f   <- p_F
    //     _   <- p_spaces
    //     op2 <- p_op2
    //     _   <- p_spaces
    //     t   <- p_T
    // } yield op2 match {
    //     case AsterixSign(src) => Mult(f, t)
    //     case FSlashSign(src)  => Div(f, t)
    //     case _                => sys.error("Error: unexpected operator token, expected * or /.") // place holder
    // }

    // def p_parenExp: Parser[PEnv, ParenExp] = for {
    //     _ <- p_lparen
    //     _ <- p_spaces
    //     e <- p_exp
    //     _ <- p_spaces
    //     _ <- p_rparen
    // } yield ParenExp(e)

    /**
      *  CFG UPDATED (trying to prevent right associative nature)
      *  The original derived CFG, was left factored to prevent left recurssion and ambiguity.
      *  A heirachy for the precendance of the operators was also achieved by splitting the operands into tiers: (op0, op1 and op2)
      *  Where operators parsed in op0 were of lowest precedance and op2 being operators of highest precedance
      *  - due to the right recursive nature of the CFG (and the fact that the operands were genrated via right recursion), modified CFG is now right assciative.
      *  We will rectify this by leftfolding operands of the same precedance level instead of using explicit recursion via production rules.
      * 
      *  NEW CFG (Left recursive hopefully)
      *  E  ::= E' (op0 E')* // where ( exp )* indicates klien star repatition (similar to parsing as many spaces as possible)
      *  E' ::= T (op1 T)*
      *  T  ::= F (op2 F)*
      *  F  ::= C | X | (E)  
      */
    
    def p_op0: Parser[PEnv, (Exp, Exp) => Exp] = 
        choice(p_dequal.map(_ => (l:Exp, r:Exp) => DEqual(l, r)))(
            choice(p_nequal.map(_ => (l:Exp, r:Exp) => NEqual(l, r)))(
                choice(p_lthan.map(_ => (l:Exp, r:Exp) => LThan(l, r)))(
                    choice(p_lequal.map(_ => (l:Exp, r:Exp) => LEqual(l, r)))(
                        choice(p_gthan.map(_ => (l:Exp, r:Exp) => GThan(l, r)))(
                            p_gequal.map(_ => (l:Exp, r:Exp) => GEqual(l, r))
                        )
                    )
                )
            )
        )
    
    def p_op1: Parser[PEnv, (Exp, Exp) => Exp] = 
        choice(p_plus.map(_ => (l:Exp, r:Exp) => Plus(l, r)))(
            p_minus.map(_ => (l:Exp, r:Exp) => Minus(l, r))
        )

    def p_op2: Parser[PEnv, (Exp, Exp) => Exp] = 
        choice(p_mult.map(_ => (l:Exp, r:Exp) => Mult(l, r)))(
            p_div.map(_ => (l:Exp, r:Exp) => Div(l, r))
        )

    def p_parenExp: Parser[PEnv, ParenExp] = for {
        _ <- p_lparen
        _ <- p_spaces
        e <- p_exp
        _ <- p_spaces
        _ <- p_rparen
    } yield ParenExp(e)

    def p_X(p_op:Parser[PEnv, (Exp, Exp) => Exp])(p_x:Parser[PEnv, Exp]):Parser[PEnv, Exp] = for {
        x          <- p_x
        op_X_star <- many(attempt(
            for {
                _   <- p_spaces
                op <- p_op 
                _   <- p_spaces
                f2  <- p_x
            } yield (op, f2) // this will now be a list of operations to conduct. (op: function that wraps expressions in op operators, exp: expression to operate on))
        ))
    } yield op_X_star.foldLeft(x){ case (acc, (op, rhs)) => op(acc, rhs)}

    def p_F: Parser[PEnv, Exp] = for {
        f <- choice(p_const.map(ConstExp(_)))( // For some reason the parser does not accept +A so we just "cast" it: edit nvm im an idiot i was too tired and didnt see the difference
                choice(p_var.map(VarExp(_)))(p_parenExp.map(identity))
            )
    } yield f

    // T ::= F (op2 F)*
    val p_T = p_X(p_op2)(p_F)

    // E' ::= T (op1 T)*
    val p_Ep = p_X(p_op1)(p_T)

    // E ::= E' (op0 E')*
    val p_exp = p_X(p_op0)(p_Ep)


    /** Lab 1 Task 1.2 end */
    
    /**
      * Parsing operator symbols
      *
      * @return
      */
    def p_plus:Parser[PEnv,LToken] = sat(ltoken => ltoken match {
        case PlusSign(_) => true 
        case _ => false
    })

    def p_minus:Parser[PEnv,LToken] = sat(ltoken => ltoken match {
        case MinusSign(_) => true 
        case _ => false
    })

    def p_mult:Parser[PEnv,LToken] = sat(ltoken => ltoken match {
        case AsterixSign(_) => true 
        case _ => false
    })

    def p_div:Parser[PEnv,LToken] = sat(ltoken => ltoken match {
        case FSlashSign(_) => true 
        case _ => false
    })

    def p_lthan:Parser[PEnv,LToken] = sat(ltoken => ltoken match {
        case LThanSign(_) => true 
        case _ => false
    })

    def p_lequal:Parser[PEnv,LToken] = sat(ltoken => ltoken match {
        case LThanEqSign(_) => true 
        case _ => false
    })

    def p_gequal:Parser[PEnv,LToken] = sat(ltoken => ltoken match {
        case LThanEqSign(_) => true 
        case _ => false
    })

    def p_gthan:Parser[PEnv,LToken] = sat(ltoken => ltoken match {
        case GThanSign(_) => true 
        case _ => false
    })

    def p_dequal:Parser[PEnv,LToken] = sat(ltoken => ltoken match {
        case DEqSign(_) => true 
        case _ => false
    })

    def p_nequal:Parser[PEnv,LToken] = sat(ltoken => ltoken match {
        case NEqSign(_) => true 
        case _ => false
    })

    def p_equal:Parser[PEnv,LToken] = sat(ltoken => ltoken match {
        case EqSign(_) => true 
        case _ => false
    })

    /**
      * Parsing a Variable
      *
      * @return
      */
    def p_var:Parser[PEnv, Var] = for {
        tok <- sat((ltoken:LToken) => ltoken match {
            case IdTok(src, v) => true
            case _ => false 
        })
        name <- someOrFail(tok)( t => t match {
            case IdTok(src, v) =>  Some(v)
            case _ => None
        })("error: expecting an identifier, but None is returned.") // this error should never occur.
    } yield Var(name)


    /**
      * Parsing a Constant
      *
      * @return
      */
    def p_const:Parser[PEnv, Const] = 
        choice(choice(p_true)(p_false))(
            choice(p_int)(
                choice(p_float)(p_string)
            )
        )

    def p_true:Parser[PEnv, Const] = for {
        tok <- sat((ltoken:LToken) => ltoken match {
            case TrueKW(src) => true
            case _ => false 
        })
    } yield BoolConst(true)

    def p_false:Parser[PEnv, Const] = for {
        tok <- sat((ltoken:LToken) => ltoken match {
            case FalseKW(src) => true
            case _ => false 
        })
    } yield BoolConst(false)

    def p_int:Parser[PEnv, Const] = for {
        tok <- sat((ltoken:LToken) => ltoken match {
            case IntTok(src, v) => true
            case _ => false
        })
        i <- someOrFail(tok)( t => t match {
            case IntTok(src, v) =>  Some(v)
            case _ => None
        })("error: expecting an integer, but None is returned.") // this error should never occur.
    } yield IntConst(i)

    def p_float:Parser[PEnv, Const] = for {
        tok <- sat((ltoken:LToken) => ltoken match {
            case FloatTok(src, v) => true
            case _ => false
        })
        f <- someOrFail(tok)( t => t match {
            case FloatTok(src, v) =>  Some(v)
            case _ => None
        })("error: expecting a float, but None is returned.") // this error should never occur.
    } yield FloatConst(f)

    def p_string:Parser[PEnv, Const] = for {
        tok <- sat((ltoken:LToken) => ltoken match {
            case StringTok(src, v) => true
            case _ => false
        })
        s <- someOrFail(tok)( t => t match {
            case StringTok(src, v) =>  Some(v)
            case _ => None
        })("error: expecting a string, but None is returned.") // this error should never occur.
    } yield StrConst(s)    

    /**
      * Parsing keywords
      *
      * @return
      */
    def p_returnKW:Parser[PEnv, LToken] = sat(ltoken => ltoken match {
        case RetKW(src) => true
        case _ => false
    })

    def p_ifKW:Parser[PEnv, LToken] = sat(ltoken => ltoken match {
        case IfKW(src) => true
        case _ => false
    })

    def p_elseKW:Parser[PEnv, LToken] = sat(ltoken => ltoken match {
        case ElseKW(src) => true
        case _ => false
    })

    def p_whileKW:Parser[PEnv, LToken] = sat(ltoken => ltoken match {
        case WhileKW(src) => true
        case _ => false
    })



    /**
      * Parsing symbols
      */
    def p_lbrace:Parser[PEnv, LToken] = sat(ltoken => ltoken match {
        case LBrace(src) => true
        case _ => false
    })

    def p_rbrace:Parser[PEnv, LToken] = sat(ltoken => ltoken match {
        case RBrace(src) => true
        case _ => false
    })

    def p_lparen:Parser[PEnv, LToken] = sat(ltoken => ltoken match {
        case LParen(src) => true
        case _ => false
    })

    def p_rparen:Parser[PEnv, LToken] = sat(ltoken => ltoken match {
        case RParen(src) => true
        case _ => false
    })

    def p_semicolon:Parser[PEnv, LToken] = sat(ltoken => ltoken match {
        case SemiColon(src) => true
        case _ => false
    })
}