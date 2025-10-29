package ir


import monad.Functor.*
import monad.Applicative.*
import monad.Monad.*
import monad.StateT.*
import ast.AST.*
import ir.PseudoAssembly.*
import ir.Util.*


object MMUpDown {
    import Stmt.*
    import Var.*
    import Exp.*
    import Const.*
    import AVar.*
    import Instr.*
    import Opr.*
    

    type UpE = Opr
    type DownE = List[Instr]


    def genExp(e:Exp)(using me:StateCogenMonad[StateInfo]):CogenState[(Opr, List[(Label,Instr)])] = e match {
        // GE(c) |- (c, [])      (Const)
        case ConstExp(IntConst(v)) => me.pure((IntLit(v), Nil))
        // true into 1, false into 0
        case ConstExp(BoolConst(b)) if b => me.pure((IntLit(1), Nil))
        case ConstExp(BoolConst(b))      => me.pure((IntLit(0), Nil))
        // GE(X) |- (X, [])      (Var)
        case VarExp(v) => {
            val av = var2AVar(v) 
            me.pure((Temp(av), Nil))
        }
        /*
         GE(e) |- (up_e, down_e)
        ------------------------- (Paren)
         GE((e)) |- (up_e, down_e)
        */ 
        case ParenExp(e) => genExp(e)
        // Lab 1 Task 2.1 
        /*
         GE(e1) |- (up_e1, down_e1)
         GE(e2) |- (up_e2, down_e2)
         X is a fresh var
         L is a fresh label
        --------------------------------------------------------------------- (Op)
         GE(e1 op e2) |- (X, down_e1 ++ down_e2 ++ [L:X <- up_e1 op up_e2])
         */
        /* 
        enum Exp{
            case Plus(e1:Exp, e2:Exp)       done
            case Minus(e1:Exp, e2:Exp)      done
            case Mult(e1:Exp, e2:Exp)       done
            case Div(e1:Exp, e2:Exp)        not done
            case DEqual(e1:Exp, e2:Exp)     done
            case NEqual(e1:Exp, e2:Exp)     not done
            case LThan(e1:Exp, e2:Exp)      done
            case LEqual(e1:Exp, e2:Exp)     not done
            case GThan(e1:Exp, e2:Exp)      not done
            case GEqual(e1:Exp, e2:Exp)     not done
            case ConstExp(l:Const)          done
            case VarExp(v:Var)              done
            case ParenExp(e:Exp)            done
        }
        */
        // All the expressions (Maybe should have made a BinOP AST class?)
        case Plus(e1, e2)   => genBinOpExp("IPlus", e1, e2)
        case Minus(e1, e2)  => genBinOpExp("IMinus", e1, e2)
        case Mult(e1, e2)   => genBinOpExp("IMult", e1, e2)
        case DEqual(e1, e2) => genBinOpExp("IDEqual", e1, e2)
        case LThan(e1, e2)  => genBinOpExp("ILThan", e1, e2)
        case _              =>  me.pure((IntLit(1), Nil)) // Implement the extra stuff later
        // Lab 1 Task 2.1 end
    }

    // Using String because it is easier since if we used enums it will still be troublesome to add more in the future
    def genBinOpExp(opName: String, e1: Exp, e2: Exp)(using me:StateCogenMonad[StateInfo]):CogenState[(Opr, List[(Label,Instr)])] = for {
        (up1, down_e1) <- genExp(e1)
        (up2, down_e2) <- genExp(e2)
        x              <- newTemp
        label          <- newLabel

        instr          = opName match {
            case "IPlus"   => IPlus(x, up1, up2)
            case "IMinus"  => IMinus(x, up1, up2)
            case "IMult"   => IMult(x, up1, up2)
            case "IDEqual" => IDEqual(x, up1, up2)
            case "ILThan"  => ILThan(x, up1, up2)
        }
    } yield (x, down_e1 ++ down_e2 ++ List((label, instr))) // (X, down_e1 ++ down_e2 ++ [L:X <- up_e1 op up_e2]) will try understand later but im just manmoding right now

    def cogen(s:Stmt):CogenState[List[(Label,Instr)]] = s match {
        case Nop => StateT{ st => Identity((st, List())) } 
        /*
        GE(e) |- (up_e, down_e)
        L is a new label
        -------------------------------------- (Assign)
        G(X = e) |- down_e ++ [ L: X <- up_e]
        */ 
        case Assign(v, e) => for {
            (u, d) <- genExp(e)
            lbl    <- newLabel
        } yield { 
            val av = var2AVar(v)
            val i = IMove(Temp(av), u)
            (d ++ List((lbl,i)))
        }
        /*
        GE(x) |- (up_e, down_e)
        L1 and L2 are new labels
        ---------------------------------------------------- (Return) 
        G(return x) |-  down_e ++ [ L1: R_ret <- up_e, L2: IReturn ]
        */
        case Ret(x) => for {
            (u, d) <- genExp(VarExp(x))
            lbl1   <- newLabel
            lbl2   <- newLabel
        } yield {
            val r_ret = Regstr("_r_ret")
            val i = IMove(r_ret, u)
            d ++ List((lbl1, i), (lbl2, IRet))
        }
        /*
        GE(cond) |- (up_cond, down_cond)
        LIf is a fresh label
        
        G(thn) |- instrs2
        LEndThen is a fresh label

        LElse is the next label (w/o incr)

        G(els) |- instrs3
        LEndElse is a fresh label

        LEndIf is the next label (w/o incr)
        instrs1 = [LIf: ifn up_cond goto LElse] 
        instrs2' = instrs2 ++ [LEndThen: goto LEndIf] 
        instrs3' = instrs3 ++ [LEndElse: goto LEndIf]
        ---------------------------------------------------- (If)
        G(if cond {thn} else {els}) |- down_cond ++ instrs1 ++ instrs2' ++ instrs3' 
        */
        case If(cond, thn, els) => for {
            (cond_u, cond_d) <- genExp(cond) 
            lblIf            <- newLabel 

            instrs2          <- cogen(thn)
            lblEThen         <- newLabel

            lblElse          <- chkNextLabel

            instrs3          <- cogen(els)
            lblEElse         <- newLabel

            lblEIf           <- chkNextLabel
            instrs1          = List((lblIf, IIfNot(cond_u, lblElse)))
            instrs2a         = instrs2 ++ List((lblEThen, IGoto(lblEIf)))
            instrs3a         = instrs3 ++ List((lblEElse, IGoto(lblEIf)))
        } yield cond_d ++ instrs1 ++ instrs2a ++ instrs3a
        // Lab 1 Task 2.2 
        /*
        LBWhile is the next label (w/o incr) 
        GE(cond) |- (up_cond, down_cond)

        LWhileCondJ is a fresh label
        G(body) |- instrs2 
        LEndBody is a fresh label
        
        LEndWhile is the next label (w/o incr)

        instrs1 = [LWhileCondJ: ifn up_cond goto LEndWhile] 
        instrs2' = instrs2 ++ [ LEndBody: goto LBWhile ]
        --------------------------------------------------------- (While)
        G(while cond {body}) |- down_cond ++ instrs1 ++ instrs2'
        */

        /* 
        enum Stmt {
            case Assign(x:Var, e:Exp)                           done
            case If(cond:Exp, th:List[Stmt], el:List[Stmt])     done
            case Nop                                            done
            case While(cond:Exp, b:List[Stmt])                  done
            case Ret(x:Var)                                     done
        }
         */
        // This is the last statement implemented
        case While(cond, body) => for {
            lbWhile              <- chkNextLabel
            (up_cond, down_cond) <- genExp(cond)

            lWhileCondj          <- newLabel
            instr2               <- cogen(body)
            lEndBody             <- newLabel

            lEndWhile            <- chkNextLabel

            instr1  = List((lWhileCondj, IIfNot(up_cond, lEndWhile))) // If the condition is not met stop the loop and jump to the end of while loop
            instr2a = instr2 ++ List((lEndBody, IGoto(lbWhile))) // Came from end of the while body and go back up to the start (if cond is still true)
        } yield down_cond ++ instr1 ++ instr2a // down_cond ++ instrs1 ++ instrs2'
        // Lab 1 Task 2.2 end
    }

    // Try implement this later
    /*
     for i in {1,n}    G(stmt_i) |- instrs_i
    -------------------------------------------------------- (Sequence)
     G(stmt_1,...,stmt_n) |- instrs_1 ++ ... ++  instrs_n
    */

    def cogen(l:List[Stmt]):CogenState[List[(Label,Instr)]] = for {
        ll <- traverse( (stmt:Stmt) => cogen(stmt), l)
    } yield ll.flatten
}