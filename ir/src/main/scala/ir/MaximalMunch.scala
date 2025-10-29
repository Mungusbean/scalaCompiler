package ir

import monad.Functor.*
import monad.Applicative.*
import monad.Monad.*
import monad.StateT.*
import ast.AST.*
import ir.PseudoAssembly.*
import ir.Util.*


object MaximalMunch {
    import Stmt.*
    import Exp.*
    import Const.*
    import Instr.*
    import Opr.*
    
    /*
     for i in {1,n}    G(stmt_i) |- instrs_i
    -------------------------------------------------------- (Sequence)
     G(stmt_1,...,stmt_n) |- instrs_1 ++ ... ++  instrs_n
    */
    def cogen(stmts:List[Stmt]):CogenState[List[(Label,Instr)]] = for {
        ll <- traverse( (stmt:Stmt)=> cogen(stmt), stmts)
    } yield ll.flatten


    def cogen(stmt:Stmt):CogenState[List[LabeledInstr]] = stmt match {
    /*
    ------------- (Nop)
    G(nop) |- []
    */ 
        case Nop => StateT{ st => Identity(st, List()) } 
    /*
     G(x)(e) |- instrs
    -------------------------------------------------------- (Assign)
     G(x = e) |- instrs
    */
        case Assign(x, exp) => cogen(Temp(var2AVar(x)), exp)
    /*
     G(r_ret)(x) |- instrs
     L is a fresh label
    -------------------------------------------------------- (Return)
     G(return x) |- instrs ++ [ L: IRet ]
    */
        case Ret(x) => for {            
            l   <- cogen(mkRegstr("_r_ret"), VarExp(x))
            lbl <- newLabel
        } yield l ++ List((lbl, IRet))

    /*
    t is a fresh var
    G(t)(cond) |- instrs0
    LIfCondJ is a fresh label

    G(thn) |- instrs2
    LEndThen is a fresh label 

    LElse is the next label (w/o incr)

    G(els) |- instrs3
    LEndElse is a fresh label

    LEndIf is the next label (w/o incr)
    instrs1 = [LIf: ifn t goto LElse ]
    instrs2' = instrs2 ++ [LEndThen:goto LEndIf]
    instrs3' = instrs3 ++ [LEndElse:goto LEndIf]
    -------------------------------------------------------- (If)
    G(if cond {thn} else {els}) |- instrs0 ++ instrs1 ++ instrs2' ++ instrs3' 
    */
        case If(cond, thn, els) => for {
            t        <- newTemp
            instrs0  <- cogen(t, cond)
            lblIfCj  <- newLabel 

            instrs2  <- cogen(thn)
            lblEThen <- newLabel 

            lblElse  <- chkNextLabel

            instrs3  <- cogen(els) 
            lblEElse <- newLabel

            lblEIf   <- chkNextLabel

            instrs1  =  List((lblIfCj, IIfNot(t, lblElse)))
            instrs2a =  instrs2 ++ List((lblEThen, IGoto(lblEIf)))
            instrs3a =  instrs3 ++ List((lblEElse, IGoto(lblEIf)))
        } yield instrs0 ++ instrs1 ++ instrs2a ++ instrs3a
    
    /*
    LBWhile is the next label (w/o incr)
    t is a fresh var    
    G(t)(cond) |- instrs0
    LWhileCj is a fresh label
    G(body) |- instrs2 
    LEndBody is a fresh label 

    LEndWhile is the next label (w/o incr)

    instrs1 = [LWhileCj: ifn t goto LEndWhile]
    instrs2' = instrs2 ++ [ LEndBody: goto LBWhile ]
    ----------------------------------------------------- (While)
    G(while cond {body}) |- instrs0 ++ instrs1 ++ insts2'
    */
        case While(cond, body) => for {
            lblBWhile   <- chkNextLabel
            t           <- newTemp
            instrs0     <- cogen(t, cond)

            lblWhileCj  <- newLabel 
            instrs2     <- cogen(body) 
            lblEndBody  <- newLabel

            lblEndWhile <- chkNextLabel 

            instrs1     =  List((lblWhileCj, IIfNot(t, lblEndWhile)))
            instrs2a    =  instrs2++List((lblEndBody, IGoto(lblBWhile)))
        } yield instrs0 ++ instrs1 ++ instrs2a
    }



    def cogen(x:Opr, e:Exp):CogenState[List[LabeledInstr]] = e match {
        /*
        L is a fresh label
        ---------------------- (Const)
        G(X)(c) |- [L: X <- c]  
        */
        case ConstExp(IntConst(v)) => for {
            lbl <- newLabel
        } yield List((lbl,IMove(x, IntLit(v))))
        case ConstExp(BoolConst(b)) if b => for {
            lbl <- newLabel
        } yield List((lbl,IMove(x, IntLit(1))))        
        case ConstExp(BoolConst(b))      => for {
            lbl <- newLabel
        } yield List((lbl,IMove(x, IntLit(0))))        
        /*
        L is a fresh label
        ---------------------- (Var)
        G(X)(Y) |- [L: X <- Y]   
        */
        case VarExp(v) => for {
            lbl <- newLabel
        } yield List((lbl, IMove(x, Temp(var2AVar(v)))))

        /*
        G(X)(E) |- instrs
        ---------------------- (Paren)
        G(X)((E)) |- instrs
        */
        case ParenExp(e) => cogen(x, e)

        /*
        t1 is a fresh var    t2 is a fresh var
        G(t1)(e1) |- instrs1 
        G(t2)(e2) |- instrs2 
        L is a fresh label
        ------------------------------------------ (Op)
        G(X)(e1 op e2) |- instrs1 ++ instrs2 ++ [ L:X <- t1 op t2]
        */
        // d <- e1-e2
        case Minus(e1, e2) => for {
            t1  <- newTemp
            t2  <- newTemp
            l1  <- cogen(t1, e1)
            l2  <- cogen(t2, e2)
            lbl <- newLabel
        } yield l1++l2++List((lbl, IMinus(x, t1, t2)))
        // d <- e1+e2
        case Plus(e1, e2) => for {
            t1  <- newTemp
            t2  <- newTemp
            l1  <- cogen(t1, e1)
            l2  <- cogen(t2, e2)
            lbl <- newLabel
        } yield l1++l2++List((lbl, IPlus(x, t1, t2)))
        // d <- e1*e2
        case Mult(e1, e2) => for {
            t1  <- newTemp
            t2  <- newTemp
            l1  <- cogen(t1, e1)
            l2  <- cogen(t2, e2)
            lbl <- newLabel
        } yield l1++l2++List((lbl,IMult(x, t1, t2)))
        // d <- e1 == e2
        case DEqual(e1, e2) => for {
            t1  <- newTemp
            t2  <- newTemp
            l1  <- cogen(t1, e1)
            l2  <- cogen(t2, e2)
            lbl <- newLabel
        } yield  l1++l2++List((lbl,IDEqual(x, t1, t2)))
        // d <- e1 < e2
        case LThan(e1, e2) => for {
            t1  <- newTemp
            t2  <- newTemp
            l1  <- cogen(t1, e1)
            l2  <- cogen(t2, e2)
            lbl <- newLabel
        } yield  l1++l2++List((lbl,ILThan(x, t1, t2)))
 
    }
}