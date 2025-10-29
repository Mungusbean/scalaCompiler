package ir

import monad.Functor.*
import monad.Applicative.*
import monad.Monad.{given, *}
import monad.StateT.{given, *}

import ast.AST.*
import ir.PseudoAssembly.*

object Util {

    case class StateInfo(nextNum:Int, prefix:String, nextLbl:Int)

    trait StateCogenMonad[S] extends StateMonad[S] { 
    }

    given stateCogenMonad[S]:StateCogenMonad[S] = new StateCogenMonad[S]{}


    def get: StateT[StateInfo, Identity, StateInfo] = StateT{ st => Identity(st, st) }
    def put(st:StateInfo): StateT[StateInfo, Identity, Unit] = StateT{ _ => Identity(st, ())}



    type CogenState[A] = StateT[StateInfo, Identity, A]

    /** 
     * issue a new name and increment the nextNum in the state
     * */

    def newName:CogenState[String] = for {
        st <- get
        _  <- put(st.copy(nextNum= st.nextNum+1))
    } yield (s"${st.prefix}_${st.nextNum}")
    
    /**
      * issue a new temp var
      *
      * @return a new temp var as an operand
      */
    def newTemp:CogenState[Opr] = for {
        n <- newName
    } yield Opr.Temp(AVar(n))

    /**
      * create a register
      *
      * @param s - name of the register
      * @return a register as an operand
      */
    def mkRegstr(s:String):Opr = Opr.Regstr(s)

    /** 
     * issue a new label and increment the nextLbl in the state
     * */
    def newLabel:CogenState[Label] = for {
        st <- get
        _  <- put(st.copy(nextLbl= st.nextLbl+1))
    } yield st.nextLbl

    /** 
     * return the next label WITHOUT incrementing the nextLbl in the state
     * */

    def chkNextLabel:CogenState[Label] = for {
        st <- get
    } yield st.nextLbl

    /**
      * Convert a variable to an AVar
      *
      * @param x - Var
      * @return
      */
    def var2AVar(x:Var):AVar = x match { 
        case Var(n) => AVar(n)
    }

    

}