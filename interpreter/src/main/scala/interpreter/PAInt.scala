package interpreter

import ir.PseudoAssembly.*
import monad.Monad.*

/**
  * the Pseudo Assembly program interpretor by implementing the big step operational semantics
  */
object PAInt {
  import Instr.*
  import Opr.*
  
  type PAProg = Map[Label, LabeledInstr]
  type ErrMsg = String

  type EitherErr = [A] =>> Either[ErrMsg, A]

  given eitherErrMonad:Monad[EitherErr] = new Monad[EitherErr] {
    def pure[A](x:A):Either[ErrMsg,A] = Right(x)
    def bind[A,B](fa: EitherErr[A])(f: A => EitherErr[B]): EitherErr[B] =
            fa.flatMap(f)
  }
  

  /**
    * The L environment. 
    * Though we define it as a mapping from Opr to Int, it is actually a mapping from Register or TempVar to Int
    * In the construction (see below) of L environment, we don't add LitInt into the domain of L environment.
    */

  enum LKey {
    case TVKey(n:String)
    case RKey(n:String)
  }
  type LEnv = Map[LKey,Int] 

  import LKey.*

  /**
    * update the local environment with the opr with a new value.
    *
    * @param env
    * @param opr
    * @param v
    * @return
    */
  def update(env:LEnv, opr:Opr, v:Int):LEnv = opr match {
    case IntLit(_) => env // no change
    case Regstr(name) => env + (RKey(name) -> v)
    case Temp(AVar(name)) =>  env + (TVKey(name) -> v)
  }

  /**
    * lookup the local environment to search for the value assocaited with the opr 
    *
    * @param env
    * @param opr
    * @return
    */
  def lookup(env:LEnv, opr:Opr):Either[ErrMsg, Int] = opr match {
    case IntLit(v) => Right(v)
    case Regstr(name) => env.get(RKey(name)) match { 
      case None => Left(s"error: trying to access an uninitialized register ${name}.")
      case Some(v) => Right(v)
    }
    case Temp(AVar(name)) => env.get(TVKey(name)) match { 
      case None => Left(s"error: trying to access an uninitialized temp var ${name}.")
      case Some(v) => Right(v)
    }
  }

  /**
    * small step operational semantics of PA, execute the current labeled instruction by 1 step.
    *
    * @param p - the PA program
    * @param env - the L Environment
    * @param li  - the current labeled instruction
    * @param m   - monad instance
    * @return    - an either type, left with error, right with the updated L Env and the next labeled instruction
    */
  def evalPA(p:PAProg)(env:LEnv, li:LabeledInstr)(using m:Monad[EitherErr]):Either[ErrMsg, (LEnv, LabeledInstr)] = li match {
    /**
      * (pConst), (pRegister), (pTempVar)
      */
    case (lbl, IMove(dst, src)) => dst match {
      case IntLit(_) => Left("error: trying to move a value into a lit.")
      case _ => for { 
        i <- lookup(env,src) 
        env1 <- m.pure(update(env, dst, i))
        next_lbl <- m.pure(lbl + 1)
        next_li <- p.get(next_lbl) match {
          case None => Left(s"error: invalid label ${next_lbl}.")
          case Some(v) => Right(v) 
        } 
      } yield ((env1, next_li))
    }
    /**
      * (pOp)
      */
    case (lbl, IPlus(dst, src1, src2)) => dst match {
      case IntLit(_) => Left("error: trying to move a value into a lit.") 
      case _ => for {
        i1 <- lookup(env,src1)
        i2 <- lookup(env,src2) 
        env1 <- m.pure(update(env, dst, (i1 + i2)))
        next_lbl <- m.pure(lbl + 1)
        next_li <- p.get(next_lbl) match {
          case None => Left(s"error: invalid label ${next_lbl}.")
          case Some(v) => Right(v) 
        } 
      } yield ((env1, next_li))
    }

    case (lbl, IMinus(dst, src1, src2)) => dst match {
      case IntLit(_) => Left("error: trying to move a value into a lit.") 
      case _ => for {
        i1 <- lookup(env,src1)
        i2 <- lookup(env,src2) 
        env1 <- m.pure(update(env, dst, (i1 - i2)))
        next_lbl <- m.pure(lbl + 1)
        next_li <- p.get(next_lbl) match {
          case None => Left(s"error: invalid label ${next_lbl}.")
          case Some(v) => Right(v) 
        } 
      } yield ((env1, next_li))
    }


    case (lbl, IMult(dst, src1, src2)) => dst match {
      case IntLit(_) => Left("error: trying to move a value into a lit.") 
      case _ => for {
        i1 <- lookup(env,src1)
        i2 <- lookup(env,src2) 
        env1 <- m.pure(update(env, dst, (i1 * i2)))
        next_lbl <- m.pure(lbl + 1)
        next_li <- p.get(next_lbl) match {
          case None => Left(s"error: invalid label ${next_lbl}.")
          case Some(v) => Right(v) 
        } 
      } yield ((env1, next_li))
    }

    case (lbl, IDEqual(dst, src1, src2)) => dst match {
      case IntLit(_) => Left("error: trying to move a value into a lit.") 
      case _ => for {
        i1 <- lookup(env,src1)
        i2 <- lookup(env,src2) 
        env1 <- m.pure(update(env, dst, (if (i1 == i2) { 1 } else { 0 })))
        next_lbl <- m.pure(lbl + 1)
        next_li <- p.get(next_lbl) match {
          case None => Left(s"error: invalid label ${next_lbl}.")
          case Some(v) => Right(v) 
        } 
      } yield ((env1, next_li))
    }

    case (lbl, ILThan(dst, src1, src2)) => dst match {
      case IntLit(_) => Left("error: trying to move a value into a lit.") 
      case _ => for {
        i1 <- lookup(env,src1)
        i2 <- lookup(env,src2) 
        env1 <- m.pure(update(env, dst, (if (i1 < i2) { 1 } else { 0 })))
        next_lbl <- m.pure(lbl + 1)
        next_li <- p.get(next_lbl) match {
          case None => Left(s"error: invalid label ${next_lbl}.")
          case Some(v) => Right(v) 
        } 
      } yield ((env1, next_li))
    }

    /**
      * (pIfn0) and (pIfnNot0)
      */

    case (lbl, IIfNot(cond, dest)) => for {
      bv <- lookup(env, cond)
      next <- if (bv == 0) {
        p.get(dest) match {
          case None => Left(s"error: invalid label ${dest}.")
          case Some(li1) => Right(env, li1)
        }
      } else {
        p.get(lbl + 1) match {
          case None => Left(s"error: invalid label ${dest}.")
          case Some(li1) => Right(env, li1)
        }
      }
    } yield next
  
    /**
      * (pGoto)
      */
    case (lbl, IGoto(dest)) => p.get(dest) match {
        case None => Left(s"error: invalid label ${dest}.")
        case Some(li1) => Right(env, li1)
    } 

    // IRet, should not be used.
    case (lbl, IRet) => Right((env, li))
  }

  /**
    * keep running the under p until li reaches the IRet instruction
    *
    * @param p - the PA program
    * @param env - the L Environment
    * @param li  - the current labeled instruction
    * @param m   - monad instance
    * @return    - an either type, left with error, right with the updated L Env and the next labeled instruction
    */
  def runUntilRet(p:PAProg)(env:LEnv, li:LabeledInstr)(using m:Monad[EitherErr]):Either[ErrMsg, (LEnv, LabeledInstr)] = evalPA(p)(env,li) match {
    case Left(err) => Left(err)
    case Right((env1, li1)) => li1 match {
      case (lbl,IRet) => Right((env1, li1))
      case (_,_) => {
        // println(env)
        runUntilRet(p)(env1, li1)
      }
    }
  }

  /**
    * Top level interpret function for a PA program.
    *
    * @param pa - the PA program
    * @param input - the input value
    * @return - either type, Left with error, Right with the Int returned value
    */
  def interpret(pa:List[LabeledInstr], input:Int):Either[ErrMsg, Int] = {
    val p = pa.foldLeft(Map():PAProg)( (m, li) => {
      li match {
        case (lbl, instr) => m + (lbl -> li)
      }
    })
    val env:LEnv = Map( TVKey("input") -> input)
    getEntry(pa) match {
      case None => Left("An empty PA program is given.")
      case Some(li) => runUntilRet(p)(env, li) match {
        case Left(err) => Left(err)
        case Right((env1,li1)) => env1.get(RKey("_r_ret")) match {
          case None =>  Left("error: trying to access an uninitialized register _r_ret")
          case Some(v) => Right(v)
        } 
      }

    }
  }

  /**
    * return the entry label instruction from set of instructions
    *
    * @param pa - a list of label instructions
    * @return Optional labeled instruction, None when the input is an empty list.
    */
  def getEntry(pa:List[LabeledInstr]):Option[LabeledInstr] = {
    pa.sortBy(li => li._1) match {
      case Nil => None 
      case (x::xs) => Some(x)
    }
  }
}