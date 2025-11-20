package semantic

import ast.AST.*
import monad.Monad.*


object TypeInf {
    import Stmt.*
    import Exp.*
    enum Type {
        case IntTy
        //case FloatTy
        //case StringTy
        case BoolTy
    }

    enum ExType {
        case MonoType(t:Type)
        case TypeVar(n:String)
    }

    type TypeEnv = Map[Var, Type]
    
    /**
      * Type constraints
      */
    type TypeConstrs = Set[(ExType, ExType)]

    import ExType.*
    /**
      * extract type variable names from a set of type constraints
      *
      * @param tcs type constraints
      * @return a set of names
      */
    def getTVars(tcs:TypeConstrs):Set[String] = tcs.toList.flatMap( p => p match {
        case (TypeVar(n1), TypeVar(n2)) => List(n1,n2)
        case (TypeVar(n1), _) => List(n1)
        case (_, TypeVar(n2)) => List(n2)
        case (_, _) => Nil
    }
    ).toSet

    /**
      * type substitutions, mapping tyvar name to ExType
      */
    enum TypeSubst {
        case Empty // [] 
        case RevComp(s:(String, ExType), psi:TypeSubst) //  psi compose s
    }
    import TypeSubst.*

    /**
     * make a singleton type subsitution
     *
     * @param n - variable name
     * @param t - the type for substitution
     * @return
     */
    def single(n:String, t:ExType):TypeSubst = RevComp((n, t), Empty)

    /**
     * composition of two type substitutions 
     *
     * @param ts1
     * @param ts2
     * @return ts1 o ts2
     */
    def compose(ts1:TypeSubst, ts2:TypeSubst):TypeSubst = ts2 match {
        case Empty => ts1
        case RevComp(x,ts3) => RevComp(x, compose(ts1, ts3))
    }

    trait Substitutable[A] {
        def applySubst(typeSubst:TypeSubst)(a:A):A 
    }

    /**
      * Apply a type substutiton to an extended type 
      *
      * @param tysubst
      * @param type
      * @return either an error or a grounded monotype
      */
    given exTypeSubstitutable:Substitutable[ExType] = new Substitutable[ExType]{
        def applySubst(tysubst:TypeSubst)(ty:ExType):ExType = tysubst match {
            // Lab 2 Task 2.1
            // enum ExType {
            //     case MonoType(t:Type)
            //     case TypeVar(n:String)
            // }
            case Empty => ty // just return the original extype
            case RevComp((alpha, ty1), ts1) => {
                val ty2 = applySubst(ts1)(ty) // apply the substition once to find out the type ty2
                ty2 match {
                    case TypeVar(n) if n == alpha => applySubst(ts1)(ty1) // [T_hat/ alpha]alpha (if n == alpha) continue down the chain
                    case _ => ty2 // [T_hat/ alpha]beta = beta and [T_hat/ alpha]T = T (both are presented by ty2)
                }
            }
            // case _ => ty // fixme
            // Lab 2 Task 2.1 end
        }
    }

    /**
      * Apply a type substution to a pair of things
      */
    given pairSubstitutable[A,B](using ia:Substitutable[A])(using ib:Substitutable[B]):Substitutable[(A,B)] = new Substitutable[(A,B)]{
        def applySubst(tysubst:TypeSubst)(p:(A,B)): (A,B) = p match {
            case (a,b) => {
                val a1 = ia.applySubst(tysubst)(a)
                val b1 = ib.applySubst(tysubst)(b)
                (a1,b1)
            }
        }
    }

    /**
      * Apply a type substution to a list of things
      */

    given listSubstitutable[A](using i:Substitutable[A]):Substitutable[List[A]] = new Substitutable[List[A]]{
        def applySubst(typeSubst: TypeSubst)(la: List[A]): List[A] = la.map(a => i.applySubst(typeSubst)(a))
    }
    

    import Type.*

    trait Infer[A] {
        def infer(a:A):TypeConstrs
    }

    /**
      * Type inference for SIMP statements
      */

    given infList[A](using infA:Infer[A]):Infer[List[A]] = new Infer[List[A]] {
        def infer(l:List[A]):TypeConstrs = 
            l.foldLeft(Set())( (k, a) => infA.infer(a) union k)
    }

    given infStmt:Infer[Stmt] = new Infer[Stmt] {
        def infer(s:Stmt):TypeConstrs = s match {
            // stmt cases
            // enum Stmt {
            //     case Assign(x:Var, e:Exp)                        done
            //     case If(cond:Exp, th:List[Stmt], el:List[Stmt])  not done
            //     case Nop                                         done 
            //     case While(cond:Exp, b:List[Stmt])               not done
            //     case Ret(x:Var)                                  done
            // }
            case Nop => Set() 
            case Assign(x, e) => {
                val n = varname(x)
                val alphax = TypeVar(n)
                inferExp(e) match {
                    case (exTy, k) => k + ((alphax, exTy))
                }
            }
            case Ret(x) => Set()
            // Lab 2 Task 2.3
            case If(cond, th, el) => {
                val kBool = inferExp(cond) match {
                    case (condTy, kCond) => kCond + ((condTy, MonoType(BoolTy)))
                }
                val kTh = infList.infer(th)
                val kEl = infList.infer(el)
                kBool.union(kTh).union(kEl)
            }
            case While(cond, body) => {
                val kBool = inferExp(cond) match {
                    case (condTy, kCond) => kCond + ((condTy, MonoType(BoolTy)))
                }
                val kBody = infList.infer(body)
                kBool.union(kBody)
            }
            // case _ => Set() // fixme
            // Lab 2 Task 2.3 end
            
        }
    }

    import Const.*
    /**
      * Type inference for SIMP expressions
      *
      * @param e
      * @return
      */
    def inferExp(e:Exp):(ExType, TypeConstrs) = e match {
        case ConstExp(IntConst(v)) => (MonoType(IntTy), Set())
        case ConstExp(BoolConst(v)) => (MonoType(BoolTy), Set())
        case VarExp(v) => {
            val n = varname(v)
            (TypeVar(n), Set())
        }
        case ParenExp(e) => inferExp(e)
        // Lab 2 Task 2.3
        // enum Exp{
        //   case Plus(e1:Exp, e2:Exp)    done
        //   case Minus(e1:Exp, e2:Exp)   done
        //   case Mult(e1:Exp, e2:Exp)    done
        //   case Div(e1:Exp, e2:Exp)     done
        //   case DEqual(e1:Exp, e2:Exp)  done
        //   case NEqual(e1:Exp, e2:Exp)  done
        //   case LThan(e1:Exp, e2:Exp)   done
        //   case LEqual(e1:Exp, e2:Exp)  done
        //   case GThan(e1:Exp, e2:Exp)   not implemented (added by me)
        //   case GEqual(e1:Exp, e2:Exp)  not implemented (added by me)
        //   case ConstExp(l:Const)       done
        //   case VarExp(v:Var)           done
        //   case ParenExp(e:Exp)         done
        // }
        case Plus(e1, e2)   => inferExp1_Exp2(e1)(e2)()
        case Minus(e1, e2)  => inferExp1_Exp2(e1)(e2)()
        case Mult(e1, e2)   => inferExp1_Exp2(e1)(e2)()
        case Div(e1, e2)    => inferExp1_Exp2(e1)(e2)()
        case DEqual(e1, e2) => inferExp1_Exp2(e1)(e2)(_ => MonoType(BoolTy))
        case NEqual(e1, e2) => inferExp1_Exp2(e1)(e2)(_ => MonoType(BoolTy))
        case LThan(e1, e2)  => inferExp1_Exp2(e1)(e2)(_ => MonoType(BoolTy))
        case LEqual(e1, e2) => inferExp1_Exp2(e1)(e2)(_ => MonoType(BoolTy))
        case _ => (MonoType(IntTy), Set()) // fixme (leaving here as i have unimplemented stuff)
        // Lab 2 Task 2.3 end        
    }
    // Helper for inferExp
    def inferExp1_Exp2(e1:Exp)(e2:Exp)(expected_type: ExType => ExType = identity):(ExType, TypeConstrs) = {
        val (e1ty, e1k) = inferExp(e1)
        val (e2ty, e2k) = inferExp(e2)
        val k_union = e1k.union(e2k) + ((e1ty, e2ty)) 
        (expected_type(e1ty), k_union)
    }

    /**
      * unification type class
      */
    trait Unifiable[A] {
        def mgu(a:A):Either[String,TypeSubst]
    }
   
    /**
      * unifying two ExTypes
      */
    given extypesUnifiable:Unifiable[(ExType, ExType)] = new Unifiable[(ExType, ExType)] {
        def mgu(p:(ExType,ExType)):Either[String,TypeSubst] = p match {
            // Lab 2 Task 2.2
            case (MonoType(IntTy), MonoType(IntTy))     => Right(Empty)
            case (MonoType(BoolTy), MonoType(BoolTy))   => Right(Empty)
            case (TypeVar(alpha), t)                    => Right(RevComp((alpha, t), Empty))
            case (t, TypeVar(alpha))                    => Right(RevComp((alpha, t), Empty))
            case _ => Left(s"error: unable to unify ${p.toString}") // fixme
            // Lab 2 Task 2.2 end
        }
    }

    /**
      * unifying a set of type constraints (i.e. a set of (ExType, ExType))
      */
    given typeConstrsUnifiable:Unifiable[TypeConstrs] = new Unifiable[TypeConstrs] {
        def mgu(tyconstrs:TypeConstrs) :Either[String,TypeSubst] = {
            listUnifiable.mgu(tyconstrs.toList) 
        } 
    }
    
    given listUnifiable[A](using u:Unifiable[A])(using s:Substitutable[List[A]]):Unifiable[List[A]] = new Unifiable[List[A]] {
        def mgu(l:List[A]):Either[String, TypeSubst] = {
            l match {
                // Lab 2 Task 2.2
                case Nil            => Right(Empty) // mgu() = []
                case t1_t2 :: k   => for { // (T_1,T_2) cup k 
                    psi1    <- u.mgu(t1_t2) // mgu(T1, T2)
                    k_prime = s.applySubst(psi1)(k) // apply psi 1 to k (k_prime will be of type List[A])
                    psi2    <- mgu(k_prime) // run this mgu on k_prime
                } yield compose(psi2, psi1)
                // case _ => Left("TODO") // fixme
                // Lab 2 Task 2.2 end
            }
        }
    }




    /**
      * grounding a type variable's name given a type substitution 
      *
      * @param varname
      * @param subst
      * @param i
      * @return
      */
    def ground(varname:String, subst:TypeSubst)(using i:Substitutable[ExType]):Either[String, Type] = i.applySubst(subst)(TypeVar(varname)) match {
        case MonoType(t) => Right(t)
        case _ => Left(s"error: type inference failed. ${varname}'s type cannot be grounded ${subst}.")
    }

    /**
      * top level type inference function
      *
      * @param s - list of SIMP Statements
      * @param i - inference statement type class instance
      * @param u - unifiable type class instance
      * @return either an error or a type environment
      */
    def typeInf(s:List[Stmt])(using i:Infer[List[Stmt]])(using st:Substitutable[ExType])(using u:Unifiable[TypeConstrs]):Either[String, TypeEnv] = {
        val typeConstraints = i.infer(s)
        u.mgu(typeConstraints) match {
            case Left(errorMessage) => Left(errorMessage)
            case Right(subst) => {
                val varnames:List[String] = getTVars(typeConstraints).toList                
                def agg(acc:TypeEnv,varname:String):Either[String, TypeEnv] = for {
                    ty <- ground(varname, subst) 
                } yield (acc + (Var(varname) -> ty))
                foldM(agg)(Map():TypeEnv)(varnames)
            }
        }
    }


}
