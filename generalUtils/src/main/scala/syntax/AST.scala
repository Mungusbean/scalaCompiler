package syntax

object AST {
  
    /**
     * S ::= X = E ; | return X ; | nop | if E { \overline{S} } else { \overline{S} } | while E { \overline{S} } 
     * E ::= E Op E | X | C | (E)
     * \overline{S} ::= S | S \overline{S}
     * Op ::= + | - | *  
     * C ::= 1 | 2 | ... | true | false 
     * X ::= a | b | c | d 
     * */
    enum Stmt {
        case Assign(x:Var, e:Exp) 
        case If(cond:Exp, th:List[Stmt], el:List[Stmt]) 
        case Nop
        case While(cond:Exp, b:List[Stmt]) 
        case Ret(x:Var)
    }
    
    case class Var(name:String)

    /**
      * extract the name from a variable
      *
      * @param v - variable
      * @return string
      */
    def varname(v:Var):String = v match {
        case Var(name) => name
    }
        
    enum Exp{
        case Plus(e1:Exp, e2:Exp)
        case Minus(e1:Exp, e2:Exp)
        case Mult(e1:Exp, e2:Exp)
        case DEqual(e1:Exp, e2:Exp)
        case LThan(e1:Exp, e2:Exp)
        case ConstExp(l:Const)
        case VarExp(v:Var)
        case ParenExp(e:Exp)
    }

    enum Const{
        case IntConst(v:Int)
        case BoolConst(v:Boolean)
    }

    
}

