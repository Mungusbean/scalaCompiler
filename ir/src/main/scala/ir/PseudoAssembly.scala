package ir


object PseudoAssembly {
  
    /**
     * (Labeled Instr) li ::= l : i
     * (Instruction) i ::= d <- s | d <- s op s | ret | ifn s goto l | goto l
     * (Operand)   d,s ::= r | c | t 
     * (Temp Var)    t ::= x | y | .. 
     * (Label)       l ::= 1 | 2 | ...
     * */

    type LabeledInstr = (Label, Instr)
    type Label = Int 
    enum Instr {
        case IMove(dest:Opr, src:Opr)
        case IPlus(dest:Opr, src1:Opr, src2:Opr)
        case IMinus(dest:Opr, src1:Opr, src2:Opr)
        case IMult(dest:Opr, src1:Opr, src2:Opr)
        //case IDiv(dest:Opr, src1:Opr, src2:Opr)
        case IDEqual(dest:Opr, src1:Opr, src2:Opr)
        //case INEqual(dest:Opr, src1:Opr, src2:Opr)
        case ILThan(dest:Opr, src1:Opr, src2:Opr)
        //case ILEqual(dest:Opr, src1:Opr, src2:Opr)
        //case IGThan(dest:Opr, src1:Opr, src2:Opr)
        //case IGEqual(dest:Opr, src1:Opr, src2:Opr)
        case IRet
        case IIfNot(cond:Opr, dest:Label)
        case IGoto(dest:Label)
    }
    
    enum Opr {
        case Regstr(name:String)
        case IntLit(v:Int)
        //case StrLit(v:String)
        //case FloatLit(v:Double)
        case Temp(v:AVar)
    }

    case class AVar(name:String)




}