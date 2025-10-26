package semantic

import monad.Monad.*
import lattice.CompleteLattice.{*, given}
import lattice.SignLattice.{*, given}
import ir.PseudoAssembly.* 
import ir.CFG.*


object Util {
    import Instr.* 
    import Opr.* 
    import AVar.* 
    /**
      * extract all variables from a program p
      *
      * @param p
      * @return
      */
    def allVars(p:List[LabeledInstr]):List[String] = p.flatMap( li => li match {
        case (lbl, instr) => instr match {
            case IRet => Nil
            case IDEqual(dest, src1, src2) => vars(dest) ++ vars(src1) ++ vars(src2)
            case IGoto(dest) => Nil
            case IIfNot(cond, dest) => vars(cond) 
            case ILThan(dest, src1, src2) => vars(dest) ++ vars(src1) ++ vars(src2) 
            case IMinus(dest, src1, src2) => vars(dest) ++ vars(src1) ++ vars(src2)
            case IMove(dest, src) => vars(dest) ++ vars(src)
            case IMult(dest, src1, src2) => vars(dest) ++ vars(src1) ++ vars(src2)
            case IPlus(dest, src1, src2) => vars(dest) ++ vars(src1) ++ vars(src2)
        } 
    }).toSet.toList.sorted

    def vars(o:Opr):List[String] = o match {
        case IntLit(v) => Nil 
        case Regstr(name) =>  Nil
        case Temp(AVar(n)) => List(n) 
    }


}
