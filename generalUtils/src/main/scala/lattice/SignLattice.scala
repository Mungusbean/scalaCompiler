package lattice

import lattice.CompleteLattice.{given, *} 

object SignLattice {
    import CompleteLattice.* 
    enum SignAbsVal {
        case Bot    // _|_
        case Minus  // -
        case Plus   // + 
        case Top    // T
        case Zero   // 0
    }

    import SignAbsVal.*
    // Cohort Problem Exercise 2
    given signLattice:CompleteLattice[SignAbsVal] = new CompleteLattice[SignAbsVal] {
        def sqSubSetEq(a: SignAbsVal, b: SignAbsVal): Option[Boolean] = None // TODO: FixMe  
        def lub(a:SignAbsVal, b:SignAbsVal):SignAbsVal = Top // TODO: FixMe
    }
}
