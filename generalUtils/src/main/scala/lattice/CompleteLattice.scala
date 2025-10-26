package lattice

object CompleteLattice {
    /**
      * complete lattice 
      * (S, sqsubseteq)
      *  sqsubseteq : S => S => Option[Boolean] compute the partial order
      *  lub : S => S => S  returns the least upper bound, also known as the join
      *  glb : S => S => S  returns the greatest lower bound (not implemented), also known as the meat
      */
    trait CompleteLattice[S] {
        def sqSubSetEq(a:S, b:S):Option[Boolean] 
        def lub(a:S, b:S): S
        def eq(a:S,b:S):Option[Boolean] = for  {
            t1 <- sqSubSetEq(a,b)
            t2 <- sqSubSetEq(b,a)
        } yield (t1 && t2)

        /**
          * Naive Fixed Point algorithm
          *  differs from the notes, f of type S => Either[String, S] instead of S => S, 
          *   i.e. it might fail
          *
          * @param f
          * @param a
          * @return
          */
        def naiveFP(f:S => Either[String, S])(a:S):Either[String, S] = for {
            b <- f(a) 
            r <- eq(a,b) match {
                case Some(true) => Right(a)
                case Some(false) => naiveFP(f)(b) 
                case None => Left(s"naiveFP failed with incomparable states ${a} and ${b}.") 
            }
        } yield r
    }


    given powerSetLattice[A]:CompleteLattice[Set[A]] = new CompleteLattice[Set[A]] {
        def sqSubSetEq(a:Set[A], b:Set[A]):Option[Boolean] =
            if (a.subsetOf(b)) {
                Some(true)
            } else {
                if (b.subsetOf(a)) {
                    Some(false)
                } else {
                    None
                }
            }
        def lub(a: Set[A], b: Set[A]): Set[A] = a union b
    }

    /**
      * map lattice
      *  if a key is absent in a map, it is assumed to be mapping to bottom
      *
      * @param vcl
      * @return
      */
    given mapLattice[K,V](using vcl:CompleteLattice[V]):CompleteLattice[Map[K,V]] = new CompleteLattice[Map[K,V]] {

        def sqSubSetEq(a: Map[K, V], b: Map[K, V]): Option[Boolean] = (a.keySet union b.keySet).toList.foldLeft(Some(true))(
            (acc:Option[Boolean], k) => (acc, a.get(k), b.get(k)) match {
                case (None, _ , _) => None
                case (_, None, _) => Some(true) // absence of key implies bottom
                case (_, _, None) => Some(false)        
                case (Some(v), Some(v1),Some(v2)) => vcl.sqSubSetEq(v1, v2) match {
                    case None => None 
                    case Some(b) => Some(v && b)
                }
            }
        )
        def lub(a: Map[K, V], b: Map[K, V]): Map[K, V] = (a.keySet union b.keySet).toList.foldLeft(Map():Map[K,V])(
            (acc:Map[K,V], k) => (a.get(k), b.get(k)) match { 
                case (Some(v1), Some(v2)) => acc + (k -> (vcl.lub(v1,v2)))
                case (_ , Some(v2)) => acc + (k -> v2) // absence of key implies bottom
                case (Some(v1), _)  => acc + (k -> v1)
                case (None, None)   => acc 
            }
        )
    }

    
}
