package ir

import ir.PseudoAssembly.*

/**
  * This module contains the implementation of control flow graph utils
  *  1. query for successors and descendants
  *  2. construct a CFG from a PA program
  */
object CFG {
    // a graph is a mapping from a source label to a set of destination labels
    type CFG = Map[Label, List[Label]]

    /**
      * return the list of predcessor of a vertex v in a graph
      *
      * @param g
      * @param v
      * @return
      */
    def predecessors(g:CFG, v:Label):List[Label] = g.toList.flatMap( p => p match {
        case (src, dsts) if dsts.contains(v) => List(src) 
        case (src, dsts) => Nil
    })

    /**
      * return the list of successors of a vertex v in a graph 
      *
      * @param g
      * @param v
      */
    def successors(g:CFG, v:Label):List[Label] = g.get(v) match {
        case None => Nil 
        case Some(vs) => vs
    }
    
    /**
      * find all descendants of vertex v in graph g
      *
      * @param g
      * @param v
      */
    def descendants(g:CFG, v:Label):Set[Label] = {
        def go(acc:Set[Label], vs:List[Label]):Set[Label] = { 
            val new_vs = vs.filter(x => !(acc.contains(x)))
            new_vs match {
                case Nil => acc
                case _::_ => {
                    val next = new_vs.flatMap(x => successors(g,x))
                    go(acc union vs.toSet, next)
                }
            }
        }
        go(Set(), List(v))
    }


    import Instr.*
    
    /**
      * constructing a CFG from a PA program
      *
      * @param p - a PA program
      * @return a CFG
      */
    def buildCFG(p:List[LabeledInstr]):CFG = {
        def go(acc:CFG, li:LabeledInstr):CFG = li match {
            case (label, IRet) => acc
            case (label, IGoto(label2)) => acc.get(label) match {
                case None => acc + (label -> List(label2))
                case Some(labels) => acc + (label -> (label2::labels))
            }
            case (label, IIfNot(cond, label2)) => {
                val label1 = label + 1
                acc.get(label) match {
                    case None => acc + (label -> List(label1, label2))
                    case Some(labels) => acc + (label -> (label1::label2::labels))
                }
            }
            case (label, _) => {
                val label1 = label + 1
                acc.get(label) match {
                    case None => acc + (label -> List(label1))
                    case Some(labels) => acc + (label -> (label1::labels))
                }
            }
        }
        p.foldLeft(Map())(go)
    }

}
