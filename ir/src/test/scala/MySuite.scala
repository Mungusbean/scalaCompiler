import scala.language.adhocExtensions
import org.scalatest.funsuite 
import org.scalatest.matchers
import ast.AST.*
import monad.StateT.{given, *}
import ir.PseudoAssembly.*
import ir.Util.{given, *}
import ir.MMUpDown.*
import ir.PseudoAssembly.*
import ir.SSA.*
import ir.CFG.*
import ir.DF.*


class MMUpDownSuite extends funsuite.AnyFunSuite {
    import Stmt.*, Var.*, Exp.*, Const.*
    import Instr.*, Opr.*, DomTree.*

    test("test maximal munch v2 genExp: y + 1") {
        val exp1:Exp = Plus(VarExp(Var("y")), ConstExp(IntConst(1)))
        val expected = List((1,IPlus(Temp(AVar("var_1")),Temp(AVar("y")),IntLit(1))))
        genExp(exp1).run(StateInfo(1,"var", 1)) match {
            case Identity((st_, (opr,instrs))) => {
                // println(instrs)
                assert(expected == instrs)
            }
        }
    }

    test("test maximal munch v2 genExp: (x + 3) * (y - 5)") {
        val exp1:Exp = Mult(Plus(VarExp(Var("x")), ConstExp(IntConst(3))), Minus(VarExp(Var("y")), ConstExp(IntConst(5))))
        val expected = List(
            (1,IPlus(Temp(AVar("var_1")),Temp(AVar("x")),IntLit(3))), 
            (2,IMinus(Temp(AVar("var_2")),Temp(AVar("y")),IntLit(5))), 
            (3,IMult(Temp(AVar("var_3")),Temp(AVar("var_1")),Temp(AVar("var_2")))))
        genExp(exp1).run(StateInfo(1,"var", 1)) match {
            case Identity((st_, (opr,instrs))) => {
                // println(instrs)
                assert(expected == instrs)
            }
        }
    }


    // y = 1
    // x = y + 1
    test("test maximal munch v2: y = 1; x = y + 1") {
        val stmt1 = Assign(Var("y"), ConstExp(IntConst(1)))
        val stmt2 = Assign(Var("x"), Plus(VarExp(Var("y")), ConstExp(IntConst(1))))
        val expected = List(
            (1, IMove(Temp(AVar("y")),IntLit(1))), 
            (2, IPlus(Temp(AVar("var_1")),Temp(AVar("y")),IntLit(1))), 
            (3, IMove(Temp(AVar("x")),Temp(AVar("var_1")))))
        cogen(List(stmt1, stmt2)).run(StateInfo(1,"var", 1)) match {
            case Identity((st_, instrs)) => {
                // println(instrs)
                assert(expected == instrs)
            }
        }
    }    



    // z = (x + 3) * (y - 5)
    // return z
    test("test maximal munch v2: z = (x + 3) * (y - 5); return z") {
        val stmt1 = Assign(Var("z"), Mult(Plus(VarExp(Var("x")), ConstExp(IntConst(3))), Minus(VarExp(Var("y")), ConstExp(IntConst(5)))))
        val stmt2 = Ret(Var("z"))
        val expected = List(
            (1, IPlus(Temp(AVar("var_1")),Temp(AVar("x")),IntLit(3))), 
            (2, IMinus(Temp(AVar("var_2")),Temp(AVar("y")),IntLit(5))), 
            (3, IMult(Temp(AVar("var_3")),Temp(AVar("var_1")),Temp(AVar("var_2")))), 
            (4, IMove(Temp(AVar("z")),Temp(AVar("var_3")))), 
            (5, IMove(Regstr("_r_ret"),Temp(AVar("z")))), 
            (6, IRet)
        )
        cogen(List(stmt1, stmt2)).run(StateInfo(1,"var",1)) match {
            case Identity((st_, instrs)) => {
                // println(instrs)
                assert(expected == instrs)
            }
        }
    }


    test("test maximal munch v2: Multiline") {
        /* x = 0
           y = 10
           i = 0
           while (i < y) {
              x = x + i
              i = i + 1
           }
           return x
        */
        val stmt1 = Assign(Var("x"), ConstExp(IntConst(0)))
        val stmt2 = Assign(Var("y"), ConstExp(IntConst(10)))
        val stmt3 = Assign(Var("i"), ConstExp(IntConst(0)))
        val stmt4 = While(LThan(VarExp(Var("i")), VarExp(Var("y"))), List(
            Assign(Var("x"), Plus(VarExp(Var("x")), VarExp(Var("i")))),
            Assign(Var("i"), Plus(VarExp(Var("i")), ConstExp(IntConst(1))))
        ))
        val stmt5 = Ret(Var("x"))

        val expected = List(
            (1,IMove(Temp(AVar("x")),IntLit(0))), 
            (2,IMove(Temp(AVar("y")),IntLit(10))), 
            (3,IMove(Temp(AVar("i")),IntLit(0))), 
            (4,ILThan(Temp(AVar("var_1")),Temp(AVar("i")),Temp(AVar("y")))), 
            (5,IIfNot(Temp(AVar("var_1")),11)), 
            (6,IPlus(Temp(AVar("var_2")),Temp(AVar("x")),Temp(AVar("i")))), 
            (7,IMove(Temp(AVar("x")),Temp(AVar("var_2")))), 
            (8,IPlus(Temp(AVar("var_3")),Temp(AVar("i")),IntLit(1))), 
            (9,IMove(Temp(AVar("i")),Temp(AVar("var_3")))), 
            (10,IGoto(4)), 
            (11,IMove(Regstr("_r_ret"),Temp(AVar("x")))), 
            (12,IRet))

        
        cogen(List(stmt1, stmt2, stmt3, stmt4, stmt5)).run(StateInfo(1,"var",1)) match {
            case Identity((st_, instrs)) => {
                // println(instrs)
                assert(expected == instrs)
            }
        }
    } // New tests added here

    // ###############  TestDf.scala added here ###########################
     test("testing buildDomTree 1") {
        val g = Map(1 -> List(2), 2 -> List(3), 3 -> List(4),
           4 -> List(5), 5 -> List(6, 11), 6 -> List(7), 7 -> List(8),
           8 -> List(9), 9 -> List(10), 10 -> List(4), 11 -> List(12))
        val expected = Node(1,
            List(Node(2,
                List(Node(3,
                    List(Node(4,
                        List(Node(5,
                            List(Node(6,
                                List(Node(7,
                                    List(Node(8,
                                        List(Node(9,
                                            List(Node(10,List()))))))))), 
                                Node(11,List(Node(12,List())))))))))))))
        buildDomTree(g, 1) match {
            case Left(err) => {
                println(err)
                assert(false)
            }
            case Right(dt) => {
                assert(dt == expected)
            }
        }
    }


    /**
      * 1 -> 2 -> 3 -> 4 -> 5 -> 6 -> 7 -> 13 -> 14
      *                          ^    |
      *                          |    v
      *                          |    8 -> 9 -> 10 -> 11 -> 12
      *                          ---------------------------/
      */
    test("testing buildDomTree 2") {
        val g = Map(1 -> List(2), 2 -> List(3), 3 -> List(4), 4 -> List(5), 
            5 -> List(6), 6 -> List(7), 7 -> List(8, 13), 8 -> List(9),  9 -> List(10), 
            10 -> List(11), 11 -> List(12), 12 -> List(6),  13 -> List(14))
        val expected = Node(1,
            List(Node(2,
                List(Node(3,
                    List(Node(4,
                        List(Node(5,
                            List(Node(6,
                                List(Node(7,
                                    List(Node(8,
                                        List(Node(9,
                                            List(Node(10,
                                                List(Node(11,
                                                    List(Node(12,List()))))))))), 
                                        Node(13,
                                            List(Node(14,List())))))))))))))))))
        buildDomTree(g, 1) match {
            case Left(err) => {
                println(err)
                assert(false)
            }
            case Right(dt) => {
                assert(dt == expected)
            }
        }
    }


    /**
      * 1 -> 2 -> 3 -> 4 -> 5 -> 20 -> 21
      *                ^    |
      *                |    v
      *                |    6 -> 7 -> 8 -> 9 -> 10 -> 11 -> 17 -> 18 -> 19 ---------\
      *                |                         ^    |                             |
      *                |                         |    v                             |
      *                |                         |    12 -> 13 -> 14 -> 15 -> 16    |
      *                |                         \----------------------------/     |
      *                \-----------------------------------------------------------/
      */
    test("testing buildDomTree 3") {
        val g = Map(1 -> List(2), 2 -> List(3), 3 -> List(4),  4 -> List(5), 
        5 -> List(6, 20), 6 -> List(7), 7 -> List(8), 8 -> List(9), 
        9 -> List(10), 10 -> List(11), 11 -> List(12, 17), 12 -> List(13),
        13 -> List(14), 14 -> List(15), 15 -> List(16), 16 -> List(10),
        17 -> List(18), 18 -> List(19), 19 -> List(4), 20 -> List(21)
        )
        val expected = Node(1,
            List(Node(2,
                List(Node(3,
                    List(Node(4,
                        List(Node(5,
                            List(Node(6,
                                List(Node(7,
                                    List(Node(8,
                                        List(Node(9,
                                            List(Node(10,
                                                List(Node(11,
                                                    List(Node(12,
                                                        List(Node(13,
                                                            List(Node(14,
                                                                List(Node(15,
                                                                    List(Node(16,List()))))))))), 
                                                        Node(17,List(Node(18,List(Node(19,List()))))))))))))))))), 
                                Node(20,List(Node(21,List())))))))))))))
        buildDomTree(g, 1) match {
            case Left(err) => {
                println(err)
                assert(false)
            }
            case Right(dt) => {
                assert(dt == expected)
            }
        }
    }

    /**
      * 1 -> 2 -> 3 -> 4 -> 5 -> 11 -> 12
      *                ^    |
      *                |    v
      *                |    6 -> 7 -> 8 -> 9 -> 10
      *                -------------------------/
      */
    test("test buildDFT 1") {
        val g = Map(1 -> List(2), 2 -> List(3), 3 -> List(4),
           4 -> List(5), 5 -> List(6, 11), 6 -> List(7), 7 -> List(8),
           8 -> List(9), 9 -> List(10), 10 -> List(4), 11 -> List(12))
        val dt = Node(1,
            List(Node(2,
                List(Node(3,
                    List(Node(4,
                        List(Node(5,
                            List(Node(6,
                                List(Node(7,
                                    List(Node(8,
                                        List(Node(9,
                                            List(Node(10,List()))))))))), 
                                Node(11,List(Node(12,List())))))))))))))
        
        val expected = Map(1 -> List(), 2 -> List(), 3 -> List(), 4 -> List(4), 
            5 -> List(4), 6 -> List(4), 7 -> List(4), 8 -> List(4),  9 -> List(4),  10 -> List(4),  
            11 -> List(), 12 -> List() ) 
        val result = buildDFT(dt, g)
        assert(result == expected)
    }


    /**
      * 1 -> 2 -> 3 -> 4 -> 5 -> 6 -> 7 -> 13 -> 14
      *                          ^    |
      *                          |    v
      *                          |    8 -> 9 -> 10 -> 11 -> 12
      *                          ---------------------------/
      */
    test("test buildDFT 2") {
        val g = Map(1 -> List(2), 2 -> List(3), 3 -> List(4), 4 -> List(5), 
            5 -> List(6), 6 -> List(7), 7 -> List(8, 13), 8 -> List(9),  9 -> List(10), 
            10 -> List(11), 11 -> List(12), 12 -> List(6),  13 -> List(14))
        val dt = Node(1,
            List(Node(2,
                List(Node(3,
                    List(Node(4,
                        List(Node(5,
                            List(Node(6,
                                List(Node(7,
                                    List(Node(8,
                                        List(Node(9,
                                            List(Node(10,
                                                List(Node(11,
                                                    List(Node(12,List()))))))))), 
                                        Node(13,
                                            List(Node(14,List())))))))))))))))))
        
        val expected = Map(1 -> List(), 2 -> List(), 3 -> List(), 4 -> List(), 5 -> List()
            , 6 -> List(6), 7 -> List(6), 8 -> List(6), 9 -> List(6), 10 -> List(6), 11 -> List(6), 12 -> List(6)
            , 13 -> List(), 14 -> List() )  
        val result = buildDFT(dt, g)
        assert(result == expected)
    }


    /**
      * 1 -> 2 -> 3 -> 4 -> 5 -> 20 -> 21
      *                ^    |
      *                |    v
      *                |    6 -> 7 -> 8 -> 9 -> 10 -> 11 -> 17 -> 18 -> 19 ---------\
      *                |                         ^    |                             |
      *                |                         |    v                             |
      *                |                         |    12 -> 13 -> 14 -> 15 -> 16    |
      *                |                         \----------------------------/     |
      *                \-----------------------------------------------------------/
      */
    test("test buildDFT 3") {
        val g = Map(1 -> List(2), 2 -> List(3), 3 -> List(4),  4 -> List(5), 
        5 -> List(6, 20), 6 -> List(7), 7 -> List(8), 8 -> List(9), 
        9 -> List(10), 10 -> List(11), 11 -> List(12, 17), 12 -> List(13),
        13 -> List(14), 14 -> List(15), 15 -> List(16), 16 -> List(10),
        17 -> List(18), 18 -> List(19), 19 -> List(4), 20 -> List(21)
        )
        val dt = Node(1,
            List(Node(2,
                List(Node(3,
                    List(Node(4,
                        List(Node(5,
                            List(Node(6,
                                List(Node(7,
                                    List(Node(8,
                                        List(Node(9,
                                            List(Node(10,
                                                List(Node(11,
                                                    List(Node(12,
                                                        List(Node(13,
                                                            List(Node(14,
                                                                List(Node(15,
                                                                    List(Node(16,List()))))))))), 
                                                        Node(17,List(Node(18,List(Node(19,List()))))))))))))))))), 
                                Node(20,List(Node(21,List())))))))))))))
        
        val expected = Map(1 -> List(), 2 -> List(), 3 -> List(),
            4 -> List(4), 5 -> List(4), 6 -> List(4), 7 -> List(4), 8 -> List(4), 9 -> List(4),
            10 -> List(10, 4), 11 -> List(10, 4), 12 -> List(10), 13 -> List(10), 14 -> List(10), 15 -> List(10),  16 -> List(10),  
            17 -> List(4), 18 -> List(4), 19 -> List(4),
            20 -> List(), 21 -> List())
        val result = buildDFT(dt, g)
        assert(result == expected)
    }

    // ###############  TestSSA.scala added here ###########################
    test("testing buildSSA 1") {
        val pa = List(
            (1,IMove(Temp(AVar("x")),IntLit(0))), 
            (2,IMove(Temp(AVar("y")),IntLit(10))), 
            (3,IMove(Temp(AVar("i")),IntLit(0))), 
            (4,ILThan(Temp(AVar("var_1")),Temp(AVar("i")),Temp(AVar("y")))), 
            (5,IIfNot(Temp(AVar("var_1")),11)), 
            (6,IPlus(Temp(AVar("var_2")),Temp(AVar("x")),Temp(AVar("i")))), 
            (7,IMove(Temp(AVar("x")),Temp(AVar("var_2")))), 
            (8,IPlus(Temp(AVar("var_3")),Temp(AVar("i")),IntLit(1))), 
            (9,IMove(Temp(AVar("i")),Temp(AVar("var_3")))), 
            (10,IGoto(4)), 
            (11,IMove(Regstr("_r_ret"),Temp(AVar("x")))), 
            (12,IRet))

        val expected = List(
            (1,List(),IMove(Temp(AVar("x_1")),IntLit(0))), 
            (2,List(),IMove(Temp(AVar("y_1")),IntLit(10))), 
            (3,List(),IMove(Temp(AVar("i_1")),IntLit(0))), 
            (4,List(
                PhiAssignment(Temp(AVar("i_2")),List((3,AVar("i_1")), (10,AVar("i_3"))),Temp(AVar("i"))), 
                PhiAssignment(Temp(AVar("x_2")),List((3,AVar("x_1")), (10,AVar("x_3"))),Temp(AVar("x")))
                ),ILThan(Temp(AVar("var_1_1")),Temp(AVar("i_2")),Temp(AVar("y_1")))), 
            (5,List(),IIfNot(Temp(AVar("var_1_1")),11)), 
            (6,List(),IPlus(Temp(AVar("var_2_1")),Temp(AVar("x_2")),Temp(AVar("i_2")))), 
            (7,List(),IMove(Temp(AVar("x_3")),Temp(AVar("var_2_1")))), 
            (8,List(),IPlus(Temp(AVar("var_3_1")),Temp(AVar("i_2")),IntLit(1))), 
            (9,List(),IMove(Temp(AVar("i_3")),Temp(AVar("var_3_1")))), 
            (10,List(),IGoto(4)), 
            (11,List(),IMove(Regstr("_r_ret"),Temp(AVar("x_2")))), 
            (12,List(),IRet))
        buildSSA(pa) match {
            case Left(err) => {
                println(err)
                assert(false)
            } 
            case Right(ssa) => assert (ssa == expected)
        }
    }



    test("testing buildSSA 2") {
        /*
        1: x <- input
        2: f <- 0
        3: s <- 1
        4: c <- 0
        5: t <- 0
        6: b <- c < x
        7: ifn b goto 13
        8: t <- f
        9: f <- s
        10: s <- t + f
        11: c <- c + 1
        12: goto 6
        13: r_ret <- s
        14: ret
        */
        val input = Temp(AVar("input"))
        val x = Temp(AVar("x"))
        val f = Temp(AVar("f"))
        val s = Temp(AVar("s"))
        val c = Temp(AVar("c"))
        val b = Temp(AVar("b"))
        val t = Temp(AVar("t"))
        val r_ret = Regstr("_r_ret")
        val pa = List(
            (1, IMove(x, input)),
            (2, IMove(f, IntLit(0))),
            (3, IMove(s, IntLit(1))),
            (4, IMove(c, IntLit(0))),
            (5, IMove(t, IntLit(0))),
            (6, ILThan(b, c, x)),
            (7, IIfNot(b, 13)),
            (8, IMove(t, f)),
            (9, IMove(f, s)),
            (10, IPlus(s, t, f)),
            (11, IPlus(c, c, IntLit(1))),
            (12, IGoto(6)),
            (13, IMove(r_ret, s)),
            (14, IRet)
        )
        val expected = List(
            (1,List(),IMove(Temp(AVar("x_1")),Temp(AVar("input")))), 
            (2,List(),IMove(Temp(AVar("f_1")),IntLit(0))), 
            (3,List(),IMove(Temp(AVar("s_1")),IntLit(1))), 
            (4,List(),IMove(Temp(AVar("c_1")),IntLit(0))), 
            (5,List(),IMove(Temp(AVar("t_1")),IntLit(0))), 
            (6,List(
                PhiAssignment(Temp(AVar("c_2")),List((5,AVar("c_1")), (12,AVar("c_3"))),Temp(AVar("c"))), 
                PhiAssignment(Temp(AVar("f_2")),List((5,AVar("f_1")), (12,AVar("f_3"))),Temp(AVar("f"))), 
                PhiAssignment(Temp(AVar("s_2")),List((5,AVar("s_1")), (12,AVar("s_3"))),Temp(AVar("s"))),
                PhiAssignment(Temp(AVar("t_2")),List((5,AVar("t_1")), (12,AVar("t_3"))),Temp(AVar("t"))) 
                ),ILThan(Temp(AVar("b_1")),Temp(AVar("c_2")),Temp(AVar("x_1")))), 
            (7,List(),IIfNot(Temp(AVar("b_1")),13)), 
            (8,List(),IMove(Temp(AVar("t_3")),Temp(AVar("f_2")))), 
            (9,List(),IMove(Temp(AVar("f_3")),Temp(AVar("s_2")))), 
            (10,List(),IPlus(Temp(AVar("s_3")),Temp(AVar("t_3")),Temp(AVar("f_3")))), 
            (11,List(),IPlus(Temp(AVar("c_3")),Temp(AVar("c_2")),IntLit(1))), 
            (12,List(),IGoto(6)), 
            (13,List(),IMove(Regstr("_r_ret"),Temp(AVar("s_2")))), 
            (14,List(),IRet)) 
        buildSSA(pa) match {
            case Left(err) => {
                println(err)
                assert(false)
            } 
            case Right(ssa) => assert (ssa == expected)
        }
    }


    test("testing buildSSA 3") {
        /*
        1: x <- input
        2: r <- 0
        3: i <- 0
        4: b <- i < x
        5: ifn b goto 20
        6: f <- 0
        7: s <- 1
        8: j <- 0
        9: t <- 0
        10: b <- j < i
        11: ifn b goto 17
        12: t <- f
        13: f <- s
        14: s <- t + f
        15: j <- j + 1
        16: goto 10
        17: r <- r + s
        18: i <- i + 1
        19: goto 4
        20: _ret_r <- r
        21: ret
        */
        val input = Temp(AVar("input"))
        val x = Temp(AVar("x"))
        val r = Temp(AVar("r"))
        val f = Temp(AVar("f"))
        val s = Temp(AVar("s"))
        val i = Temp(AVar("i"))
        val j = Temp(AVar("j"))
        val b = Temp(AVar("b"))
        val t = Temp(AVar("t"))
        val r_ret = Regstr("_r_ret")
        val pa = List(
            (1, IMove(x, input)),
            (2, IMove(r, IntLit(0))),
            (3, IMove(i, IntLit(0))),
            (4, ILThan(b, i, x)),
            (5, IIfNot(b, 20)),
            (6, IMove(f, IntLit(0))),
            (7, IMove(s, IntLit(1))),
            (8, IMove(j, IntLit(0))),
            (9, IMove(t, IntLit(0))),
            (10, ILThan(b, j, i)),
            (11, IIfNot(b, 17)),
            (12, IMove(t, f)),
            (13, IMove(f, s)),
            (14, IPlus(s, t, f)),
            (15, IPlus(j, j, IntLit(1))),
            (16, IGoto(10)),
            (17, IPlus(r, r, s)),
            (18, IPlus(i, i, IntLit(1))),
            (19, IGoto(4)),
            (20, IMove(r_ret, r)),
            (21, IRet)
        )
        val expected = List(
            (1,List(),IMove(Temp(AVar("x_1")),Temp(AVar("input")))), 
            (2,List(),IMove(Temp(AVar("r_1")),IntLit(0))), 
            (3,List(),IMove(Temp(AVar("i_1")),IntLit(0))), 
            (4,List(
                PhiAssignment(Temp(AVar("i_2")),List((3,AVar("i_1")), (19,AVar("i_3"))),Temp(AVar("i"))),
                PhiAssignment(Temp(AVar("r_2")),List((3,AVar("r_1")), (19,AVar("r_3"))),Temp(AVar("r"))) 
                ),ILThan(Temp(AVar("b_1")),Temp(AVar("i_2")),Temp(AVar("x_1")))), 
            (5,List(),IIfNot(Temp(AVar("b_1")),20)), 
            (6,List(),IMove(Temp(AVar("f_1")),IntLit(0))), 
            (7,List(),IMove(Temp(AVar("s_1")),IntLit(1))), 
            (8,List(),IMove(Temp(AVar("j_1")),IntLit(0))), 
            (9,List(),IMove(Temp(AVar("t_1")),IntLit(0))), 
            (10,List(
                PhiAssignment(Temp(AVar("b_2")),List((9,AVar("b_1")), (16,AVar("b_3"))),Temp(AVar("b"))), 
                PhiAssignment(Temp(AVar("f_2")),List((9,AVar("f_1")), (16,AVar("f_3"))),Temp(AVar("f"))), 
                PhiAssignment(Temp(AVar("j_2")),List((9,AVar("j_1")), (16,AVar("j_3"))),Temp(AVar("j"))), 
                PhiAssignment(Temp(AVar("s_2")),List((9,AVar("s_1")), (16,AVar("s_3"))),Temp(AVar("s"))),
                PhiAssignment(Temp(AVar("t_2")),List((9,AVar("t_1")), (16,AVar("t_3"))),Temp(AVar("t"))) 
                ),ILThan(Temp(AVar("b_3")),Temp(AVar("j_2")),Temp(AVar("i_2")))), 
            (11,List(),IIfNot(Temp(AVar("b_3")),17)), 
            (12,List(),IMove(Temp(AVar("t_3")),Temp(AVar("f_2")))), 
            (13,List(),IMove(Temp(AVar("f_3")),Temp(AVar("s_2")))), 
            (14,List(),IPlus(Temp(AVar("s_3")),Temp(AVar("t_3")),Temp(AVar("f_3")))), 
            (15,List(),IPlus(Temp(AVar("j_3")),Temp(AVar("j_2")),IntLit(1))), 
            (16,List(),IGoto(10)), 
            (17,List(),IPlus(Temp(AVar("r_3")),Temp(AVar("r_2")),Temp(AVar("s_2")))), 
            (18,List(),IPlus(Temp(AVar("i_3")),Temp(AVar("i_2")),IntLit(1))), 
            (19,List(),IGoto(4)), 
            (20,List(),IMove(Regstr("_r_ret"),Temp(AVar("r_2")))), 
            (21,List(),IRet))
        buildSSA(pa) match {
            case Left(err) => {
                println(err)
                assert(false)
            } 
            case Right(ssa) => assert (ssa == expected)
        }
    }




}