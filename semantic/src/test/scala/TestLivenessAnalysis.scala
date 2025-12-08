import scala.language.adhocExtensions
import org.scalatest.funsuite 
import org.scalatest.matchers
import ir.PseudoAssembly.*
import semantic.LivenessAnalysis.*
import scala.collection.immutable.HashMap

class TestLivenessAnalysis extends funsuite.AnyFunSuite {
    import Instr.* 
    import Opr.*

    

    /*
    1: x <- inpput
    2: y <- x + 1
    3: z <- y + 1
    4: w <- y * z
    5: _r_ret <- w
    6: ret
    */
    test("test Liveness Analysis with simple ex"){
        val input = Temp(AVar("input"))
        val x = Temp(AVar("x"))
        val y = Temp(AVar("y"))
        val z = Temp(AVar("z"))
        val w = Temp(AVar("w"))
        val r_ret = Regstr("_r_ret")
        val pa = List(
            (1, IMove(x, input)),
            (2, IPlus(y, x, IntLit(1))),
            (3, IPlus(z, y, IntLit(1))),
            (4, IMult(w, y, z)),
            (5, IMove(r_ret, w)),
            (6, IRet)
        )
        val expected = HashMap(
            1 -> Set("input"), 
            2 -> Set("x"), 
            3 -> Set("y"), 
            4 -> Set("y", "z"),
            5 -> Set("w"), 
            6 -> Set()
        ) 

        analyze(pa) match {
            case Left(err) => { 
                println(err)
                assert(false)
            }
            case Right(res) => assert(res == expected) 
        }

    }


    /*
    1: x <- input
    2: s <- 0
    3: c <- 0
    4: b <- c < x
    5: ifn b goto 9
    6: s <- c + s
    7: c <- c + 1
    8: goto 4
    9: _ret_r <- s
    10: ret
    */
    test("test Liveness Analysis with sum()"){
        val input = Temp(AVar("input"))
        val x = Temp(AVar("x"))
        val s = Temp(AVar("s"))
        val c = Temp(AVar("c"))
        val b = Temp(AVar("b"))
        val r_ret = Regstr("_r_ret")
        val pa = List(
            (1, IMove(x, input)),
            (2, IMove(s, IntLit(0))),
            (3, IMove(c, IntLit(0))),
            (4, ILThan(b, c, x)),
            (5, IIfNot(b, 9)),
            (6, IPlus(s, c, s)),
            (7, IPlus(c, c, IntLit(1))),
            (8, IGoto(4)),
            (9, IMove(r_ret, s)),
            (10, IRet)
        )
        val expected = HashMap(
            1 -> Set("input"), 
            2 -> Set("x"), 
            3 -> Set("s", "x"), 
            4 -> Set("c", "s", "x"),
            5 -> Set("x", "c", "s", "b"), 
            6 -> Set("x", "c", "s"), 
            7 -> Set("s", "x", "c"), 
            8 -> Set("c", "s", "x"), 
            9 -> Set("s"), 
            10 -> Set(), 
        ) 
        analyze(pa) match {
            case Left(err) => { 
                println(err)
                assert(false)
            }
            case Right(res) => assert(res == expected) 
        }
    }


    /*
    1: x <- input
    2: y <- 0
    3: s <- 0
    4: b <- y < x
    5: ifn b goto 10
    6: y <- y + 1
    7: t <- s
    8: s <- s + y
    9: goto 4
    10: rret <- s
    11: ret
    */
    test("test Liveness Analysis with notes example"){
        val input = Temp(AVar("input"))
        val x = Temp(AVar("x"))
        val y = Temp(AVar("y"))
        val s = Temp(AVar("s"))
        val t = Temp(AVar("t"))
        val b = Temp(AVar("b"))
        val r_ret = Regstr("_r_ret")
        val pa = List(
            (1, IMove(x, input)),
            (2, IMove(y, IntLit(0))),
            (3, IMove(s, IntLit(0))),
            (4, ILThan(b, y, x)),
            (5, IIfNot(b, 10)),
            (6, IPlus(y, y, IntLit(1))),
            (7, IMove(t, s)),
            (8, IPlus(s, s, y)),
            (9, IGoto(4)),
            (10, IMove(r_ret, s)),
            (11, IRet)
        )
        val expected = HashMap(
            1 -> Set("input"), 
            2 -> Set("x"), 
            3 -> Set("y", "x"), 
            4 -> Set("s", "y", "x"),
            5 -> Set("x", "s", "y", "b"), 
            6 -> Set("x", "s", "y"), 
            7 -> Set("y", "x", "s"), 
            8 -> Set("y", "x", "s"), 
            9 -> Set("s", "y", "x"), 
            10 -> Set("s"), 
            11 -> Set()
        ) 
        analyze(pa) match {
            case Left(err) => { 
                println(err)
                assert(false)
            }
            case Right(res) => assert(res == expected) 
        }
    }


    test("test Liveness Analysis with fib()"){
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
        val expected = HashMap(
            1 -> Set("input"), 
            2 -> Set("x"), 
            3 -> Set("f", "x"), 
            4 -> Set("s", "f", "x"),
            5 -> Set("s", "f", "c", "x"), 
            6 -> Set("c", "s", "f", "x"), 
            7 -> Set("f", "b", "c", "s", "x"), 
            8 -> Set("x", "c", "s", "f"), 
            9 -> Set("x", "c", "t", "s"), 
            10 -> Set("f", "x", "c", "t"), 
            11 -> Set("s", "f", "x", "c"), 
            12 -> Set("c", "s", "f", "x"), 
            13 -> Set("s"), 
            14 -> Set() 
        )
        analyze(pa) match {
            case Left(err) => { 
                println(err)
                assert(false)
            }
            case Right(res) => assert(res == expected) 
        }
    }

}