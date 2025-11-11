import scala.language.adhocExtensions
import org.scalatest.funsuite 
import org.scalatest.matchers
import ast.AST.*
import interpreter.SimpInt.*



class TestSimpInt extends funsuite.AnyFunSuite {
    import Stmt.* 
    import Exp.*
    import Const.*

    test("test simp evalExp with c + s"){
        val s = Var("s")
        val c = Var("c")
        val delta = Map(s -> IntConst(3), c -> IntConst(1))
        val exp1:Exp = Plus(VarExp(c), VarExp(s))
        val expected = IntConst(4)
        evalExp(delta,exp1) match {
            case Left(error) => {
                assert(false)
            }
            case Right(result) => {
                assert(result == expected)
            }
        }
    } 


    test("test simp evalExp with c < (s * 2)"){
        val s = Var("s")
        val c = Var("c")
        val delta = Map(s -> IntConst(3), c -> IntConst(1))
        val exp1:Exp = LThan(VarExp(c), Mult(VarExp(s), ConstExp(IntConst(2))))
        val expected = BoolConst(true)
        evalExp(delta,exp1) match {
            case Left(error) => {
                assert(false)
            }
            case Right(result) => {
                assert(result == expected)
            }
        }
    } 

    /*
    x = input;
    s = 0;
    c = 0;
    while c < x {
        s = c + s;
        c = c + 1;
    }
    return s;
    */
    test("test simp interpretor with sum(3)"){
        val input = Var("input")
        val x = Var("x")
        val s = Var("s")
        val c = Var("c")
        val p = List(
            Assign(x, VarExp(input)),
            Assign(s, ConstExp(IntConst(0))),
            Assign(c, ConstExp(IntConst(0))),
            While(LThan(VarExp(c), VarExp(x)), List(
                Assign(s, Plus(VarExp(c), VarExp(s))),
                Assign(c, Plus(VarExp(c), ConstExp(IntConst(1)))
            ))),
            Ret(s)
        )
        val expected = 3
        interpret(p, 3) match {
            case Left(value) => assert(false)
            case Right(IntConst(result)) => assert(expected == result)
            case Right(BoolConst(result)) => assert(false)
        }
    }
    /*
    x = input;
    f = 0;
    s = 1;
    c = 0;
    t = 0;
    while c < x {
        t = f;
        f = s;
        s = t + f;
        c = c + 1;
    }
    return s;
    */
    test("test simp interpretor with fib(4)") {
        val input = Var("input")
        var x = Var("x")
        val f = Var("f")
        val s = Var("s")
        val c = Var("c")
        val t = Var("t")
        val p = List(
            Assign(x, VarExp(input)),
            Assign(f, ConstExp(IntConst(0))),
            Assign(s, ConstExp(IntConst(1))),
            Assign(c, ConstExp(IntConst(0))),
            Assign(t, ConstExp(IntConst(0))),
            While(LThan(VarExp(c), VarExp(x)), List(
                Assign(t, VarExp(f)),
                Assign(f, VarExp(s)),
                Assign(s, Plus(VarExp(t), VarExp(f))),
                Assign(c, Plus(VarExp(c), ConstExp(IntConst(1)))
            ))),
            Ret(s)
        )
        val expected = 5
        interpret(p, 4) match {
            case Left(value) => assert(false)
            case Right(IntConst(result)) => assert(expected == result)
            case Right(BoolConst(result)) => assert(false)
        }
    }


    /*
    x = input;
    r = 0;
    i = 0;
    while i < x {
        f = 0;
        s = 1;
        j = 0;
        t = 0;
        while j < i {
            t = f;
            f = s;
            s = t + f;
            j = j + 1;
        }
        r = r + s;
        i = i + 1;
    }
    return r;
    */
    test("test simp interpretor with sum((0 to 10).map(fib(_)))") {
        val input = Var("input")
        var x = Var("x")
        val r = Var("r")
        val i = Var("i")
        val f = Var("f")
        val s = Var("s")
        val j = Var("j")
        val t = Var("t")
        val p = List(
            Assign(x, VarExp(input)),
            Assign(r, ConstExp(IntConst(0))),
            Assign(i, ConstExp(IntConst(0))),
            While(LThan(VarExp(i), VarExp(x)), List(
                Assign(f, ConstExp(IntConst(0))),
                Assign(s, ConstExp(IntConst(1))),
                Assign(j, ConstExp(IntConst(0))),
                Assign(t, ConstExp(IntConst(0))),
                While(LThan(VarExp(j), VarExp(i)), List(
                    Assign(t, VarExp(f)),
                    Assign(f, VarExp(s)),
                    Assign(s, Plus(VarExp(t), VarExp(f))),
                    Assign(j, Plus(VarExp(j), ConstExp(IntConst(1)))
                ))),
                Assign(r, (Plus(VarExp(r), VarExp(s)))),
                Assign(i, Plus(VarExp(i), ConstExp(IntConst(1))))
            )),
            Ret(r)
        )
        val expected = 7
        interpret(p, 4) match {
            case Left(value) => assert(false)
            case Right(IntConst(result)) => assert(expected == result)
            case Right(BoolConst(result)) => assert(false)
        }
    }
}