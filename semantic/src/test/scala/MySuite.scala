import scala.language.adhocExtensions
import org.scalatest.funsuite 
import org.scalatest.matchers
import ast.AST.*
import semantic.TypeInf.{given, *}


class TestTypeInf extends funsuite.AnyFunSuite {
    import Stmt.* 
    import Exp.*
    import Const.*
    import Type.*
    import ExType.*
    import TypeSubst.*

    test("test typeinf type substitution [int/a]a"){
        val psi = RevComp(("a",MonoType(IntTy)), Empty)
        val expected = MonoType(IntTy)
        val result = exTypeSubstitutable.applySubst(psi)(TypeVar("a"))
        assert(result == expected)
    }

    test("test typeinf type substitution [int/a]b"){
        val psi = RevComp(("a",MonoType(IntTy)), Empty)
        val expected = TypeVar("b")
        val result = exTypeSubstitutable.applySubst(psi)(TypeVar("b"))
        assert(result == expected)
    }

    test("test typeinf type substitution [int/a]bool"){
        val psi = RevComp(("a",MonoType(IntTy)), Empty)
        val expected = MonoType(BoolTy)
        val result = exTypeSubstitutable.applySubst(psi)(MonoType(BoolTy))
        assert(result == expected)
    }

    test("test typeinf type substitution ([int/a]o[a/b])b"){
        val psi = RevComp(("b",TypeVar("a")), RevComp(("a",MonoType(IntTy)), Empty))
        val expected = MonoType(IntTy)
        val result = exTypeSubstitutable.applySubst(psi)(TypeVar("b"))
        assert(result == expected)
    }

    test("test typeinf unification (Int,Bool) should fail.") {
        val tyconstrs = Set( (MonoType(IntTy), MonoType(BoolTy)))
        typeConstrsUnifiable.mgu(tyconstrs) match {
            case Left(error) => {
                assert(true)
            }
            case Right(tySubst) => {
                assert(false)
            }
        }
    }

    test("test typeinf unification (Int,a) should ground a.") {
        val tyconstrs = Set( (MonoType(IntTy), TypeVar("a")))
        val expected = MonoType(IntTy)
        typeConstrsUnifiable.mgu(tyconstrs) match {
            case Left(error) => {
                assert(false)
            }
            case Right(tySubst) => {
                val result = exTypeSubstitutable.applySubst(tySubst)(TypeVar("a"))
                assert(result == expected)
            }
        }
    }


    test("test typeinf unification {(Int,a), (a, b)} should ground b.") {
        val tyconstrs = Set((MonoType(IntTy), TypeVar("a")), (TypeVar("a"), TypeVar("b")))
        val expected = MonoType(IntTy)
        typeConstrsUnifiable.mgu(tyconstrs) match {
            case Left(error) => {
                assert(false)
            }
            case Right(tySubst) => {
                val result = exTypeSubstitutable.applySubst(tySubst)(TypeVar("b"))
                assert(result == expected)
            }
        }
    }

    test("test typeinf unification {(a, b), (Int, Int) (Int,a)} should ground b.") {
        val tyconstrs = Set((TypeVar("a"), TypeVar("b")), (MonoType(IntTy), MonoType(IntTy)), (MonoType(IntTy), TypeVar("a")))
        val expected = MonoType(IntTy)
        typeConstrsUnifiable.mgu(tyconstrs) match {
            case Left(error) => {
                assert(false)
            }
            case Right(tySubst) => {
                val result = exTypeSubstitutable.applySubst(tySubst)(TypeVar("b"))
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
    test("test typeinf sum()") {
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
        val expected = Map(Var("c") -> IntTy, Var("x") -> IntTy, Var("input") -> IntTy, Var("s") -> IntTy)
        typeInf(p) match {
            case Left(errorMessage) => {
                println(errorMessage)
                assert(false)
            }
            case Right(typeenv) => assert(expected == typeenv)
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
    test("test typeinf fib") {
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
        val expected = Map(Var("f") -> IntTy, Var("input") -> IntTy, Var("x") -> IntTy, Var("s") -> IntTy, Var("c") -> IntTy, Var("t") -> IntTy)
        typeInf(p) match {
            case Left(errorMessage) => {
                println(errorMessage)
                assert(false)
            }
            case Right(typeenv) => assert(expected == typeenv)
        }
    }

    /*
    x = input;          // (α_x, α_input)      
    y = 0;              // (α_y, int)
    while (y < 3) {     // (α_y, int)
        y = y + 1;      // (α_y, int)
    }
    return y;
    */
    test("test typeinf should fail in grounding input and x's types") {
        val input = Var("input")
        var x = Var("x")
        val y = Var("y")
        val p = List(
            Assign(x, VarExp(input)),
            Assign(y, ConstExp(IntConst(0))),
            While(LThan(VarExp(y), ConstExp(IntConst(3))), List(
                Assign(y, Plus(VarExp(y), ConstExp(IntConst(1)))
            ))),
            Ret(y)
        )
        
        typeInf(p) match {
            case Left(errorMessage) => {
                // println(errorMessage)
                assert(true)
            }
            case Right(typeenv) => {
                println("type inference it should fail in grounding x and input's types")
                assert(false)
            }
        }
    }


    /*
    x = input;          // (α_x, α_input)      
    y = 0;              // (α_y, int)
    while (y - x) {     // (α_y, int), (α_x,int)
        y = y + 1;      // (α_y, int)
    }
    return y;
    */
    test("test typeinf should fail in unifying int with bool") {
        val input = Var("input")
        var x = Var("x")
        val y = Var("y")
        val p = List(
            Assign(x, VarExp(input)),
            Assign(y, ConstExp(IntConst(0))),
            While(Minus(VarExp(y), VarExp(x)), List(
                Assign(y, Plus(VarExp(y), ConstExp(IntConst(1)))
            ))),
            Ret(y)
        )
        
        typeInf(p) match {
            case Left(errorMessage) => {
                println(errorMessage)
                assert(true)
            }
            case Right(typeenv) => {
                assert(false)
            }
        }
    }
}