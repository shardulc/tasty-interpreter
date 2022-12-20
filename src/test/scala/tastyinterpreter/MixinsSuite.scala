package tastyinterpreter

import tastyquery.Contexts.Context
import tastyquery.Names.*
import tastyquery.Trees.*
import tastyquery.Spans.NoSpan
import tastyquery.Types.*

import testinputs.inheritance.{ Mixins, Diamond }


class MixinsSuite extends TastyInterpreterSuite:

  testWithInterpreter("simple mixin") { interpreter =>
    val pkg = makePackageName("testinputs", "inheritance")
    interpreter.evaluateDeclarationsInPackage(pkg)

    assertInterpretedEquals(
      interpreter.evaluate(makeSelectTree(pkg, "Mixins", "test00")),
      Mixins.test00, "ploooeeey")
    assertInterpretedEquals(
      interpreter.evaluate(makeSelectTree(pkg, "Mixins", "test01")),
      Mixins.test01, "ploooeeey ploooeeey")
    assertInterpretedEquals(
      interpreter.evaluate(makeSelectTree(pkg, "Mixins", "test02")),
      Mixins.test02, "Bell")
  }

  testWithInterpreter("mixin overriding class method") { interpreter =>
    val pkg = makePackageName("testinputs", "inheritance")
    interpreter.evaluateDeclarationsInPackage(pkg)

    assertInterpretedEquals(
      interpreter.evaluate(makeSelectTree(pkg, "Mixins", "test10")),
      Mixins.test10, 10)
    assertInterpretedEquals(
      interpreter.evaluate(makeSelectTree(pkg, "Mixins", "test11")),
      Mixins.test11, 2)
    assertInterpretedEquals(
      interpreter.evaluate(makeSelectTree(pkg, "Mixins", "test12")),
      Mixins.test12, 30)
  }

  testWithInterpreter("class overriding mixin method") { interpreter =>
    val pkg = makePackageName("testinputs", "inheritance")
    interpreter.evaluateDeclarationsInPackage(pkg)

    assertInterpretedEquals(
      interpreter.evaluate(makeSelectTree(pkg, "Mixins", "test20")),
      Mixins.test20, 20)
    assertInterpretedEquals(
      interpreter.evaluate(makeSelectTree(pkg, "Mixins", "test21")),
      Mixins.test21, 40)
  }

  testWithInterpreter("parametric trait inheritance") { interpreter =>
    val pkg = makePackageName("testinputs", "inheritance")
    interpreter.evaluateDeclarationsInPackage(pkg)

    assertInterpretedEquals(
      interpreter.evaluate(makeSelectTree(pkg, "Mixins", "test30")),
      Mixins.test30, 30)
    assertInterpretedEquals(
      interpreter.evaluate(makeSelectTree(pkg, "Mixins", "test31")),
      Mixins.test31, 30)
  }

  testWithInterpreter("multiple mixins order") { interpreter =>
    val pkg = makePackageName("testinputs", "inheritance")
    interpreter.evaluateDeclarationsInPackage(pkg)

    assertInterpretedEquals(
      interpreter.evaluate(makeSelectTree(pkg, "Mixins", "test40")),
      Mixins.test40, 5)
    assertInterpretedEquals(
      interpreter.evaluate(makeSelectTree(pkg, "Mixins", "test41")),
      Mixins.test41, 0)
    assertInterpretedEquals(
      interpreter.evaluate(makeSelectTree(pkg, "Mixins", "test42")),
      Mixins.test42, 2)
    assertInterpretedEquals(
      interpreter.evaluate(makeSelectTree(pkg, "Mixins", "test43")),
      Mixins.test43, 4)
    assertInterpretedEquals(
      interpreter.evaluate(makeSelectTree(pkg, "Mixins", "test44")),
      Mixins.test44, 2)
  }

  testWithInterpreter("diamond problem") { interpreter =>
    val pkg = makePackageName("testinputs", "inheritance")
    interpreter.evaluateDeclarationsInPackage(pkg)

    assertInterpretedEquals(
      interpreter.evaluate(makeSelectTree(pkg, "Diamond", "test1")),
      Diamond.test1, 5)
    assertInterpretedEquals(
      interpreter.evaluate(makeSelectTree(pkg, "Diamond", "test2")),
      Diamond.test2, 3)
    assertInterpretedEquals(
      interpreter.evaluate(makeSelectTree(pkg, "Diamond", "test3")),
      Diamond.test3, 2)
  }
