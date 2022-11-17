package tastyinterpreter

import tastyquery.Contexts.Context
import tastyquery.Names.*
import tastyquery.Trees.*
import tastyquery.Spans.NoSpan
import tastyquery.Types.*

import testinputs.basic
import testinputs.otherbasic

class BasicSuite extends TastyInterpreterSuite:

  testWithInterpreter("classes and objects") { interpreter =>
    val basicPkg = makePackageName("testinputs", "basic")
    interpreter.evaluateDeclarationsInPackage(basicPkg)

    assertInterpretedEquals(
      interpreter.evaluate(
        Apply(makeSelectTree(basicPkg, "Foo", "doit"), List.empty)(NoSpan)),
      basic.Foo.doit,
      5)

    assertInterpretedEquals(
      interpreter.evaluate(
        Apply(makeSelectTree(basicPkg, "Foo", "doitagain"), List.empty)(NoSpan)),
      basic.Foo.doitagain,
      6)
  }

  testWithInterpreter("inner class") { interpreter =>
    val basicPkg = makePackageName("testinputs", "otherbasic")
    interpreter.evaluateDeclarationsInPackage(basicPkg)

    assertInterpretedEquals(
      interpreter.evaluate(
        Apply(makeSelectTree(basicPkg, "Foobar", "doit"), List.empty)(NoSpan)),
      otherbasic.Foobar.doit,
      7)
  }


  testWithInterpreter("closures top-level") { interpreter =>
    val basicPkg = makePackageName("testinputs", "basic")
    interpreter.evaluateDeclarationsInPackage(basicPkg)

    assertInterpretedEquals(
      interpreter.evaluate(makeSelectTree(basicPkg, "ClosuresTopLevel$package", "y")),
      basic.y,
      1)

    assertInterpretedEquals(
      interpreter.evaluate(makeSelectTree(basicPkg, "ClosuresTopLevel$package", "z")),
      basic.z,
      2)

    assertInterpretedEquals(
      interpreter.evaluate(makeSelectTree(basicPkg, "ClosuresTopLevel$package", "yy")),
      basic.yy,
      1)
  }

  testWithInterpreter("closures") { interpreter =>
    val basicPkg = makePackageName("testinputs", "basic")
    interpreter.evaluateDeclarationsInPackage(basicPkg)

    assertInterpretedEquals(
      interpreter.evaluate(makeSelectTree(basicPkg, "Closures", "y")),
      basic.Closures.y,
      1)

    assertInterpretedEquals(
      interpreter.evaluate(makeSelectTree(basicPkg, "Closures", "z")),
      basic.Closures.z,
      2)

    assertInterpretedEquals(
      interpreter.evaluate(makeSelectTree(basicPkg, "Closures", "yy")),
      basic.Closures.yy,
      1)
  }
