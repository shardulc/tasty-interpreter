package tastyinterpreter

import tastyquery.Contexts.Context
import tastyquery.Names.*
import tastyquery.Trees.*
import tastyquery.Spans.NoSpan
import tastyquery.Types.*

import testinputs.basic
import testinputs.otherbasic

class BasicSuite extends TastyInterpreterSuite:

  testWithCtx("classes and objects") {
    val globalEnv = globalEnvironment()
    val basicPkg = makePackageName("testinputs", "basic")
    evaluateDeclarationsInPackage(globalEnv, basicPkg)

    List(
      (Apply(makeSelectTree(basicPkg, "Foo", "doit"), List.empty)(NoSpan),
        assertScalaEquals(basic.Foo.doit)),
      (Apply(makeSelectTree(basicPkg, "Foo", "doitagain"), List.empty)(NoSpan),
        assertScalaEquals(basic.Foo.doitagain)))
      .map(evaluateAndCheck(globalEnv))
  }

  testWithCtx("inner class") {
    val globalEnv = globalEnvironment()
    val basicPkg = makePackageName("testinputs", "otherbasic")
    evaluateDeclarationsInPackage(globalEnv, basicPkg)

    List(
      (Apply(makeSelectTree(basicPkg, "Foobar", "doit"), List.empty)(NoSpan),
        assertScalaEquals(otherbasic.Foobar.doit)))
      .map(evaluateAndCheck(globalEnv))
  }

  testWithCtx("closures top-level") { ctx ?=>
    val globalEnv = globalEnvironment()
    val basicPkg = makePackageName("testinputs", "basic")
    evaluateDeclarationsInPackage(globalEnv, basicPkg)

    List(
      (makeSelectTree(basicPkg, "ClosuresTopLevel$package", "y"),
        assertScalaEquals(basic.y)),
      (makeSelectTree(basicPkg, "ClosuresTopLevel$package", "z"),
        assertScalaEquals(basic.z)),
      (makeSelectTree(basicPkg, "ClosuresTopLevel$package", "yy"),
        assertScalaEquals(basic.yy)))
      .map(evaluateAndCheck(globalEnv))
  }

  testWithCtx("closures") { ctx ?=>
    val globalEnv = globalEnvironment()
    val basicPkg = makePackageName("testinputs", "basic")
    evaluateDeclarationsInPackage(globalEnv, basicPkg)

    List(
      (makeSelectTree(basicPkg, "Closures", "y"),
        assertScalaEquals(basic.Closures.y)),
      (makeSelectTree(basicPkg, "Closures", "z"),
        assertScalaEquals(basic.Closures.z)),
      (makeSelectTree(basicPkg, "Closures", "yy"),
        assertScalaEquals(basic.Closures.yy)))
      .map(evaluateAndCheck(globalEnv))
  }
