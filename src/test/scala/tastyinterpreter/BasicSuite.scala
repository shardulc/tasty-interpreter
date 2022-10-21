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
      (Apply(makeSelectTree(basicPkg, "Foo", "doit"), List.empty)(NoSpan),
        assertScalaEquals(otherbasic.Foo.doit)))
      .map(evaluateAndCheck(globalEnv))
  }
