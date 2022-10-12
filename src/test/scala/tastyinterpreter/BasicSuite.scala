package tastyinterpreter

import tastyquery.Contexts.Context
import tastyquery.Names.*
import tastyquery.Trees.*
import tastyquery.Spans.NoSpan
import tastyquery.Types.NoType

import testinputs.basic

class BasicSuite extends TastyInterpreterSuite:

  testWithCtx("classes and objects") { ctx =>
    given Context = ctx
    val globalEnv = globalEnvironment()
    evaluateDeclarationsInPackage(globalEnv, makePackageName("testinputs", "basic")).flatMap(_ =>
      mapUnderTry(List(
        (Apply(makeSelectTree("Foo", "doit"), List.empty)(NoSpan),
          assertScalaEquals(basic.Foo.doit)),
        (Apply(makeSelectTree("Foo", "doitagain"), List.empty)(NoSpan),
          assertScalaEquals(basic.Foo.doitagain))),
        evaluateAndCheck(globalEnv)))
  }

