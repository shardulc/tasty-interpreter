package tastyinterpreter

import testinputs.inheritance.*


class InheritanceSuite extends TastyInterpreterSuite:

  testWithInterpreter("simple inheritance") { interpreter =>
    val pkg = makePackageName("testinputs", "inheritance")
    interpreter.evaluateDeclarationsInPackage(pkg)

    assertInterpretedEquals(
      interpreter.evaluate(makeSelectTree(pkg, "Simple", "test1")),
      Simple.test1, 2)
    assertInterpretedEquals(
      interpreter.evaluate(makeSelectTree(pkg, "Simple", "test2")),
      Simple.test2, 2)
    assertInterpretedEquals(
      interpreter.evaluate(makeSelectTree(pkg, "Simple", "test3")),
      Simple.test3, 14)
    assertInterpretedEquals(
      interpreter.evaluate(makeSelectTree(pkg, "Simple", "test4")),
      Simple.test4, 2)
    assertInterpretedEquals(
      interpreter.evaluate(makeSelectTree(pkg, "Simple", "test5")),
      Simple.test5, 10)
    assertInterpretedEquals(
      interpreter.evaluate(makeSelectTree(pkg, "Simple", "test6")),
      Simple.test6, 6)
    assertInterpretedEquals(
      interpreter.evaluate(makeSelectTree(pkg, "Simple", "test7")),
      Simple.test7, 110)
  }

  testWithInterpreter("inheritance with overriding") { interpreter =>
    val pkg = makePackageName("testinputs", "inheritance")
    interpreter.evaluateDeclarationsInPackage(pkg)

    assertInterpretedEquals(
      interpreter.evaluate(makeSelectTree(pkg, "Override", "test1")),
      Override.test1, 2)
    assertInterpretedEquals(
      interpreter.evaluate(makeSelectTree(pkg, "Override", "test1_1")),
      Override.test1_1, 22)
    assertInterpretedEquals(
      interpreter.evaluate(makeSelectTree(pkg, "Override", "test2")),
      Override.test2, 3)
    assertInterpretedEquals(
      interpreter.evaluate(makeSelectTree(pkg, "Override", "test2_1")),
      Override.test2_1, 33)
    assertInterpretedEquals(
      interpreter.evaluate(makeSelectTree(pkg, "Override", "test3")),
      Override.test3, 105)
    assertInterpretedEquals(
      interpreter.evaluate(makeSelectTree(pkg, "Override", "test4")),
      Override.test4, 3)
    assertInterpretedEquals(
      interpreter.evaluate(makeSelectTree(pkg, "Override", "test5")),
      Override.test5, 15)
    assertInterpretedEquals(
      interpreter.evaluate(makeSelectTree(pkg, "Override", "test6")),
      Override.test6, 0)
    assertInterpretedEquals(
      interpreter.evaluate(makeSelectTree(pkg, "Override", "test7")),
      Override.test7, 13)
  }

  testWithInterpreter("initialization order in inheritance") { interpreter =>
    val pkg = makePackageName("testinputs", "inheritance")
    interpreter.evaluateDeclarationsInPackage(pkg)

    assertInterpretedEquals(
      interpreter.evaluate(makeSelectTree(pkg, "InitOrder", "test1")),
      InitOrder.test1, 0)
    assertInterpretedEquals(
      interpreter.evaluate(makeSelectTree(pkg, "InitOrder", "test2")),
      InitOrder.test2, 3)
    assertInterpretedEquals(
      interpreter.evaluate(makeSelectTree(pkg, "InitOrder", "test3")),
      InitOrder.test3, 5)
    assertInterpretedEquals(
      interpreter.evaluate(makeSelectTree(pkg, "InitOrder", "test4")),
      InitOrder.test4, 7)
    assertInterpretedEquals(
      interpreter.evaluate(makeSelectTree(pkg, "InitOrder", "test5")),
      InitOrder.test5, 3)
    assertInterpretedEquals(
      interpreter.evaluate(makeSelectTree(pkg, "InitOrder", "test6")),
      InitOrder.test6, 0)
    assertInterpretedEquals(
      interpreter.evaluate(makeSelectTree(pkg, "InitOrder", "test7")),
      InitOrder.test7, 7)
    assertInterpretedEquals(
      interpreter.evaluate(makeSelectTree(pkg, "InitOrder", "test8")),
      InitOrder.test8, 91)
    assertInterpretedEquals(
      interpreter.evaluate(makeSelectTree(pkg, "InitOrder", "test9")),
      InitOrder.test9, 119)
    assertInterpretedEquals(
      interpreter.evaluate(makeSelectTree(pkg, "InitOrder", "test10")),
      InitOrder.test10, 3)
    assertInterpretedEquals(
      interpreter.evaluate(makeSelectTree(pkg, "InitOrder", "test11")),
      InitOrder.test11, 0)
    assertInterpretedEquals(
      interpreter.evaluate(makeSelectTree(pkg, "InitOrder", "test12")),
      InitOrder.test12, 11)
    assertInterpretedEquals(
      interpreter.evaluate(makeSelectTree(pkg, "InitOrder", "test13")),
      InitOrder.test13, 0)
    assertInterpretedEquals(
      interpreter.evaluate(makeSelectTree(pkg, "InitOrder", "test14")),
      InitOrder.test14, 187)
  }

  testWithInterpreter("scoping in inheritance") { interpreter =>
    val pkg = makePackageName("testinputs", "inheritance")
    interpreter.evaluateDeclarationsInPackage(pkg)

    assertInterpretedEquals(
      interpreter.evaluate(makeSelectTree(pkg, "Scopes", "test11")),
      Scopes.test11, 190)
    assertInterpretedEquals(
      interpreter.evaluate(makeSelectTree(pkg, "Scopes", "test12")),
      Scopes.test12, 51)
    assertInterpretedEquals(
      interpreter.evaluate(makeSelectTree(pkg, "Scopes", "test13")),
      Scopes.test13, 38)
    assertInterpretedEquals(
      interpreter.evaluate(makeSelectTree(pkg, "Scopes", "test20")),
      Scopes.test20, 34)
    assertInterpretedEquals(
      interpreter.evaluate(makeSelectTree(pkg, "Scopes", "test21")),
      Scopes.test21, 187)
    assertInterpretedEquals(
      interpreter.evaluate(makeSelectTree(pkg, "Scopes", "test22")),
      Scopes.test22, 34)
    assertInterpretedEquals(
      interpreter.evaluate(makeSelectTree(pkg, "Scopes", "test23")),
      Scopes.test23, 119)
    assertInterpretedEquals(
      interpreter.evaluate(makeSelectTree(pkg, "Scopes", "test41")),
      Scopes.test41, 19)
    assertInterpretedEquals(
      interpreter.evaluate(makeSelectTree(pkg, "Scopes", "test42")),
      Scopes.test42, 19)
    assertInterpretedEquals(
      interpreter.evaluate(makeSelectTree(pkg, "Scopes", "test43")),
      Scopes.test43, 13)
    assertInterpretedEquals(
      interpreter.evaluate(makeSelectTree(pkg, "Scopes", "test44")),
      Scopes.test44, 38)
    assertInterpretedEquals(
      interpreter.evaluate(makeSelectTree(pkg, "Scopes", "test52")),
      Scopes.test52, 26)
    assertInterpretedEquals(
      interpreter.evaluate(makeSelectTree(pkg, "Scopes", "test53")),
      Scopes.test53, 13)
    assertInterpretedEquals(
      interpreter.evaluate(makeSelectTree(pkg, "Scopes", "test54")),
      Scopes.test54, 26)
    assertInterpretedEquals(
      interpreter.evaluate(makeSelectTree(pkg, "Scopes", "test61")),
      Scopes.test61, 17)
    assertInterpretedEquals(
      interpreter.evaluate(makeSelectTree(pkg, "Scopes", "test62")),
      Scopes.test62, 19)
  }
