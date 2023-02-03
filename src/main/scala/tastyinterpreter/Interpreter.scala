package tastyinterpreter

import tastyquery.Contexts.*
import tastyquery.Names.FullyQualifiedName
import tastyquery.Trees.Tree

class Interpreter(using Context):

  val topLevelEnv = ScalaEnvironment(None)
  val ctx = tastyquery.Contexts.ctx

  def evaluate(tree: Tree) =
    Evaluators.evaluate(topLevelEnv, tree)

  def evaluateDeclarationsInPackage(packageName: FullyQualifiedName) =
    ctx.findPackageFromRoot(packageName).asPackage.declarations
      // asInstanceOf is safe because all DefTree subclasses are Tree subclasses
      .map(_.tree.get.asInstanceOf[Tree])
      .foreach(evaluate)
