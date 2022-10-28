package tastyinterpreter

import scala.collection.mutable.HashMap

import tastyquery.Trees.*
import tastyquery.Types.*
import tastyquery.Contexts.*
import tastyquery.Names.*
import tastyquery.Spans.NoSpan
import tastyquery.Constants.Constant
import tastyquery.Flags


class TastyEvaluationError(m: String) extends RuntimeException(m)

def evaluate(env: ScalaEnvironment)(tree: Tree)(using Context): ScalaBox[ScalaTerm] =
  tree match
    case (t: ClassDef) => evaluateClassDef(env)(t)
    case (t: New) => evaluateNew(env)(t)
    case (t: ValDef) => evaluateValDef(env)(t)
    case (t: DefDef) => evaluateDefDef(env)(t)
    case (t: Block) => evaluateBlock(env)(t)
    case (t: Apply) => evaluateApply(env)(t)
    case ts @ Select(qualifier, t) =>
      evaluate(env)(qualifier).value match
        case (q: ScalaObject) => q
        case (q: ScalaLazyValue) => q.value
        case q @ _ => throw TastyEvaluationError(s"don't know how to select in ${q}")
      TypeEvaluators.evaluate(env)(ts.tpe) match
        case (st: ScalaTerm) => st
        case _ => throw TastyEvaluationError("type of Select node must be ScalaTerm")
    case (t: Lambda) => evaluateLambda(env)(t)
    case (t: Ident) => evaluateIdent(env)(t)
    case Typed(t, _) => evaluate(env)(t)
    case EmptyTree => ScalaUnit
    case Literal(Constant(t: Int)) => ScalaInt(t)
    case Literal(Constant(_: Unit)) => ScalaUnit
    case (t: This) => evaluateThis(env)(t)
    case (t: Assign) => evaluateAssign(env)(t)
    case t @ _ => throw TastyEvaluationError(s"not implemented for ${t.toString}")

def evaluateClassDef(env: ScalaEnvironment)(tree: ClassDef)(using Context): ScalaUnit =
  def constr(objEnv: ScalaEnvironment)(arguments: List[ScalaTerm])(using Context): ScalaObject =
    tree.rhs.constr.paramLists match
      case Left(vds) :: Nil =>
        // 1. bind arguments
        vds.zip(arguments).foreach((vd, arg) =>
          objEnv(tree.symbol.getDecl(vd.name).get.asTerm) = arg)
        // 2. evaluate superclass constructor *in current env*
        // (how?)
        // 3. evaluate template body
        tree.rhs.body.foreach(evaluate(objEnv))
        objEnv.getThisObject
      case _ => throw TastyEvaluationError("don't know how to init this")
  val clsEnv = ScalaEnvironment(Some(env))
  val clsConstr = ScalaConstructor(constr)
  clsEnv(tree.rhs.constr.symbol) = clsConstr
  env(tree.symbol) = ScalaClass(clsEnv, tree.symbol, clsConstr)
  ScalaUnit

def evaluateNew(env: ScalaEnvironment)(tree: New)(using Context): ScalaObject =
  TypeEvaluators.evaluate(env)(tree.tpe) match
    case (cls: ScalaClass) =>
      lazy val (objEnv: ScalaEnvironment, obj: ScalaObject) =
        (ScalaEnvironment(Some(cls.environment), Some(obj)), ScalaObject(objEnv))
      cls.constr.setObjEnv(objEnv)
      obj
    case _ => throw TastyEvaluationError("don't know how to init this")

def evaluateThis(env: ScalaEnvironment)(t: This)(using Context): ScalaObject =
  env.getThisObject

def evaluateValDef(env: ScalaEnvironment)(tree: ValDef)(using Context): ScalaUnit =
  // condition is true for fields referenced in templates
  if tree.rhs == EmptyTree then ScalaUnit else {
    def evaledRhs = evaluate(env)(tree.rhs).value match
      case (result: ScalaApplicable) => result.apply(List.empty)
      case (result: ScalaValue) => result
      case (result: ScalaLazyValue) => result.value
    env(tree.symbol) =
      if tree.symbol.is(Flags.Lazy) then ScalaLazyValue(evaledRhs)
      else evaledRhs
    ScalaUnit}

def evaluateAssign(env: ScalaEnvironment)(tree: Assign)(using Context): ScalaUnit =
  evaluate(env)(tree.lhs).set(evaluate(env)(tree.rhs))
  ScalaUnit

def evaluateDefDef(env: ScalaEnvironment)(tree: DefDef, isConstructor: Boolean = false)
    (using Context): ScalaUnit =
  if tree.name != SimpleName("writeReplace") then
    // assume only one, val params clause for now
    val valParams: List[ValDef] = tree.paramLists match
      case Left(p) :: _ => p
      case _ => List.empty
    env(tree.symbol) = ScalaMethod(env, valParams.map(_.symbol), tree.rhs, isConstructor)
  ScalaUnit

def evaluateBlock(env: ScalaEnvironment)(tree: Block)(using Context): ScalaTerm =
  tree.stats.foreach(evaluate(env))
  evaluate(env)(tree.expr)

def evaluateApply(env: ScalaEnvironment)(tree: Apply)(using Context): ScalaValue =
  // ignore possibility of call-by-name
  evaluate(env)(tree.fun).value match
    case (f: ScalaApplicable) => f.apply(tree.args.map(evaluate(env)))
    case f @ _ => throw TastyEvaluationError(s"can't apply ${f}")

def evaluateLambda(env: ScalaEnvironment)(tree: Lambda)(using c: Context): ScalaFunctionObject =
  val method = evaluate(env)(tree.meth).value.asInstanceOf[ScalaMethod]
  ScalaFunctionObject(ScalaEnvironment(Some(env)), method)

def evaluateIdent(env: ScalaEnvironment)(tree: Ident)(using Context): ScalaBox[ScalaTerm] =
  env.lookup(tree.tpe.asInstanceOf[TermRef].symbol)
