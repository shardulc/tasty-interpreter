package tastyinterpreter

import tastyquery.Trees.*
import scala.collection.mutable.HashMap
import scala.util.{ Try, Success, Failure }
import tastyquery.Contexts.Context
import tastyquery.Names.*
import tastyquery.TypeTrees.TypeIdent
import tastyquery.Spans.NoSpan
import tastyquery.Spans.Span
import tastyquery.Types.*
import tastyquery.Constants.Constant


class TastyEvaluationError(m: String) extends RuntimeException(m)

case class InterpreterLiteral(term: ScalaTerm) extends Tree(NoSpan):
  override def withSpan(span: Span): Tree = InterpreterLiteral(term)
  override protected def calculateType(using Context): Type = ???


def evaluate(env: ScalaEnvironment)(tree: Tree)(using Context): ScalaTerm =
  tree match
    case (t: ClassDef) => evaluateClassDef(env)(t)
    case (t: New) => evaluateNew(env)(t)
    case (t: ValDef) => evaluateValDef(env)(t)
    case (t: DefDef) => evaluateDefDef(env)(t)
    case (t: Block) => evaluateBlock(env)(t)
    case (t: Apply) => evaluateApply(env)(t)
    case Select(qualifier, t) =>
      evaluate(env)(qualifier) match
        case (q: ScalaLazyObject) =>
          evaluateName(q.value.environment)(t)
        case (q: ScalaObject) => evaluateName(q.environment)(t)
        // case (q: ScalaClass) => evaluateName(q.environment)(t)
        case q @ _ => throw TastyEvaluationError(s"don't know how to select in ${q}")
    case (t: Lambda) => evaluateLambda(env)(t)
    case (t: Ident) => evaluateIdent(env)(t)
    // case (t: Assign) => evaluateAssign(env)(t)
    case Typed(t, _) => evaluate(env)(t)
    case EmptyTree => ScalaUnit
    case InterpreterLiteral(t) => t
    case Literal(Constant(t: Int)) => ScalaInt(t)
    case Literal(Constant(_: Unit)) => ScalaUnit
    case t @ _ => throw TastyEvaluationError(s"not implemented for ${t.toString}")

def evaluateClassDef(env: ScalaEnvironment)(tree: ClassDef)(using Context): ScalaUnit =
  env(tree.symbol) = ScalaClass(env, tree.symbol, tree.rhs.constr, tree.rhs.body)
  ScalaUnit

def evaluateNew(env: ScalaEnvironment)(tree: New)(using Context): ScalaObject =
  env.lookup(tree.tpe.asInstanceOf[TypeRef].symbol) match
    case (cls: ScalaClass) =>
      val objEnv = ScalaEnvironment(Some(cls.environment))
      val obj = ScalaObject(objEnv)
      val constructor = cls.constr.copy(
        rhs = InterpreterLiteral(
          ScalaLazyObject(
            objEnv,
            Block(cls.body, InterpreterLiteral(obj))(NoSpan)
          )
        ))(NoSpan)
      evaluateDefDef(objEnv)(constructor, isConstructor = true)
      obj

def evaluateValDef(env: ScalaEnvironment)(tree: ValDef)(using Context): ScalaUnit =
  // condition is true for fields referenced in templates
  if tree.rhs == EmptyTree then ScalaUnit else {
    val evaledRhs = evaluate(env)(tree.rhs) match
      case (result: ScalaApplicable) => result.apply(List.empty)
      case (result: ScalaValue) => result
    env(tree.symbol) = evaledRhs
    ScalaUnit}

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
  evaluate(env)(tree.fun) match
    case (f: ScalaApplicable) => f.apply(tree.args.map(evaluate(env)))
    case f @ _ => throw TastyEvaluationError(s"can't apply ${f}")

def evaluateLambda(env: ScalaEnvironment)(tree: Lambda)(using Context): ScalaFunctionObject =
  val method = evaluate(env)(tree.meth).asInstanceOf[ScalaMethod]
  ScalaFunctionObject(ScalaEnvironment(Some(env)), method)

def evaluateName(env: ScalaEnvironment)(name: TermName)(using Context): ScalaTerm =
  name match
    case SignedName(underlying, _, _) => env.lookupByName(underlying)
    case _ => env.lookupByName(name)

def evaluateIdent(env: ScalaEnvironment)(tree: Ident)(using Context): ScalaTerm =
  evaluateName(env)(tree.name)
