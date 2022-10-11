package tastyinterpreter

import tastyquery.Trees.*
import scala.collection.mutable.HashMap
import scala.util.{ Try, Success, Failure }
import tastyquery.Contexts.Context
import tastyquery.Names.*
import tastyquery.TypeTrees.TypeIdent
import tastyquery.Spans.NoSpan
import tastyquery.Spans.Span
import tastyquery.Constants.Constant

def mapUnderTry[T, U](lt: List[T], f: T => Try[U]): Try[List[U]] =
  lt.foldLeft(Try(List.empty[U]))((lu, elem) => lu.flatMap(slu => f(elem).transform(
    sf => Success(slu :+ sf),
    ff => Failure(ff))))


class TastyEvaluationError(m: String) extends RuntimeException(m)

case class InterpreterLiteral(term: ScalaTerm) extends Tree(NoSpan):
  override def withSpan(span: Span): Tree = InterpreterLiteral(term)


def evaluate(env: ScalaEnvironment)(tree: Tree)(using Context): Try[ScalaTerm] =
  tree match
    case (t: ClassDef) => evaluateClassDef(env)(t)
    case (t: New) => evaluateNew(env)(t)
    case (t: ValDef) => evaluateValDef(env)(t)
    case (t: DefDef) => evaluateDefDef(env)(t)
    case (t: Block) => evaluateBlock(env)(t)
    case (t: Apply) => evaluateApply(env)(t)
    case Select(qualifier, t) =>
      evaluate(env)(qualifier).flatMap(_ match
        case (q: ScalaObject) => evaluateName(q.environment)(t)
        case (q: ScalaClass) => evaluateName(q.environment)(t)
        case q @ _ => Failure(TastyEvaluationError(s"don't know how to select in ${q}")))
    case (t: Lambda) => evaluateLambda(env)(t)
    case (t: Ident) => evaluateIdent(env)(t)
    // case (t: Assign) => evaluateAssign(env)(t)
    case Typed(t, _) => evaluate(env)(t)
    case EmptyTree => Success(ScalaUnit)
    case InterpreterLiteral(t) => Success(t)
    case Literal(Constant(t: Int)) => Success(ScalaInt(t))
    case Literal(Constant(_: Unit)) => Success(ScalaUnit)
    case t @ _ => Failure(TastyEvaluationError(s"not implemented for ${t.toString}"))

def evaluateClassDef(env: ScalaEnvironment)(tree: ClassDef)(using Context): Try[ScalaUnit] =
  env(tree.name) = ScalaClass(env, tree.symbol, tree.rhs.constr, tree.rhs.body)
  Success(ScalaUnit)

def evaluateNew(env: ScalaEnvironment)(tree: New)(using Context): Try[ScalaObject] =
  evaluateTypeIdent(env)(tree.tpt.asInstanceOf[TypeIdent])
    .map(_.asInstanceOf[ScalaClass])
    .map(cls =>
      val objEnv = ScalaEnvironment(Some(cls.environment))
      val obj = ScalaObject(objEnv)
      val constructor = cls.constr.copy(
        rhs = Block(cls.body, InterpreterLiteral(obj))(NoSpan))(NoSpan)
      evaluateDefDef(objEnv)(constructor, isConstructor = true)
      obj)

def evaluateValDef(env: ScalaEnvironment)(tree: ValDef)(using Context): Try[ScalaUnit] =
  // condition is true for fields referenced in templates
  if tree.rhs == EmptyTree then Success(ScalaUnit) else
  evaluate(env)(tree.rhs)
    .flatMap { _ match
      case (result: ScalaApplicable) => result.apply(env, List.empty)
      case (result: ScalaValue) => Success(result)
    }
    .map { (result: ScalaValue) =>
      env(tree.name) = result
      ScalaUnit
    }

def evaluateDefDef(env: ScalaEnvironment)(tree: DefDef, isConstructor: Boolean = false)
    (using Context): Try[ScalaUnit] =
  if tree.name != SimpleName("writeReplace") then
    // assume only one, val params clause for now
    val valParams: List[ValDef] = tree.paramLists match
      case Left(p) :: _ => p
      case _ => List.empty
    env(tree.name) = ScalaMethod(env, valParams.map(_.name), tree.rhs, isConstructor)
  Success(ScalaUnit)

def evaluateBlock(env: ScalaEnvironment)(tree: Block)(using Context): Try[ScalaTerm] =
  mapUnderTry(tree.stats, evaluate(env)).flatMap(_ => evaluate(env)(tree.expr))

def evaluateApply(env: ScalaEnvironment)(tree: Apply)(using Context): Try[ScalaValue] =
  // ignore possibility of call-by-name  
  evaluate(env)(tree.fun).flatMap(fun => fun match
    case (f: ScalaApplicable) => f.apply(env, tree.args)
    case _ => Failure(TastyEvaluationError(s"can't apply ${fun}")))

def evaluateLambda(env: ScalaEnvironment)(tree: Lambda)(using Context): Try[ScalaFunctionObject] =
  evaluate(env)(tree.meth).map(meth =>
    ScalaFunctionObject(ScalaEnvironment(Some(env)), meth.asInstanceOf[ScalaMethod]))

def evaluateName(env: ScalaEnvironment)(tree: TermName)(using Context): Try[ScalaTerm] =
  tree match
    case SignedName(underlying, _, _) => env.lookup(underlying)
    case (_: SimpleName) => env.lookup(tree)

def evaluateIdent(env: ScalaEnvironment)(tree: Ident)(using Context): Try[ScalaTerm] =
  evaluateName(env)(tree.name)

def evaluateTypeIdent(env: ScalaEnvironment)(tree: TypeIdent)(using Context): Try[ScalaType] =
  env.lookup(tree.name)

// def evaluateAssign(env: ScalaEnvironment)(tree: Assign)(using Context): Try[ScalaUnit] =
//   evaluate(env)(tree.lhs).flatMap { _ match
//     case (lhs: ScalaReference) =>
//       evaluate(env)(tree.rhs).map(rhs => lhs.set(rhs))
//   }.map(_ => ScalaUnit)
  
  
