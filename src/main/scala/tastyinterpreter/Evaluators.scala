package tastyinterpreter

import scala.collection.mutable.HashMap

import tastyquery.Trees.*
import tastyquery.Types.*
import tastyquery.Contexts.*
import tastyquery.Names.*
import tastyquery.Spans.NoSpan
import tastyquery.Constants.Constant


class TastyEvaluationError(m: String) extends RuntimeException(m)

def evaluate(env: ScalaEnvironment)(tree: Tree)(using Context): ScalaTerm =
  tree match
    case (t: ClassDef) => evaluateClassDef(env)(t)
    case (t: New) => evaluateNew(env)(t)
    case (t: ValDef) => evaluateValDef(env)(t)
    case (t: DefDef) => evaluateDefDef(env)(t)
    case (t: Block) => evaluateBlock(env)(t)
    case (t: Apply) => evaluateApply(env)(t)
    case ts @ Select(qualifier, t) =>
      {evaluate(env)(qualifier) match 
          case (q: ScalaLazyObject) => q.value
          case (q: ScalaObject) => q
          case q @ _ => throw TastyEvaluationError(s"don't know how to select in ${q}")
      }.environment.lookup(ts.tpe.asInstanceOf[TermRef].symbol)
    case (t: Lambda) => evaluateLambda(env)(t)
    case (t: Ident) => evaluateIdent(env)(t)
    case Typed(t, _) => evaluate(env)(t)
    case EmptyTree => ScalaUnit
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
      objEnv(cls.constr.symbol) = BuiltInMethod { arguments =>
        cls.constr.paramLists match
          case Left(vds) :: Nil =>
            // objEnv.bindAll(vds.map(_.symbol).zip(arguments))
            var idx = 0
            cls.body.foreach(t =>
              t match
                case ValDef(_, _, _, symbol) if symbol.is(tastyquery.Flags.ParamAccessor) =>
                  objEnv(symbol) = arguments(idx)
                  idx += 1
                case _ => evaluate(objEnv)(t))
            obj
          case _ => throw TastyEvaluationError("don't know how to init this")
      }
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

def evaluateLambda(env: ScalaEnvironment)(tree: Lambda)(using c: Context): ScalaFunctionObject =
  val method = evaluate(env)(tree.meth).asInstanceOf[ScalaMethod]
  ScalaFunctionObject(ScalaEnvironment(Some(env)), method)

def evaluateIdent(env: ScalaEnvironment)(tree: Ident)(using Context): ScalaTerm =
  env.lookup(tree.tpe.asInstanceOf[TermRef].symbol)
