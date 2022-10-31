package tastyinterpreter

import tastyquery.Types.*
import tastyquery.Contexts.*

object TypeEvaluators:

  def evaluate(env: ScalaEnvironment)(tpe: Type)(using Context): ScalaEntity =
    tpe match
      case (t: TermRef) => evaluatePrefix(env)(t.prefix).lookup(t.symbol)
      case (t: TypeRef) => evaluatePrefix(env)(t.prefix).lookup(t.symbol)

  def evaluatePrefix(env: ScalaEnvironment)(tpe: Type)(using Context): ScalaEnvironment =
    tpe match
      case (t: TypeRef) => evaluateTypeRef(env)(t)
      case (t: TermRef) => evaluateTermRef(env)(t)
      case NoPrefix => env
      case (_: PackageRef) => env
      case (t: ThisType) => env

  def evaluateTypeRef(env: ScalaEnvironment)(tpe: TypeRef)(using Context): ScalaEnvironment =
    evaluatePrefix(env)(tpe.prefix).lookup(tpe.symbol).value match
      case (cls: ScalaClass) => cls.environment

  def evaluateTermRef(env: ScalaEnvironment)(tpe: TermRef)(using Context): ScalaEnvironment =
    evaluatePrefix(env)(tpe.prefix).lookup(tpe.symbol).value match
      case (obj: ScalaObject) => obj.environment
      case (obj: ScalaLazyValue) =>
        obj.value.asInstanceOf[ScalaObject].environment
      case _ => throw TastyEvaluationError("can't evaluate TermRef")
