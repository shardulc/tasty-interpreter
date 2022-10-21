package tastyinterpreter

import tastyquery.Types.*
import tastyquery.Contexts.*

object TypeEvaluators:

  def evaluate(env: ScalaEnvironment)(tpe: Type)(using Context): ScalaBox[ScalaEntity] =
    tpe match
      case (t: TypeRef) => evaluateTypeRef(env)(t)
      case (t: TermRef) => evaluateTermRef(env)(t)

  def evaluateTypeRef(env: ScalaEnvironment)(tpe: TypeRef)(using Context): ScalaType =
    {tpe.prefix match
       case NoPrefix | (_: PackageRef) => env
       case _ =>
         evaluate(env)(tpe.prefix).value match
           case (cls: ScalaClass) => cls.environment
           case (obj: ScalaObject) => obj.environment
    }.lookup(tpe.symbol)

  def evaluateTermRef(env: ScalaEnvironment)(tpe: TermRef)(using Context): ScalaEntity =
    {tpe.prefix match
       case NoPrefix | (_: PackageRef) => env
       case _ =>
         evaluate(env)(tpe.prefix).value match
           case (cls: ScalaClass) => cls.environment
           case (obj: ScalaObject) => obj.environment
    }.lookup(tpe.symbol)
