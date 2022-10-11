package tastyinterpreter

import scala.annotation.targetName
import scala.collection.mutable
import scala.util.{ Try, Success, Failure }

import tastyquery.Types.Type
import tastyquery.Contexts.Context
import tastyquery.Names.*
import tastyquery.Symbols.*
import tastyquery.Trees.*


/*
interpreted entity hierarchy
----------------------------

ScalaEntity
-- ScalaTerm
   -- ScalaValue
      -- ScalaObject
         -- ScalaFunctionObject
         -- ScalaInt
         -- ScalaUnit
   -- ScalaApplicable
      -- ScalaMethod
      -- BuiltInMethod
-- ScalaType
   -- ScalaClass

*/


trait ScalaEntity
sealed trait ScalaTerm extends ScalaEntity
trait ScalaValue extends ScalaTerm
trait ScalaType extends ScalaEntity

class ScalaEnvironment(
    parent: Option[ScalaEnvironment],
    termBindings: mutable.HashMap[TermName, ScalaTerm] = mutable.HashMap.empty,
    typeBindings: mutable.HashMap[TypeName, ScalaType] = mutable.HashMap.empty):

  def apply(name: TermName): ScalaTerm = termBindings(name)
  def apply(name: TypeName): ScalaType = typeBindings(name)

  def update(name: TermName, value: ScalaTerm): Unit =
    termBindings.getOrElseUpdate(name, value)
  def update(name: TypeName, value: ScalaType): Unit =
    typeBindings.getOrElseUpdate(name, value)

  def bindAll(namesAndValues: IterableOnce[(TermName, ScalaTerm)]): Unit =
    termBindings.addAll(namesAndValues)
  @targetName("bindAllTypes")
  def bindAll(namesAndValues: IterableOnce[(TypeName, ScalaType)]): Unit =
    typeBindings.addAll(namesAndValues)

  def lookup(name: TermName): Try[ScalaTerm] =
    termBindings.get(name).map(Success(_))
      .getOrElse(parent.fold(Failure(TastyEvaluationError(s"${name} not found")))
        (_.lookup(name)))
  def lookup(name: TypeName): Try[ScalaType] =
    typeBindings.get(name).map(Success(_))
      .getOrElse(parent.fold(Failure(TastyEvaluationError(s"${name} not found")))
        (_.lookup(name)))

  override def toString(): String =
    s"""term bindings: ${termBindings.keysIterator.toList.toString()}
type bindings: ${typeBindings.keysIterator.toList.toString()}
parent: ${parent.toString}"""


case class ScalaClass(
    environment: ScalaEnvironment,
    symbol: ClassSymbol,
    constr: DefDef,
    body: List[Tree]) extends ScalaType

class ScalaObject(val environment: ScalaEnvironment) extends ScalaValue

case class ScalaFunctionObject(override val environment: ScalaEnvironment, method: ScalaMethod)
    extends ScalaObject(environment):
  environment(termName("apply")) = BuiltInMethod { (env, arguments, ctx) =>
    method.apply(env, arguments)(using ctx)
  }

case class ScalaInt(value: Int) extends ScalaObject(ScalaEnvironment(None)):
  environment(termName("+")) = BuiltInMethod { (env, arguments, ctx) =>
    mapUnderTry(arguments, evaluate(env)(_)(using ctx)).flatMap(_ match
      case (a: ScalaInt) :: Nil => Success(ScalaInt(a.value + this.value))
      case _ => Failure(TastyEvaluationError("wrong args for Int +")))
  }

case object ScalaUnit extends ScalaObject(ScalaEnvironment(None))
type ScalaUnit = ScalaUnit.type

trait ScalaApplicable extends ScalaTerm:
  def apply(callingEnvironment: ScalaEnvironment, arguments: List[Tree])
    (using Context): Try[ScalaValue] 

case class ScalaMethod(
    parent: ScalaEnvironment,
    parameters: List[TermName],
    // returnType: ScalaType,
    body: Tree,
    isConstructor: Boolean = false) extends ScalaApplicable:
  override def apply(callingEnvironment: ScalaEnvironment, arguments: List[Tree])
      (using Context): Try[ScalaValue] =
    val callEnvironment =
      if isConstructor then parent
      else ScalaEnvironment(Some(parent), mutable.HashMap.empty)
    mapUnderTry(arguments, evaluate(callingEnvironment))
      .map(args => callEnvironment.bindAll(parameters.zip(args)))
      .flatMap(_ => evaluate(callEnvironment)(body))
      .flatMap { _ match
        case (result: ScalaApplicable) => result.apply(callEnvironment, List.empty)
        case (result: ScalaValue) => Success(result)
      }
      

class BuiltInMethod[T <: ScalaValue]
    (underlying: Function3[ScalaEnvironment, List[Tree], Context, Try[T]]) extends ScalaApplicable:
  override def apply(callingEnvironment: ScalaEnvironment, arguments: List[Tree])
      (using ctx: Context): Try[T] =
    underlying(callingEnvironment, arguments, ctx)
