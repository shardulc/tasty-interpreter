package tastyinterpreter

import scala.annotation.targetName
import scala.collection.mutable

import tastyquery.Contexts.*
import tastyquery.Names.*
import tastyquery.Symbols.*
import tastyquery.Trees.*
import tastyquery.Signatures.*


/*
interpreted entity hierarchy
----------------------------

sealed ScalaEntity
-- sealed ScalaTerm
   -- sealed ScalaValue
      -- ScalaObject
         -- ScalaFunctionObject
         -- ScalaInt (with ScalaValueExtractor)
         -- ScalaUnit
   -- sealed ScalaApplicable
      -- ScalaMethod
      -- BuiltInMethod
   -- ScalaLazyValue
-- sealed ScalaType
   -- ScalaClass

*/


sealed trait ScalaEntity
sealed trait ScalaTerm extends ScalaEntity
sealed trait ScalaValue extends ScalaTerm
sealed trait ScalaType extends ScalaEntity

class ScalaEnvironment(
    parent: Option[ScalaEnvironment],
    thisObj: => Option[ScalaObject] = None,
    termBindings: mutable.HashMap[TermSymbol, ScalaTerm] = mutable.HashMap.empty,
    typeBindings: mutable.HashMap[TypeSymbol, ScalaType] = mutable.HashMap.empty):

  private lazy val thisObject = thisObj
  def getThisObject: ScalaObject = thisObj.getOrElse(parent.get.getThisObject)

  def update(symbol: TermSymbol, value: ScalaTerm): Unit =
    termBindings.update(symbol, value)
  def update(symbol: TypeSymbol, value: ScalaType): Unit =
    typeBindings.update(symbol, value)

  def bindAll(symbolsAndValues: IterableOnce[(TermSymbol, ScalaTerm)]): Unit =
    termBindings.addAll(symbolsAndValues)
  @targetName("bindAllTypes")
  def bindAll(symbolsAndValues: IterableOnce[(TypeSymbol, ScalaType)]): Unit =
    typeBindings.addAll(symbolsAndValues)

  def lookup(symbol: TermSymbol): ScalaTerm =
    termBindings.getOrElse(symbol, parent.get.lookup(symbol))
  def lookup(symbol: TypeSymbol): ScalaType =
    typeBindings.getOrElse(symbol, parent.get.lookup(symbol))

  override def toString(): String =
    s"""term bindings: ${termBindings.keysIterator.toList.map(s => s.name.toString() + s.owner.toString + s.flags.toString() )}
type bindings: ${typeBindings.keysIterator.toList.toString()}
parent: ${parent.toString}"""


case class ScalaClass(
    environment: ScalaEnvironment,
    symbol: ClassSymbol,
    constr: DefDef,
    body: List[Tree]) extends ScalaType

class ScalaObject(env: => ScalaEnvironment) extends ScalaValue:
  lazy val environment = env
class ScalaLazyValue(valueDefinition: => ScalaValue)(using Context) extends ScalaTerm:
  lazy val value = valueDefinition

class ScalaFunctionObject(env: => ScalaEnvironment, method: ScalaMethod)(using Context)
    extends ScalaObject(env):
  override lazy val environment: ScalaEnvironment = env
  val applySymbol = defn.Function0Class.getDecl(termName("apply")).get.asTerm
  environment.update(applySymbol, BuiltInMethod { arguments  =>
    method.apply(arguments)(using ctx)
  })

trait ScalaValueExtractor[T](val value: T)

class ScalaInt(override val value: Int)(using Context) extends ScalaObject(ScalaEnvironment(None))
    with ScalaValueExtractor(value):
  val scalaIntName = defn.IntClass.fullName
  val addSymbol = defn.IntClass.getDecl(
    SignedName(termName("+"), Signature(List(ParamSig.Term(scalaIntName)), scalaIntName), termName("+")))
    .get.asTerm
  environment.update(addSymbol, BuiltInMethod { arguments =>
    arguments match
      case (a: ScalaInt) :: Nil => ScalaInt(a.value + this.value)
      case _ => throw TastyEvaluationError("wrong args for Int +")
  })

object ScalaUnit extends ScalaObject(ScalaEnvironment(None))
type ScalaUnit = ScalaUnit.type


sealed trait ScalaApplicable extends ScalaTerm:
  def apply(arguments: List[ScalaTerm])(using Context): ScalaValue 

class ScalaMethod(
    parent: ScalaEnvironment,
    parameters: List[TermSymbol],
    body: Tree,
    isConstructor: Boolean = false) extends ScalaApplicable:
  override def apply(arguments: List[ScalaTerm])(using Context): ScalaValue =
    val callEnvironment =
      if isConstructor then parent
      else ScalaEnvironment(Some(parent))
    callEnvironment.bindAll(parameters.zip(arguments))
    evaluate(callEnvironment)(body) match
      case (result: ScalaApplicable) => result.apply(List.empty)
      case (result: ScalaValue) => result
      case (result: ScalaLazyValue) => result.value

class BuiltInMethod[T <: ScalaValue]
    (underlying: List[ScalaTerm] => Context ?=> T) extends ScalaApplicable:
  override def apply(arguments: List[ScalaTerm])(using Context): T =
    underlying(arguments)
