package tastyinterpreter

import scala.annotation.targetName
import scala.collection.mutable

import tastyquery.Types.Type
import tastyquery.Contexts.Context
import tastyquery.Names.*
import tastyquery.Symbols.*
import tastyquery.Trees.*


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
-- sealed ScalaType
   -- ScalaClass

*/


sealed trait ScalaEntity
sealed trait ScalaTerm extends ScalaEntity
sealed trait ScalaValue extends ScalaTerm
sealed trait ScalaType extends ScalaEntity

class ScalaEnvironment(
    parent: Option[ScalaEnvironment],
    termBindings: mutable.HashMap[TermSymbol, ScalaTerm] = mutable.HashMap.empty,
    termNameBindings: mutable.HashMap[TermName, ScalaTerm] = mutable.HashMap.empty,
    typeBindings: mutable.HashMap[TypeSymbol, ScalaType] = mutable.HashMap.empty,
    typeNameBindings: mutable.HashMap[TypeName, ScalaType] = mutable.HashMap.empty):

  // def apply(symbol: TermSymbol): ScalaTerm = termBindings(symbol)
  // def apply(symbol: TypeSymbol): ScalaType = typeBindings(symbol)

  def update(symbol: TermSymbol, value: ScalaTerm): Unit =
    termBindings.update(symbol, value)
  def update(symbol: TypeSymbol, value: ScalaType): Unit =
    typeBindings.update(symbol, value)
  def updateByName(symbol: TermName, value: ScalaTerm): Unit =
    termNameBindings.update(symbol, value)
  def updateByName(symbol: TypeName, value: ScalaType): Unit =
    typeNameBindings.update(symbol, value)

  def bindAll(symbolsAndValues: IterableOnce[(TermSymbol, ScalaTerm)]): Unit =
    termBindings.addAll(symbolsAndValues)
  @targetName("bindAllTypes")
  def bindAll(symbolsAndValues: IterableOnce[(TypeSymbol, ScalaType)]): Unit =
    typeBindings.addAll(symbolsAndValues)

  def lookup(symbol: TermSymbol): ScalaTerm =
    termBindings.getOrElse(symbol,
      termNameBindings.getOrElse(symbol.name, parent.get.lookup(symbol)))
  def lookup(symbol: TypeSymbol): ScalaType =
    typeBindings.getOrElse(symbol,
      typeNameBindings.getOrElse(symbol.name, parent.get.lookup(symbol)))
  def lookupByName(name: TermName)(using Context): ScalaTerm =
    val nameBindings = termBindings.map((sym, v) => (sym.name, v)) ++ termNameBindings
    nameBindings.getOrElse(name, parent.fold(throw TastyEvaluationError(name.toString))(_.lookupByName(name)))

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
class ScalaLazyValue[T <: ScalaValue](environment: ScalaEnvironment, toBeValue: Block)(using Context) extends ScalaValue:
  lazy val value = evaluateBlock(environment)(toBeValue).asInstanceOf[T]
class ScalaLazyObject(environment: ScalaEnvironment, toBeValue: Block)(using Context)
    extends ScalaLazyValue[ScalaObject](environment, toBeValue)

class ScalaFunctionObject(override val environment: ScalaEnvironment, method: ScalaMethod)
    extends ScalaObject(environment):
  environment.updateByName(termName("apply"), BuiltInMethod { (arguments, ctx) =>
    method.apply(arguments)(using ctx)
  })

trait ScalaValueExtractor[T](val value: T)

/*
 * This is a mockup of the built-in Int type. Eventually, we want to pass
 * the Scala library through the interpreter and use its definitions (in our
 * target language) rather than the metalanguage definitions here.
 */
class ScalaInt(override val value: Int) extends ScalaObject(ScalaEnvironment(None))
    with ScalaValueExtractor(value):
  environment.updateByName(termName("+"), BuiltInMethod { (arguments, ctx) =>
    arguments match
      case (a: ScalaInt) :: Nil => ScalaInt(a.value + this.value)
      case _ => throw TastyEvaluationError("wrong args for Int +")
  })

object ScalaUnit extends ScalaObject(ScalaEnvironment(None))
type ScalaUnit = ScalaUnit.type

sealed trait ScalaApplicable extends ScalaTerm:
  def apply(arguments: List[ScalaTerm])
    (using Context): ScalaValue 

class ScalaMethod(
    parent: ScalaEnvironment,
    parameters: List[TermSymbol],
    // returnType: ScalaType,
    body: Tree,
    isConstructor: Boolean = false) extends ScalaApplicable:
  override def apply(arguments: List[ScalaTerm])
      (using Context): ScalaValue =
    val callEnvironment =
      if isConstructor then parent
      else ScalaEnvironment(Some(parent), mutable.HashMap.empty)
    callEnvironment.bindAll(parameters.zip(arguments))
    evaluate(callEnvironment)(body) match
      case (result: ScalaApplicable) => result.apply(List.empty)
      case (result: ScalaValue) => result

      

class BuiltInMethod[T <: ScalaValue]
    (underlying: Function2[List[ScalaTerm], Context, T]) extends ScalaApplicable:
  override def apply(arguments: List[ScalaTerm])
      (using ctx: Context): T =
    underlying(arguments, ctx)
