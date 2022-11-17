package tastyinterpreter

import scala.annotation.targetName
import scala.collection.mutable
import scala.language.implicitConversions

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
         -- ScalaUninitializedObject
   -- sealed ScalaApplicable
      -- ScalaMethod
      -- BuiltInMethod
   -- ScalaClassMethod
   -- ScalaClassConstructor
   -- ScalaLazyValue
-- sealed ScalaType
   -- ScalaClass

*/


sealed trait ScalaEntity
sealed trait ScalaTerm extends ScalaEntity:
  def forceValue()(using Context): ScalaValue
sealed trait ScalaValue extends ScalaTerm:
  override def forceValue()(using Context): ScalaValue = this
sealed trait ScalaType extends ScalaEntity

case class ScalaBox[T](var value: T)

class ScalaEnvironment(
    parent: Option[ScalaEnvironment],
    val thisObjectClass: Option[ClassSymbol] = None,
    termBindings: mutable.HashMap[TermSymbol, ScalaBox[ScalaTerm]] = mutable.HashMap.empty,
    typeBindings: mutable.HashMap[TypeSymbol, ScalaBox[ScalaType]] = mutable.HashMap.empty):

  var thisObject: Option[ScalaObject] = parent.flatMap(_.thisObject)

  def update(symbol: TermSymbol, value: ScalaTerm): Unit =
    termBindings.update(symbol, ScalaBox(value))
  def update(symbol: TypeSymbol, value: ScalaType): Unit =
    typeBindings.update(symbol, ScalaBox(value))

  def bindAll(symbolsAndValues: Iterable[(TermSymbol, ScalaTerm)]): Unit =
    termBindings.addAll(symbolsAndValues.map((s, t) => (s, ScalaBox(t))))
  @targetName("bindAllTypes")
  def bindAll(symbolsAndValues: Iterable[(TypeSymbol, ScalaType)]): Unit =
    typeBindings.addAll(symbolsAndValues.map((s, t) => (s, ScalaBox(t))))

  def lookup(symbol: TermSymbol): ScalaBox[ScalaTerm] =
    termBindings.getOrElse(symbol, parent.get.lookup(symbol))
  def lookup(symbol: TypeSymbol): ScalaBox[ScalaType] =
    typeBindings.getOrElse(symbol, parent.get.lookup(symbol))

  def lookupHere(symbol: TermSymbol): Option[ScalaBox[ScalaTerm]] =
    termBindings.get(symbol)

  override def toString(): String =
    s"""term bindings: ${termBindings.keysIterator.toList.map(s => s.name.toString() + s.owner.toString + s.flags.toString() )}
type bindings: ${typeBindings.keysIterator.toList.toString()}
parent: ${parent.toString}"""


class ScalaClass(
      val environment: ScalaEnvironment,
      val symbol: ClassSymbol,
      val constructor: ScalaClassConstructor)
    extends ScalaType

class ScalaObject(val environment: ScalaEnvironment,
                  val cls: ClassSymbol,
                  val superObj: Option[ScalaObject]) extends ScalaValue
class ScalaUninitializedObject(environment: ScalaEnvironment, cls: ClassSymbol)
  extends ScalaObject(environment, cls, None)

class ScalaLazyValue(valueDefinition: => ScalaValue)(using Context) extends ScalaTerm:
  lazy val value: ScalaValue = valueDefinition
  override def forceValue()(using Context): ScalaValue = value

class ScalaFunctionObject(environment: ScalaEnvironment, method: ScalaMethod)(using Context)
    extends ScalaObject(environment, defn.Function0Class, None):
  val applySymbol = defn.Function0Class.getDecl(termName("apply")).get.asTerm
  environment.update(applySymbol, BuiltInMethod { arguments  =>
    method.apply(arguments)(using ctx)
  })

trait ScalaValueExtractor[T](val value: T)

class ScalaInt(override val value: Int)(using Context)
    extends ScalaObject(ScalaEnvironment(None), defn.IntClass, None)
    with ScalaValueExtractor(value):
  val scalaIntName = cls.fullName
  val addSymbol = cls.getDecl(
    SignedName(termName("+"), Signature(List(ParamSig.Term(scalaIntName)), scalaIntName), termName("+")))
    .get.asTerm
  environment.update(addSymbol, BuiltInMethod { arguments =>
    arguments match
      case (a: ScalaInt) :: Nil => ScalaInt(a.value + this.value)
      case _ => throw TastyEvaluationError("wrong args for Int +")
  })
  val multSymbol = cls.getDecl(
    SignedName(termName("*"), Signature(List(ParamSig.Term(scalaIntName)), scalaIntName), termName("*")))
    .get.asTerm
  environment.update(multSymbol, BuiltInMethod { arguments =>
    arguments match
      case (a: ScalaInt) :: Nil => ScalaInt(a.value * this.value)
      case _ => throw TastyEvaluationError("wrong args for Int *")
  })

case class ScalaUnit()(using Context)
  extends ScalaObject(ScalaEnvironment(None), defn.UnitClass, None)

case class ScalaNull()(using Context)
  extends ScalaObject(ScalaEnvironment(None), defn.NullClass, None)


sealed trait ScalaApplicable extends ScalaTerm:
  def apply(arguments: List[ScalaTerm])(using Context): ScalaValue
  override def forceValue()(using Context): ScalaValue = apply(List.empty)

class ScalaMethod(
    parent: ScalaEnvironment,
    parameters: List[TermSymbol],
    body: Tree) extends ScalaApplicable:
  override def apply(arguments: List[ScalaTerm])(using Context): ScalaValue =
    val callEnvironment = ScalaEnvironment(Some(parent))
    callEnvironment.bindAll(parameters.zip(arguments))
    Evaluators.evaluate(callEnvironment)(body)

class BuiltInMethod[T <: ScalaValue]
    (underlying: List[ScalaTerm] => Context ?=> T) extends ScalaApplicable:
  override def apply(arguments: List[ScalaTerm])(using Context): T =
    underlying(arguments)

class ScalaClassMethod(parameters: List[TermSymbol], body: Tree, val symbol: TermSymbol)
    extends ScalaTerm:
  def specialize(obj: ScalaObject): ScalaMethod =
    ScalaMethod(obj.environment, parameters, body)
  override def forceValue()(using Context): ScalaValue =
    throw TastyEvaluationError("can't force without object")

class ScalaClassConstructor(val symbol: TermSymbol, val specialize:
      ScalaUninitializedObject => BuiltInMethod[ScalaObject])
    extends ScalaTerm:
  override def forceValue()(using Context): ScalaValue =
    throw TastyEvaluationError("can't force without object")
