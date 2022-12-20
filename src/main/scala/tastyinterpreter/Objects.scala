package tastyinterpreter

import scala.annotation.targetName
import scala.collection.mutable
import scala.language.implicitConversions

import tastyquery.Contexts.*
import tastyquery.Names.*
import tastyquery.Symbols.*
import tastyquery.Trees.*
import tastyquery.Signatures.*
import tastyquery.Flags


/*
interpreted entity hierarchy
----------------------------

sealed ScalaEntity
-- sealed ScalaTerm
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

sealed ScalaSpecializable
-- ScalaClassMethod
-- ScalaClassBuiltInMethod

*/


sealed trait ScalaEntity
sealed trait ScalaTerm extends ScalaEntity:
  def forceValue()(using Context): ScalaObject
sealed trait ScalaType extends ScalaEntity

case class ScalaBox[T](var value: T)
class SetOnce[T]:
  private var value: Option[T] = None
  def get = value.get
  def isSet = value.isDefined
  def isUnset = value.isEmpty
  def set(v: T) =
    if isUnset then value = Some(v)
    else throw IllegalStateException("can only set value once")

class ScalaEnvironment(
    val parent: Option[ScalaEnvironment],
    termBindings: mutable.HashMap[TermSymbol, ScalaBox[ScalaTerm]] = mutable.HashMap.empty,
    typeBindings: mutable.HashMap[TypeSymbol, ScalaBox[ScalaType]] = mutable.HashMap.empty,
    thisObj: => Option[ScalaObject] = None):

  lazy val thisObject: Option[ScalaObject] =
    thisObj.orElse(parent.flatMap(_.thisObject))

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

  override def toString(): String =
    s"""term bindings: ${termBindings.keysIterator.toList.map(s => s.name.toString() + s.owner.toString + s.flags.toString() )}
type bindings: ${typeBindings.keysIterator.toList.toString()}
parent: ${parent.toString}"""


class ScalaClass(
    val environment: ScalaEnvironment,
    val symbol: ClassSymbol,
    val constructor: ScalaClassBuiltInMethod)
  extends ScalaType

class ScalaObject(env: => ScalaEnvironment,
                  val linearization: List[ClassSymbol]) extends ScalaTerm:
  lazy val environment = env
  val superObject = SetOnce[ScalaObject]()

  def resolve(symbol: TermSymbol)(using Context): ScalaBox[ScalaTerm] =
    environment.lookup(
      linearization
        .collectFirst{ c => symbol.overridingSymbol(c) match { case Some(s) => s } }
        .get.asTerm)

  def resolve(name: TermName)(using Context): ScalaBox[ScalaTerm] =
    // this will only ever be called due to a super-accessor prefixed name
    environment.lookup(
      linearization
        // TODO: the first one is probably not the correct overload resolution
        .map(_.getAllOverloadedDecls(name).headOption)
        .collectFirst { case Some(s) => s }
        .get)

  override def forceValue()(using Context): ScalaObject = this


class ScalaLazyValue(valueDefinition: => ScalaObject)(using Context) extends ScalaTerm:
  lazy val value: ScalaObject = valueDefinition
  override def forceValue()(using Context): ScalaObject = value

class ScalaFunctionObject(environment: ScalaEnvironment, method: ScalaMethod)(using Context)
    extends ScalaObject(environment, defn.Function0Class.linearization):
  val applySymbol = defn.Function0Class.getAllOverloadedDecls(termName("apply")).head
  environment.update(applySymbol, BuiltInMethod { arguments  =>
    method.apply(arguments)(using ctx)
  })

trait ScalaValueExtractor[T]:
  val value: T

class ScalaBoolean(val value: Boolean)(using Context)
    extends ScalaObject(ScalaEnvironment(None), defn.BooleanClass.linearization)
    with ScalaValueExtractor[Boolean]

class ScalaInt(val value: Int)(using Context)
    extends ScalaObject(ScalaEnvironment(None), defn.IntClass.linearization)
    with ScalaValueExtractor[Int]:
  private val cls = defn.IntClass
  val scalaIntName = cls.fullName
  val addSymbol = cls.getDecl(
    SignedName(termName("+"), Signature(List(ParamSig.Term(scalaIntName)), scalaIntName), termName("+")))
    .get.asTerm
  environment.update(addSymbol, BuiltInMethod { arguments =>
    arguments match
      case (a: ScalaInt) :: Nil => ScalaInt(a.value + this.value)
      case _ => throw TastyEvaluationError("wrong args for Int +")
  })
  val subSymbol = cls.findDecl(
    SignedName(termName("-"), Signature(List(ParamSig.Term(scalaIntName)), scalaIntName)))
    .asTerm
  environment.update(subSymbol, BuiltInMethod { arguments =>
    arguments match
      case (a: ScalaInt) :: Nil => ScalaInt(this.value - a.value)
      case _ => throw TastyEvaluationError("wrong args for Int -")
  })
  val multSymbol = cls.getDecl(
    SignedName(termName("*"), Signature(List(ParamSig.Term(scalaIntName)), scalaIntName), termName("*")))
    .get.asTerm
  environment.update(multSymbol, BuiltInMethod { arguments =>
    arguments match
      case (a: ScalaInt) :: Nil => ScalaInt(a.value * this.value)
      case _ => throw TastyEvaluationError("wrong args for Int *")
  })
  val scalaBooleanName = defn.BooleanClass.fullName
  val ltSymbol = cls.findDecl(
    SignedName(termName("<"), Signature(List(ParamSig.Term(scalaIntName)), scalaBooleanName)))
    .asTerm
  environment.update(ltSymbol, BuiltInMethod { arguments =>
    arguments match
      case (a: ScalaInt) :: Nil => ScalaBoolean(this.value < a.value)
      case _ => throw TastyEvaluationError("wrong args for Int <")
  })
  val gtSymbol = cls.findDecl(
    SignedName(termName(">"), Signature(List(ParamSig.Term(scalaIntName)), scalaBooleanName)))
    .asTerm
  environment.update(gtSymbol, BuiltInMethod { arguments =>
    arguments match
      case (a: ScalaInt) :: Nil => ScalaBoolean(this.value > a.value)
      case _ => throw TastyEvaluationError("wrong args for Int >")
  })

class ScalaString(val value: String)(using Context)
    extends ScalaObject(ScalaEnvironment(None), defn.StringClass.linearization)
    with ScalaValueExtractor[String]:
  private val cls = defn.StringClass
  val scalaStringName = cls.fullName
  val addSymbol = cls.findDecl(
    SignedName(termName("concat"), Signature(List(ParamSig.Term(scalaStringName)), scalaStringName)))
    .asTerm
  environment.update(addSymbol, BuiltInMethod { arguments =>
    arguments match
      case (a: ScalaString) :: Nil => ScalaString(a.value + this.value)
      case _ => throw TastyEvaluationError("wrong args for String +")
  })

case class ScalaUnit()(using Context)
  extends ScalaObject(ScalaEnvironment(None), defn.UnitClass.linearization)

case class ScalaNull()(using Context)
  extends ScalaObject(ScalaEnvironment(None), defn.NullClass.linearization)


sealed trait ScalaApplicable extends ScalaTerm:
  def apply(arguments: List[ScalaTerm])(using Context): ScalaObject
  override def forceValue()(using Context): ScalaObject = apply(List.empty)

class ScalaMethod(
    parent: ScalaEnvironment,
    parameters: List[TermSymbol],
    body: Tree) extends ScalaApplicable:
  override def apply(arguments: List[ScalaTerm])(using Context): ScalaObject =
    val callEnvironment = ScalaEnvironment(Some(parent))
    callEnvironment.bindAll(parameters.zip(arguments))
    Evaluators.evaluate(callEnvironment)(body)

class BuiltInMethod[T <: ScalaObject]
    (underlying: List[ScalaTerm] => Context ?=> T) extends ScalaApplicable:
  override def apply(arguments: List[ScalaTerm])(using Context): T =
    underlying(arguments)


sealed trait ScalaSpecializable:
  def symbol: TermSymbol
  def specialize(obj: ScalaObject): ScalaApplicable

class ScalaClassMethod(
      parameters: List[TermSymbol], body: Tree,
      val symbol: TermSymbol)
    extends ScalaSpecializable:
  def specialize(obj: ScalaObject): ScalaMethod =
    ScalaMethod(obj.environment, parameters, body)

class ScalaClassBuiltInMethod(
      specializer: ScalaObject => BuiltInMethod[ScalaObject],
      val symbol: TermSymbol)
    extends ScalaSpecializable:
  def specialize(obj: ScalaObject) = specializer(obj)
