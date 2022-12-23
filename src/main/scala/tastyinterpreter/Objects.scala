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
import dotty.tools.tasty.TastyFormat.NameTags


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
                  val runtimeClass: ClassSymbol) extends ScalaTerm:
  lazy val environment = env
  var createNewEnvironments = false

  def resolve(symbol: TermSymbol)
      (using Context): ScalaBox[ScalaTerm] =
    if Seq(
        defn.IntClass,
        defn.StringClass,
        defn.Function0Class,
        defn.BooleanClass,
        defn.UnitClass).contains(runtimeClass) then
      // TODO: actually dispatch these to the standard library
      environment.lookup(symbol)
    else
      environment.lookup(
        symbol.runtimeImplementingSymbol(runtimeClass))

  def resolveDynamicSuper(symbol: TermSymbol, from: ClassSymbol)
      (using Context): ScalaBox[ScalaTerm] =
    environment.lookup(
      symbol.nextSuperSymbol(runtimeClass, from))

  def resolveStaticSuper(symbol: TermSymbol, from: ClassSymbol)
      (using Context): ScalaBox[ScalaTerm] =
    environment.lookup(symbol.runtimeImplementingSymbol(from))

  def resolveSuperAccessor(name: PrefixedName, from: ClassSymbol)
      (using Context): ScalaBox[ScalaTerm] =
    name.underlying match
      case ExpandedName(NameTags.EXPANDPREFIX, prefix, name) =>
        val superClass = runtimeClass.linearization
          .find(_.asType.name.asSimpleName == name).get
        val underlyingSymbol = superClass.appliedRef.member(prefix).asTerm
        resolveStaticSuper(underlyingSymbol, superClass)
      case _ =>
        val underlying = name.underlying match
          case ExpandedName(_, _, name) => name
          case name @ _ => name
        // TODO: this is probably not the right way to get the underlying symbol
        val underlyingSymbol = runtimeClass.linearization
          .dropWhile(_ != from)
          .collectFirst({ (c: ClassSymbol) =>
            c.getAllOverloadedDecls(underlying).headOption }.unlift)
          .get
        resolveDynamicSuper(underlyingSymbol, from)

  override def forceValue()(using Context): ScalaObject = this


class ScalaLazyValue(valueDefinition: => ScalaObject)(using Context) extends ScalaTerm:
  lazy val value: ScalaObject = valueDefinition
  override def forceValue()(using Context): ScalaObject = value

class ScalaFunctionObject(environment: ScalaEnvironment, method: ScalaMethod)(using Context)
    extends ScalaObject(environment, defn.Function0Class):
  val applySymbol = defn.Function0Class.getAllOverloadedDecls(termName("apply")).head
  environment.update(applySymbol, BuiltInMethod { arguments  =>
    method.apply(arguments)(using ctx)
  })

trait ScalaValueExtractor[T]:
  val value: T

class ScalaBoolean(val value: Boolean)(using Context)
    extends ScalaObject(ScalaEnvironment(None), defn.BooleanClass)
    with ScalaValueExtractor[Boolean]

class ScalaInt(val value: Int)(using Context)
    extends ScalaObject(ScalaEnvironment(None), defn.IntClass)
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
    extends ScalaObject(ScalaEnvironment(None), defn.StringClass)
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
  extends ScalaObject(ScalaEnvironment(None), defn.UnitClass)

case class ScalaNull()(using Context)
  extends ScalaObject(ScalaEnvironment(None), defn.NullClass)


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
