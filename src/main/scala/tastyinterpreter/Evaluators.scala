package tastyinterpreter

import scala.collection.mutable.HashMap

import tastyquery.Trees.*
import tastyquery.Types.*
import tastyquery.Contexts.*
import tastyquery.Names.*
import tastyquery.Spans.NoSpan
import tastyquery.Constants.Constant
import tastyquery.Flags
import tastyquery.Types
import tastyquery.Symbols.*


class TastyEvaluationError(m: String) extends RuntimeException(m)

object Evaluators:

  def evaluate(env: ScalaEnvironment)(tree: Tree)(using Context): ScalaValue =
    tree match
      case t: ClassDef => evaluateClassDef(env)(t)
      case t: New => evaluateNew(env)(t)
      case t: ValDef => evaluateValDef(env)(t)
      case t: DefDef => evaluateDefDef(env)(t)
      case t: Block => evaluateBlock(env)(t)
      case t: Apply => evaluateApply(env)(t)
      case t: Select => evaluateSelect(env)(t)
      case t: Lambda => evaluateLambda(env)(t)
      case t: Ident => evaluateIdent(env)(t)
      case Typed(t, _) => evaluate(env)(t)
      case EmptyTree => ScalaUnit()
      case Literal(Constant(t: Int)) => ScalaInt(t)
      case Literal(Constant(_: Unit)) => ScalaUnit()
      case t: This => evaluateThis(env)(t)
      case t: Assign => evaluateAssign(env)(t)
      case t @ _ => throw TastyEvaluationError(s"not implemented for ${t.toString}")

  def evaluateClassDef(env: ScalaEnvironment)(tree: ClassDef)(using Context): ScalaUnit =
    // 1. class with parent = symbol.linearization, env child of current
    // val parent = tree.symbol.linearization.headOption
    //   .map(sym => env.lookup(sym).value match
    //   	case c: ScalaClass => c.environment)
    //   .getOrElse(env)
    // val parent =
    //   if tree.symbol.linearization(1) == defn.ObjectClass then
    //     env
    //   else env.lookup(tree.symbol.linearization(1)).value match
    //   	case c: ScalaClass => c.environment
    val parent = env
    val clsEnv = ScalaEnvironment(Some(parent))

    inline def filterType[U <: Tree](l: Iterable[Tree]): Iterable[U] =
      l.flatMap(_ match { case u: U => List(u); case _ => List.empty })
    val valDecls = filterType[ValDef](tree.rhs.body)
    val classDecls = filterType[ClassDef](tree.rhs.body)

    // 2. DefDefs to be specialized per object
    val defDecls = filterType[DefDef](tree.rhs.body)
      .map { t =>
        val valParams: List[ValDef] = t.paramLists match
          case Left(p) :: _ => p
          case _ => List.empty
        ScalaClassMethod(valParams.map(_.symbol), t.rhs, t.symbol)
      }

    // 2. bind <init> to do (whiteboard)
    val constructorSymbol = tree.rhs.constr.symbol
    val constructor = ScalaClassConstructor(constructorSymbol, { uninitObj =>
      val objEnv = uninitObj.environment
      BuiltInMethod[ScalaObject]{ arguments =>
        val constrParamNames = tree.rhs.constr.paramLists match
          case Left(p) :: _ => p.map(_.name)
          case _ => List.empty
        val constrArgs = Map.from(constrParamNames.zip(arguments))
        valDecls
          .filter(_.symbol.is(Flags.ParamAccessor))
          .foreach { v => objEnv(v.symbol) = constrArgs(v.name) }

        // 1. vals to initialize = 
        //      (vals declared in this template)
        //      without (vals already defined in this env)
        val valsToInit = valDecls
          .filterNot(_.symbol.is(Flags.ParamAccessor))
          .filterNot(t =>
            objEnv.thisObject.get.cls.linearization
              .takeWhile(c => c != tree.symbol)
              .map(c => t.symbol.overridingSymbol(c))
              .exists(_.isDefined))
        // println(s"me: ${tree.symbol}\nlinearization: ${objEnv.thisObject.get.cls.linearization}\nmy vals: ${valsToInit.map(_.symbol)}\n\n")

        // 2. set those to null-like value
        valsToInit.foreach { v =>
          if v.symbol.is(Flags.Lazy) then
            evaluateValDef(objEnv)(v)
          else
            val vType = v.symbol.declaredType
            val vErasedType = ErasedTypeRef.erase(vType)
            objEnv(v.symbol) = vErasedType match
              case ErasedTypeRef.ClassRef(cls) =>
                if cls == defn.IntClass then ScalaInt(0)
                else if cls == defn.UnitClass then ScalaUnit()
                else ScalaNull()
              case ErasedTypeRef.ArrayTypeRef(_, _) => ScalaNull()
        }

        // 3. eval parent in current object's environment
        // evaluate(objEnv)(tree.rhs.parents.head.asInstanceOf[Apply])
        val parent = tree.rhs.parents.head.asInstanceOf[Apply]
        // println(s"i'm ${tree.symbol} initing parent $parent")
        uninitObj.superObj =
          if !parent.tpe.isSameType(defn.ObjectType) then
            evaluate(objEnv)(parent) match { case o: ScalaObject => Some(o) }
          else Some(ScalaUnit())
        // this is now an 'initialized' object
        val obj = uninitObj

        // 4. specialize defs for object
        defDecls.foreach{ d =>
          // println(s"evaling def: ${d.symbol}")
          objEnv(d.symbol) = d.specialize(obj) }

        classDecls.foreach(evaluateClassDef(objEnv))

        // 5. init vals
        valsToInit.foreach(evaluateValDef(objEnv))

        // 6. make this a proper obj and return it
        obj
    }})
    clsEnv(constructorSymbol) = constructor
    env(tree.symbol) = ScalaClass(clsEnv, tree.symbol, constructor)

    ScalaUnit()

  def evaluateNew(env: ScalaEnvironment)(tree: New)(using Context): ScalaObject =
    // 1. check env.This is an uninitialized object:
    //    1. if yes, it
    //    2. if no, make one with self-ref This
    TypeEvaluators.evaluate(env)(tree.tpe) match
      case cls: ScalaClass =>
        val obj =
          // if thisObject exists:
          //   if its superObj exists:
          //     fully defined previous obj, should create new env
          //   else:
          //     we are in the middle of initing, use thisObject.env
          // else:
          //   fully defined previous obj, should create new env
          if env.thisObject.isEmpty || env.thisObject.get.superObj.isDefined then
            val newObjEnv = ScalaEnvironment(Some(cls.environment))
            val newObj = ScalaObject(newObjEnv, cls.symbol, None)
            newObjEnv.thisObject = Some(newObj)
            newObj
          else ScalaObject(env.thisObject.get.environment, cls.symbol, None)
        obj.environment(cls.constructor.symbol) = cls.constructor.specialize(obj)
        obj
      case _ => throw TastyEvaluationError("don't know how to init this")

  def evaluateThis(env: ScalaEnvironment)(t: This)(using Context): ScalaObject =
    t.qualifier match
      case None => env.thisObject.get
      case Some(qualifier) =>
        val qualSymbol = TypeEvaluators.evaluate(env)(qualifier.toType) match
          case c: ScalaClass => c.symbol
          case _ => throw TastyEvaluationError("This() qualifier is not a class")
        println(qualifier)
        println(qualSymbol)
        def tryThis(e: ScalaEnvironment): ScalaObject =
          if e.thisObject.get.cls.isSubclass(qualSymbol) then
            e.thisObject.get
          else tryThis(e.parent.get.parent.get)
        tryThis(env)

  def evaluateValDef(env: ScalaEnvironment)(tree: ValDef)(using Context): ScalaUnit =
    def evaledRhs = evaluate(env)(tree.rhs)
    env(tree.symbol) =
      if tree.symbol.is(Flags.Lazy) then ScalaLazyValue(evaledRhs)
      else evaledRhs
    ScalaUnit()

  def evaluateAssign(env: ScalaEnvironment)(tree: Assign)(using Context): ScalaUnit =
    evaluateNonForcedIdentSelect(env)(tree.lhs).value = evaluate(env)(tree.rhs)
    ScalaUnit()

  def evaluateDefDef(env: ScalaEnvironment)(tree: DefDef)
      (using Context): ScalaUnit =
    if tree.name != SimpleName("writeReplace") then
      // assume only one, val params clause for now
      val valParams: List[ValDef] = tree.paramLists match
        case Left(p) :: _ => p
        case _ => List.empty
      env(tree.symbol) = ScalaMethod(env, valParams.map(_.symbol), tree.rhs)
    ScalaUnit()

  def evaluateBlock(env: ScalaEnvironment)(tree: Block)(using Context): ScalaValue =
    tree.stats.foreach(evaluate(env))
    evaluate(env)(tree.expr)

  def evaluateApply(env: ScalaEnvironment)(tree: Apply)(using Context): ScalaValue =
    // ignore possibility of call-by-name
    evaluateNonForcedIdentSelect(env)(tree.fun).value match
      case f: ScalaApplicable => f.apply(tree.args.map(evaluate(env)))
      case f @ _ => throw TastyEvaluationError(s"can't apply ${f}")

  def evaluateLambda(env: ScalaEnvironment)(tree: Lambda)(using c: Context): ScalaFunctionObject =
    val method = evaluateNonForcedIdentSelect(env)(tree.meth).value.asInstanceOf[ScalaMethod]
    ScalaFunctionObject(ScalaEnvironment(Some(env)), method)

  def evaluateIdent(env: ScalaEnvironment)(tree: Ident)(using Context): ScalaValue =
    evaluateNonForcedIdentSelect(env)(tree).value.forceValue()

  def evaluateSelect(env: ScalaEnvironment)(tree: Select)(using Context): ScalaValue =
    evaluateNonForcedIdentSelect(env)(tree).value.forceValue()

  def evaluateNonForcedIdentSelect(env: ScalaEnvironment)(tree: Tree)(using Context): ScalaBox[ScalaTerm] =
    tree match
      case t: Ident => env.lookup(t.tpe.asInstanceOf[TermRef].symbol)
      case t: Select =>
        evaluate(env)(t.qualifier) match
          case q: ScalaObject =>
            val symbol = tree.tpe.asInstanceOf[TermRef].symbol
            // println(s"$symbol ${symbol.owner} ${symbol.flags} ${symbol.is(Flags.Private)} ${symbol.allOverriddenSymbols.toList}")
            q.resolve(symbol)
      case t @ _ => throw TastyEvaluationError(s"can't non-force $t")
