package tastyinterpreter

import dotty.tools.tasty.TastyFormat.NameTags

import tastyquery.Trees.*
import tastyquery.Types.*
import tastyquery.Contexts.*
import tastyquery.Names.*
import tastyquery.Spans.NoSpan
import tastyquery.Constants.Constant
import tastyquery.Flags
import tastyquery.Types
import tastyquery.Symbols.*
import tastyquery.Signatures.Signature


class TastyEvaluationError(m: String) extends RuntimeException(m)

object Evaluators:

  def evaluate(env: ScalaEnvironment, tree: Tree, constructingParent: Boolean = false)(using Context): ScalaObject =
    val cp = constructingParent
    tree match
      case t: ClassDef => evaluateClassDef(env, t, cp)
      case t: New => evaluateNew(env, t, cp)
      case t: ValDef => evaluateValDef(env, t, cp)
      case t: DefDef => evaluateDefDef(env, t, cp)
      case t: Block => evaluateBlock(env, t, cp)
      case t: Apply => evaluateApply(env, t, cp)
      case t: Lambda => evaluateLambda(env, t, cp)
      case t: (Ident | Select) => evaluateIdentSelect(env, t, cp)
      case Typed(t, _) => evaluate(env, t, cp)
      case Literal(Constant(t: Int)) => ScalaInt(t)
      case Literal(Constant(t: String)) => ScalaString(t)
      case Literal(Constant(t: Boolean)) => ScalaBoolean(t)
      case Literal(Constant(_: Unit)) => ScalaUnit()
      case Literal(Constant(null)) => ScalaNull()
      case t: This => evaluateThis(env, t, cp)
      case t: Assign => evaluateAssign(env, t, cp)
      case t: If => evaluateIf(env, t, cp)
      case t: Super =>
        throw TastyEvaluationError("Super should never be evaluated on its own")
      case t @ _ => throw TastyEvaluationError(s"not implemented for ${t.toString}")


  def evaluateClassDef(env: ScalaEnvironment, tree: ClassDef, constructingParent: Boolean)(using Context): ScalaUnit =

    inline def filterType[U <: Tree](l: Iterable[Tree]): Iterable[U] =
      l.flatMap((_: @unchecked) match { case u: U => List(u); case _ => List.empty })
    val valDecls = filterType[ValDef](tree.rhs.body)
    val classDecls = filterType[ClassDef](tree.rhs.body)

    val defDecls = filterType[DefDef](tree.rhs.body)
      .filter(s => !s.symbol.is(Flags.Abstract) || s.symbol.is(Flags.Artifact))
      .collect ({ (t: DefDef) =>
        t.name match
          case _: SimpleName =>
            // TODO: handle constructor params that are
            // not just a single list of term params
            val valParams: List[ValDef] = t.paramLists match
              case Left(p) :: _ => p
              case _ => List.empty
            Some(ScalaClassMethod(valParams.map(_.symbol), t.rhs.get, t.symbol))
          case _: PrefixedName => None
          //   this happens for superaccessor methods, which we don't bind at all
          //   but instead look up their referent directly while evaluating Select
          case n @ _ =>
            throw TastyEvaluationError(s"can't handle name $n of type ${n.getClass}")
      }.unlift)

    /*
     * An object of class C is represented with a ScalaEnvironment and a ScalaObject
     * with mutual references. The createNewEnvironments field of the object
     * determines, at any point in time, whether a New node evaluated in that
     * object's environment should create new environment-object pairs or not. In an
     * instantiated ScalaObject, this field is always true. During instantiation, it
     * is true during the evaluation of the body of a template, but false during the
     * evaluation of parent constructors, because they should not create new
     * ScalaObjects. The field is false for a fresh, uninstantiated ScalaObject.
     *
     * The constructor of a class/trait C takes an uninstantiated object and
     * instantiates the 'C' aspect of it with the following steps:
     *  1. Fields that access constructor arguments are bound in the object's
     *     environment to the values of those arguments. (They may be needed to
     *     call parent constructors, which happens before other fields are bound.)
     *  2. Other fields are initialized to null/false/zero/equivalent per their
     *     erased type. Lazy vals are initialized but not computed. Both these
     *     initializations are done for fields that are *not overridden* by an
     *     object lower in the linearization.
     *  3. The parent constructors are evaluated in the object's environment.
     *     Specifically, the concrete parent class constructor is evaluated (which
     *     performs these steps recursively), and then the mixins are evaluated
     *     (which do not recursively call any constructors). Finally,
     *     createNewEnvironments is set to true.
     *  4. The class' methods, represented in the interpreter as ScalaSpecializables,
     *     are specialized to this object and bound in its environment.
     *  5. Inner classes and non-overridden fields are initialized. New bindings in
     *     the object's environment are created for inner classes while the bindings
     *     from step 2 are updated for fields.
     *  6. createNewEnvironments is reset to false, unless the constructor being
     *     evaluated was the 'lowest' constructor in the linearization, in which case
     *     the object is instantiated and createNewEnvironments stays true (from #3).
     *     In any case, the object is returned from the constructor.
     */
    val constructor = ScalaClassBuiltInMethod({ uninstObj =>
      BuiltInMethod[ScalaObject]{ arguments =>
        val objEnv = uninstObj.environment

        // 1. bind constructor argument accessors
        val constrParamNames = tree.rhs.constr.paramLists match
          case Left(p) :: _ => p.map(_.name)
          case _ => List.empty
        val constrArgs = Map.from(constrParamNames.zip(arguments))
        valDecls
          .filter(_.symbol.is(Flags.ParamAccessor))
          .foreach { v => objEnv(v.symbol) = constrArgs(v.name) }

        // 2. compute non-overridden fields, and init them to null-like
        val valsToInit = valDecls
          .filterNot(_.symbol.is(Flags.ParamAccessor))
          .filterNot(t =>
            objEnv.thisObject.get.runtimeClass.linearization
              .takeWhile(c => c != tree.symbol)
              .map(c => t.symbol.overridingSymbol(c))
              .exists(_.isDefined))
          .toList

        valsToInit.foreach { v =>
          if v.symbol.is(Flags.Lazy) then
            evaluateValDef(objEnv, v, cp = false)
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

        // 4. specialize methods
        defDecls.foreach{ d =>
          objEnv(d.symbol) = d.specialize(uninstObj) }

        // 3. eval parents in current object's environment
        if !tree.symbol.is(Flags.Trait) then
          val parent = tree.rhs.parents.head.asInstanceOf[Apply]
          if !parent.tpe.isSameType(defn.ObjectType) then
            evaluateApplyParent(objEnv, parent)

          val parentSymbolsToTrees = tree.rhs.parents
            .drop(1)    // drop concrete parent, evaluated above
            .collect {
              case t: Apply if !t.tpe.isSameType(defn.ObjectType) =>
                TypeEvaluators.evaluate(env)(t.tpe) match
                  case sc: ScalaClass => (sc.symbol, t)
              case t: TypeTree =>
                val mixinSymbol = TypeEvaluators.evaluate(env)(t.toType) match
                  case sc: ScalaClass => sc.symbol
                val mixinInitSymbol = mixinSymbol
                  .findDecl(SignedName(
                    termName("<init>"),
                    Signature(List.empty, mixinSymbol.fullName)))
                  .signedName
                  .toTermName
                (mixinSymbol,
                 Apply(Select(New(t)(NoSpan), mixinInitSymbol)
                   (Some(t.toType.asInstanceOf[TypeRef]))
                   (NoSpan), List.empty)(NoSpan))
            }
            .toMap

          tree.symbol.linearization
            .drop(1)                            // drop self
            .takeWhile(_.is(Flags.Trait))       // take until concrete parent
            .reverse                            // evaluation happens in reverse
            .collect(parentSymbolsToTrees)      // reorder parent Applys by linearization
            .foreach(evaluateApplyParent(objEnv, _))

        // 5. bind inner classes and non-overridden fields and evaluate
        //    non-defining statements in the right order
        classDecls.foreach(evaluateClassDef(objEnv, _, false))
        tree.rhs.body.foreach { _ match
          case stat: ValDef =>
            if valsToInit.contains(stat) then evaluateValDef(objEnv, stat, false)
          case _: DefTree => ()
          case stat @ _ =>
            evaluate(objEnv, stat, false)
        }

        // 6. return the now instantiated object
        uninstObj
    }}, tree.rhs.constr.symbol)

    env(tree.symbol) = ScalaClass(env, tree.symbol, constructor)
    ScalaUnit()


  def evaluateNew(env: ScalaEnvironment, tree: New, constructingParent: Boolean)(using Context): ScalaObject =
    TypeEvaluators.evaluate(env)(tree.tpe) match
      case cls: ScalaClass =>
        val obj =
          // see comment in evaluateClassDef for explanation
          if env.thisObject.isEmpty || !constructingParent then
            // lazy vals for mutual reference
            lazy val (newObjEnv: ScalaEnvironment, newObj: ScalaObject) =
              (ScalaEnvironment(Some(cls.environment), thisObj = Some(newObj)),
               ScalaObject(newObjEnv, cls.symbol))
            newObj
          else env.thisObject.get
        obj.environment(cls.constructor.symbol) = cls.constructor.specialize(obj)
        obj

  def evaluateThis(env: ScalaEnvironment, tree: This, cp: Boolean)(using Context): ScalaObject =
    val qualSymbol = TypeEvaluators.evaluate(env)(tree.qualifier.toType) match
      case c: ScalaClass => c.symbol
    def findEnclosingFrame(e: ScalaEnvironment): ScalaObject =
      if e.thisObject.get.runtimeClass.isSubclass(qualSymbol) then
        e.thisObject.get
      // remember: e.parent is the enclosing frame, not the super object!
      else findEnclosingFrame(e.parent.get)
    findEnclosingFrame(env)

  def evaluateValDef(env: ScalaEnvironment, tree: ValDef, cp: Boolean)(using Context): ScalaUnit =
    def evaledRhs = evaluate(env, tree.rhs.get, cp)
    env(tree.symbol) =
      if tree.symbol.is(Flags.Lazy) then ScalaLazyValue(evaledRhs)
      else evaledRhs
    ScalaUnit()

  def evaluateAssign(env: ScalaEnvironment, tree: Assign, cp: Boolean)(using Context): ScalaUnit =
    tree.lhs match
      case lhs: (Ident | Select) =>
        evaluateNonForcedIdentSelect(env, lhs, cp).value = evaluate(env, tree.rhs, cp)
      case lhs @ _ =>
        throw TastyEvaluationError(s"don't know how to assign to ${lhs.getClass}")
    ScalaUnit()

  def evaluateDefDef(env: ScalaEnvironment, tree: DefDef, cp: Boolean)
      (using Context): ScalaUnit =
    if tree.name != SimpleName("writeReplace") then
      // assume only one, val params clause for now
      val valParams: List[ValDef] = tree.paramLists match
        case Left(p) :: _ => p
        case _ => List.empty
      env(tree.symbol) = ScalaMethod(env, valParams.map(_.symbol), tree.rhs.get)
    ScalaUnit()

  def evaluateBlock(env: ScalaEnvironment, tree: Block, cp: Boolean)(using Context): ScalaObject =
    tree.stats.foreach(evaluate(env, _, cp))
    evaluate(env, tree.expr, cp)

  def evaluateApply(env: ScalaEnvironment, tree: Apply, cp: Boolean)(using Context): ScalaObject =
    // ignore possibility of call-by-name
    tree.fun match
      case fun: (Ident | Select) =>
        evaluateNonForcedIdentSelect(env, fun, cp).value match
          case f: ScalaApplicable => f.apply(tree.args.map(evaluate(env, _, cp)), cp)
          case f @ _ => throw TastyEvaluationError(s"can't apply ${f}")
      case fun @ _ =>
        throw TastyEvaluationError(s"don't know how to apply ${fun.getClass}")

  def evaluateApplyParent(env: ScalaEnvironment, tree: Apply)(using Context): ScalaObject =
    // ignore possibility of call-by-name
    tree.fun match
      case fun: (Ident | Select) =>
        evaluateNonForcedIdentSelect(env, fun, true).value match
          case f: ScalaApplicable => f.apply(tree.args.map(evaluate(env, _, false)), true)
          case f @ _ => throw TastyEvaluationError(s"can't apply ${f}")
      case fun @ _ =>
        throw TastyEvaluationError(s"don't know how to apply ${fun.getClass}")

  def evaluateLambda(env: ScalaEnvironment, tree: Lambda, cp: Boolean)
      (using Context): ScalaFunctionObject =
    tree.meth match
      case meth: (Ident | Select) =>
        val method = evaluateNonForcedIdentSelect(env, meth, cp).value match
          case m: ScalaMethod => m
          case m =>
            throw TastyEvaluationError(s"Lambda must refer to method, not ${m.getClass}")
        ScalaFunctionObject(ScalaEnvironment(Some(env)), method)
      case meth @ _ =>
        throw TastyEvaluationError(s"Lambda must refer to method, not ${meth.getClass}")

  def evaluateIf(env: ScalaEnvironment, tree: If, cp: Boolean)(using Context): ScalaObject =
    if evaluate(env, tree.cond, cp).asInstanceOf[ScalaValueExtractor[Boolean]].value then
      evaluate(env, tree.thenPart, cp)
    else
      evaluate(env, tree.elsePart, cp)

  def evaluateIdentSelect(env: ScalaEnvironment, tree: Ident | Select, cp: Boolean)
      (using Context): ScalaObject =
    evaluateNonForcedIdentSelect(env, tree, cp).value.forceValue()

  def evaluateNonForcedIdentSelect(env: ScalaEnvironment, tree: Ident | Select, cp: Boolean)
      (using Context): ScalaBox[ScalaTerm] =
    tree match
      case t: Ident =>
        t.symbol match
          case ts: TermSymbol => env.lookup(ts)
          case ps: PackageSymbol => throw TastyEvaluationError("can't do packages yet")
      case t: Select =>
        t.symbol match
          case ts: TermSymbol =>
            t.qualifier match
              case Super(qual: This, None) => evaluateThis(env, qual, cp)
                .resolveDynamicSuper(ts, from = qual.tpe.asInstanceOf[ThisType].cls)
              case Super(qual: This, Some(mixin)) =>
                val mixinSymbol = TypeEvaluators.evaluate(env)(mixin.toType) match
                  case c: ScalaClass => c.symbol
                evaluateThis(env, qual, cp).resolveStaticSuper(ts, mixinSymbol)
              case qual: This if ts.name.isInstanceOf[PrefixedName] =>
                evaluateThis(env, qual, cp).resolveSuperAccessor(
                  ts.name.asInstanceOf[PrefixedName],
                  qual.tpe.asInstanceOf[ThisType].cls)
              case _ => evaluate(env, t.qualifier, cp).resolve(ts)
          case ps: PackageSymbol => throw TastyEvaluationError("can't do packages yet")
