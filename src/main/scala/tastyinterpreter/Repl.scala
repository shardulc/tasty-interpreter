package tastyinterpreter

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.control.NonFatal

import tastyquery.Names.*
import tastyquery.Symbols.*
import tastyquery.Trees.*
import tastyquery.Types.*
import tastyquery.Contexts.Context
import tastyquery.nodejs.ClasspathLoaders
import tastyquery.Contexts
import tastyquery.Names
import scala.collection.mutable.HashMap
import tastyquery.TypeTrees.EmptyTypeTree
import tastyquery.Spans


object Repl:
  val classpaths = List(
    "/home/shardulc/Documents/school/EPFL/LAMP/tasty-interpreter/src/test/resources/testinputs/target/scala-3.1.3/classes/",
  )

  def main(args: Array[String]): Unit =
    println("repl not implemented yet!")
    initializeContext()

  def initializeContext(): Future[Context] =
    ClasspathLoaders.read(classpaths).map(Contexts.init(_))

/*
match {
      case PackageDef(pid, stats)                   => stats
      case ImportSelector(imported, renamed, bound) => imported :: renamed :: Nil
      case Import(expr, selectors)                  => expr :: selectors
      case Export(expr, selectors)                  => expr :: selectors
      case ClassDef(name, rhs, symbol)              => rhs :: Nil
      case Template(constr, parents, self, body) =>
        (constr :: parents.collect { case p if p.isInstanceOf[Tree] => p.asInstanceOf[Tree] }) ++ (self :: body)
      case ValDef(name, tpt, rhs, symbol)         => rhs :: Nil
      case DefDef(name, params, tpt, rhs, symbol) => params.flatMap(_.merge) :+ rhs
      case Select(qualifier, name)                => qualifier :: Nil
      case Super(qual, mix)                       => qual :: Nil
      case Apply(fun, args)                       => fun :: args
      case TypeApply(fun, args)                   => fun :: Nil
      case Typed(expr, tpt)                       => expr :: Nil
      case Assign(lhs, rhs)                       => lhs :: rhs :: Nil
      case NamedArg(name, arg)                    => arg :: Nil
      case Block(stats, expr)                     => stats :+ expr
      case If(cond, thenPart, elsePart)           => cond :: thenPart :: elsePart :: Nil
      case Lambda(meth, tpt)                      => meth :: Nil
      case Match(selector, cases)                 => selector :: cases
      case CaseDef(pattern, guard, body)          => pattern :: guard :: body :: Nil
      case Bind(name, body, symbol)               => body :: Nil
      case Alternative(trees)                     => trees
      case Unapply(fun, implicits, patterns)      => fun :: implicits ++ patterns
      case SeqLiteral(elems, elemtpt)             => elems
      case While(cond, body)                      => cond :: body :: Nil
      case Throw(expr)                            => expr :: Nil
      case Try(expr, cases, finalizer)            => (expr :: cases) :+ finalizer
      case Return(expr, from)                     => expr :: from :: Nil
      case Inlined(expr, caller, bindings)        => expr :: bindings

      case _: TypeMember | _: TypeParam | _: Ident | _: ReferencedPackage | _: This | _: New | _: Literal | _: SelfDef |
          EmptyTree =>
        Nil
    }
*/
