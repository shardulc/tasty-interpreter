package tastyinterpreter

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

import munit.FunSuite
import munit.Location

import tastyquery.Contexts.Context
import tastyquery.Contexts
import tastyquery.nodejs.ClasspathLoaders
import tastyquery.Names.*
import tastyquery.Trees.*
import tastyquery.Spans.NoSpan
import tastyquery.Types.NoType

class TastyInterpreterSuite extends FunSuite:

  val classpaths = List(
    "target/scala-3.1.3/test-classes/",
  )

  val ctx = new Fixture[Future[Context]]("context") {
    private var ctx: Future[Context] = null
    def apply() = ctx
    override def beforeAll(): Unit =
      ctx = ClasspathLoaders.read(classpaths).map(Contexts.init(_))
  }

  val globalEnvironment = new Fixture[ScalaEnvironment]("global environment") {
    private var env: ScalaEnvironment = null
    def apply() = env
    override def beforeEach(context: BeforeEach): Unit =
      env = ScalaEnvironment(None)
  }

  override def munitFixtures = List(ctx, globalEnvironment)

  def evaluateDeclarationsInPackage(
      environment: ScalaEnvironment = ScalaEnvironment(None),
      packageName: FullyQualifiedName)(using ctx: Context, l: Location) =
    ctx.findPackageFromRoot(packageName).asPackage.declarations
      // asInstanceOf is safe because all DefTree subclasses are Tree subclasses
      .map(_.tree.get.asInstanceOf[Tree])
      .foreach(evaluate(environment))

  def evaluateAndCheck(environment: ScalaEnvironment)(tree: Tree, check: ScalaTerm => Unit)
      (using Context, Location) =
    check(evaluate(environment)(tree))

  def testWithCtx(testName: String)(testBody: Context => Any)
      (using Location)=
    test(testName) { ctx().map { ctx => testBody(ctx) } }

  def assertScalaEquals[T](expected: => T)(result: ScalaTerm) =
    assert(clue(result.asInstanceOf[ScalaValueExtractor[T]].value) == expected)


  def makePackageName(names: String*) = FullyQualifiedName(names.map(termName).toList)

  def makeSelectTree(names: String*) =
    names.tail.foldLeft[Tree](TermRefTree(termName(names.head), NoType)(NoSpan))
      ((tree, name) => Select(tree, termName(name))(NoSpan))
