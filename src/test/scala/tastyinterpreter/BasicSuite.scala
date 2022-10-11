package tastyinterpreter

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

import munit.FunSuite

import tastyquery.Contexts.Context
import tastyquery.Contexts
import tastyquery.nodejs.ClasspathLoaders
import tastyquery.Names.*
import tastyquery.Trees.*
import tastyquery.Spans.NoSpan
import tastyquery.Types.NoType

class BasicSuite extends FunSuite:

  val classpaths = List(
    "src/test/resources/testinputs/target/scala-3.1.3/classes/",
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
      packageName: FullyQualifiedName)(using ctx: Context) =
    mapUnderTry(
      ctx.findPackageFromRoot(packageName).asPackage.declarations
        .map(_.tree.get.asInstanceOf[Tree]),
      evaluate(environment))

  def packageName(names: String*) = FullyQualifiedName(names.map(termName).toList)

  def evaluateAndCheck(environment: ScalaEnvironment)(tree: Tree, check: ScalaTerm => Unit)
      (using Context, munit.Location) =
    evaluate(environment)(tree).map(check)
      .recover { case (e: TastyEvaluationError) => fail(e.toString()) }

  test("classes and objects") {
    ctx().map { ctx =>
      given Context = ctx
      val globalEnv = globalEnvironment()
      evaluateDeclarationsInPackage(globalEnv, packageName("testinputs")).flatMap(_ =>
        mapUnderTry(List(
          (Apply(
              Select(
                TermRefTree(termName("Foo"), NoType)(NoSpan),
                termName("doit"))(NoSpan),
              List.empty)(NoSpan),
            (result: ScalaTerm) => assert(clue(result.asInstanceOf[ScalaInt].value) == 6))),
          evaluateAndCheck(globalEnv)))
        .get
    }
  }

