package tastyinterpreter

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

import munit.FunSuite
import munit.Location

import tastyquery.Classpaths.Classpath
import tastyquery.Contexts.Context
import tastyquery.Contexts
import tastyquery.nodejs.ClasspathLoaders
import tastyquery.Names.*
import tastyquery.Trees.*
import tastyquery.Spans.NoSpan
import tastyquery.Types.*

import tastyinterpreter.generated.TestClasspaths

class TastyInterpreterSuite extends FunSuite:

  val classpaths = TestClasspaths.classpaths ++ List(
    "target/scala-3.1.3/test-classes/"
  )

  val clspth = new Fixture[Future[Classpath]]("classpath") {
    private var clspth: Future[Classpath] = null
    def apply() = clspth
    override def beforeAll(): Unit =
      clspth = ClasspathLoaders.read(classpaths)
  }

  override def munitFixtures = List(clspth)


  def testWithInterpreter(testName: String)(testBody: Interpreter => Context ?=> Any) =
    test(testName) { clspth().map { clspth =>
      val ctx = Contexts.init(clspth)
      testBody(Interpreter(using ctx))(using ctx) } }

  def assertInterpretedEquals[T](result: ScalaTerm, expectedScala: T, expectedManual: T)(using Location) =
    assert(clue(result.asInstanceOf[ScalaValueExtractor[T]].value) == clue(expectedScala))
    assert(clue(result.asInstanceOf[ScalaValueExtractor[T]].value) == clue(expectedManual))

  def makePackageName(names: String*) = FullyQualifiedName(names.map(termName).toList)

  def makeSelectTree(packageName: FullyQualifiedName, objectName: String, methodName: String)
      (using c: Context) =
    val objectSymbol = c.findPackageFromRoot(packageName)
      .getDecl(termName(objectName))
      .get
      .asTerm
    val pkg = c.findPackageFromRoot(packageName)
    Select(
      Ident(SimpleName(objectName))(objectSymbol.staticRef)(NoSpan),
      SimpleName(methodName))(None)(NoSpan)
