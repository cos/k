package org.kframework.kast

import org.mockito.Mockito._
import org.kframework._
import utils._
import errorsystem.KExceptionManager
import file.FileUtil
import kompile.KompileOptions
import kompile.KompileOptions.Experimental
import parser.DefinitionLoader
import parser.generator.OuterParser
import main.GlobalOptions
import backend.Backend
import backend.Backends
import backend.java.symbolic.BuiltinFunction
import backend.java.kil.KItem.KItemOperations
import backend.java.symbolic.KILtoBackendJavaKILTransformer
import backend.java.symbolic.JavaSymbolicBackend
import backend.java.indexing.IndexingTable
import compile.utils.CompilerStepDone
import com.google.inject.util.Providers
import java.nio.file.Files
import java.io.File
import collection.JavaConversions._
import org.kframework.backend.java.kil.GlobalContext

/**
 * A class that makes all the necessary wiring to invoke kompile programatically
 */
case class TestingCompiler(definitionText: String, mainModule: String) {

  val kem = new KExceptionManager(new main.GlobalOptions())

  val globalOptions = new GlobalOptions()
  utils.general.GlobalSettings.kem = kem
  val context = new kil.loader.Context()
  context.globalOptions = globalOptions
  OS.current().libDir = "lib/native/macosx"

  val testDir = Files.createTempDirectory("test")

  context.kompiled = new File(testDir.toAbsolutePath().toString() + "/" + "test-kompiled")

  context.dotk = new File(testDir.toAbsolutePath().toString() + "/" + ".k")

  println(OS.current().getNativeExecutable("sdf2table"))

  val file = File.createTempFile("test", ".k")
  FileUtil.save(file.toString(), definitionText)

  val kompileOptions = mock(classOf[KompileOptions])
  when(kompileOptions.mainModule()).thenReturn(mainModule)
  kompileOptions.global = globalOptions
  kompileOptions.backend = Backends.SYMBOLIC
  kompileOptions.experimental = new Experimental();
  kompileOptions.transition = List[String]()
  kompileOptions.supercool = List[String]()
  kompileOptions.superheat = List[String]()

  context.kompileOptions = kompileOptions

  val binaryLoader = mock(classOf[BinaryLoader])
  BinaryLoader.loader = binaryLoader

  lazy val parsedKIL: kil.Definition = {
    val definitionLoader = new DefinitionLoader(new Stopwatch(globalOptions), binaryLoader, kem, new OuterParser(kompileOptions, false, ""), false, false)
    definitionLoader.parseDefinition(file, mainModule, context)
  }

  lazy val globalContext = new backend.java.kil.GlobalContext(null,
    mock(classOf[BuiltinFunction]), null,
    new KItemOperations(null, null, null))

  lazy val kompiledKIL: kil.Definition = {

    val symbolicBackend = new JavaSymbolicBackend(
      mock(classOf[Stopwatch]),
      context,
      binaryLoader,
      Providers.of(mock(classOf[backend.java.indexing.IndexingTable])),
      Providers.of(mock(classOf[KILtoBackendJavaKILTransformer]))) {
      override def lastStep(javaDef: kil.Definition): kil.Definition = javaDef
    }

    try {
      symbolicBackend.getCompilationSteps().compile(parsedKIL, symbolicBackend.getDefaultStep())
    } catch {
      case e: CompilerStepDone => e.getResult().asInstanceOf[kil.Definition]
    }
  }

  lazy val javaBackendKIL: backend.java.kil.Definition = {
    val transformer = new KILtoBackendJavaKILTransformer(context, globalContext, true, new IndexingTable.Data())

    transformer.transformDefinition(kompiledKIL)
  }
}