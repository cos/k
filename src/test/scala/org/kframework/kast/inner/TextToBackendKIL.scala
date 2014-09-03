package org.kframework.kast.inner

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
import kil.Definition
import kil.loader.Context
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

object TextToBackendKIL {
  def parseDefinition(definitionText: String): (kil.Definition, kil.loader.Context, BinaryLoader) = {
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

    val backend = mock(classOf[Backend])
    val kompileOptions = mock(classOf[KompileOptions])
    when(kompileOptions.mainModule()).thenReturn("TEST")
    kompileOptions.global = globalOptions
    kompileOptions.backend = Backends.SYMBOLIC
    kompileOptions.experimental = new Experimental();

    when(backend.autoinclude()).thenReturn(false)
    when(backend.documentation()).thenReturn(false)
    context.kompileOptions = kompileOptions

    val binaryLoader = mock(classOf[BinaryLoader])
    BinaryLoader.loader = binaryLoader

    val definitionLoader = new DefinitionLoader(new Stopwatch(globalOptions), binaryLoader, kem, new OuterParser(kompileOptions, backend), backend)
    (definitionLoader.parseDefinition(file, "TEST", context), context, binaryLoader)
  }

  def toBackendKIL(d: Definition, context: kil.loader.Context, loader: BinaryLoader): backend.java.kil.Definition = {

    val globalContext = new backend.java.kil.GlobalContext(null,
      mock(classOf[BuiltinFunction]), null,
      new KItemOperations(null, null, null))

    val transformer = new KILtoBackendJavaKILTransformer(context, globalContext, true, new IndexingTable.Data())

    val symbolicBackend = new JavaSymbolicBackend(
      mock(classOf[Stopwatch]),
      context,
      loader,
      Providers.of(mock(classOf[backend.java.indexing.IndexingTable])),
      Providers.of(transformer)) {

      override def lastStep(javaDef: Definition): Definition = javaDef
    }

    val prepared = try {
      symbolicBackend.getCompilationSteps().compile(d, symbolicBackend.getDefaultStep())
    } catch {
      case e: CompilerStepDone => e.getResult().asInstanceOf[Definition]
    }

    transformer.transformDefinition(prepared)
  }

  def apply(definitionText: String) = {
    val (definition, context, loader) = parseDefinition(definitionText)
    toBackendKIL(definition, context, loader)
  }
}