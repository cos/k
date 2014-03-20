package org.kframework.backend.java.symbolic;

import org.kframework.backend.BasicBackend;
import org.kframework.compile.FlattenModules;
import org.kframework.compile.ResolveConfigurationAbstraction;
import org.kframework.compile.checks.CheckConfigurationCells;
import org.kframework.compile.checks.CheckRewrite;
import org.kframework.compile.checks.CheckVariables;
import org.kframework.compile.sharing.DeclareCellLabels;
import org.kframework.compile.tags.AddDefaultComputational;
import org.kframework.compile.tags.AddOptionalTags;
import org.kframework.compile.tags.AddStrictStar;
import org.kframework.compile.transformers.*;
import org.kframework.compile.utils.*;
import org.kframework.kil.ASTNode;
import org.kframework.kil.Definition;
import org.kframework.kil.loader.AddConsesVisitor;
import org.kframework.kil.loader.CollectConsesVisitor;
import org.kframework.kil.loader.CollectSubsortsVisitor;
import org.kframework.kil.loader.Context;
import org.kframework.kil.visitors.CopyOnWriteTransformer;
import org.kframework.kil.visitors.exceptions.TransformerException;
import org.kframework.main.FirstStep;
import org.kframework.main.LastStep;
import org.kframework.utils.BinaryLoader;
import org.kframework.utils.Stopwatch;

import java.io.File;
import java.io.IOException;


/**
 * Backend for the Java Rewrite Engine.
 *
 * @author AndreiS
 */
public class JavaSymbolicBackend extends BasicBackend {

    private class DefinitionSerializer extends CopyOnWriteTransformer {

        public DefinitionSerializer(Context context) {
            super("Serialize Compiled Definition to XML", context);
        }

        @Override
        public ASTNode transform(Definition node) throws TransformerException {
            BinaryLoader.save(new File(context.dotk, "defx-java.bin").toString(), node);

            return node;
        }
    }

    public static final String DEFINITION_FILENAME = "java_symbolic_definition.bin";

    public JavaSymbolicBackend(Stopwatch sw, Context context) {
        super(sw, context);
    }

    @Override
    public Definition lastStep(Definition javaDef) {
        BinaryLoader.save(new File(context.dotk,
                JavaSymbolicBackend.DEFINITION_FILENAME).toString(),
                new KILtoBackendJavaKILTransformer(context, true)
                        .transformDefinition(javaDef));

        return javaDef;
    }

    @Override
    public void run(Definition def) throws IOException { }

    @Override
    public String getDefaultStep() {
        return "LastStep";
    }

    @Override
    public CompilerSteps<Definition> getCompilationSteps() {
        CompilerSteps<Definition> steps = new CompilerSteps<Definition>(context);
        steps.add(new FirstStep(this, context));

        steps.add(new CheckVisitorStep<Definition>(new CheckConfigurationCells(context), context));
        steps.add(new RemoveBrackets(context));
        // SetVariablesInferredSort must be performed before AddEmptyLists
        steps.add(new SetVariablesInferredSort(context));
        steps.add(new AddEmptyLists(context));
        steps.add(new RemoveSyntacticCasts(context));
        steps.add(new CheckVisitorStep<Definition>(new CheckVariables(context), context));
        steps.add(new CheckVisitorStep<Definition>(new CheckRewrite(context), context));
        steps.add(new FlattenModules(context));

        steps.add(new CompleteSortLatice(context));
        steps.add(new CheckVisitorStep<Definition>(new AddConsesVisitor(context), context));
        steps.add(new CheckVisitorStep<Definition>(new CollectConsesVisitor(context), context));
        steps.add(new CheckVisitorStep<Definition>(new CollectSubsortsVisitor(context), context));
        steps.add(new DefinitionSerializer(context));

        steps.add(new StrictnessToContexts(context));
        steps.add(new FreezeUserFreezers(context));
        steps.add(new ContextsToHeating(context));
        //steps.add(new AddSupercoolDefinition(context));
        steps.add(new AddHeatingConditions(context));
        //steps.add(new AddSuperheatRules(context));
        steps.add(new DesugarStreams(context, true));
        steps.add(new ResolveFunctions(context));
        steps.add(new AddKCell(context));
        steps.add(new AddStreamCells(context));
        //steps.add(new AddSymbolicK(context));
        //steps.add(new AddSemanticEquality(context));
        // steps.add(new ResolveFresh());
        //steps.add(new FreshCondToFreshVar(context));
        //steps.add(new ResolveFreshVarMOS(context));

        steps.add(new AddTopCellConfig(context));
        steps.add(new AddTopCellRules(context));

        steps.add(new ResolveBinder(context));
        steps.add(new ResolveAnonymousVariables(context));
        //steps.add(new AddK2SMTLib(context));
        steps.add(new AddPredicates(context));
        //steps.add(new ResolveSyntaxPredicates(context));
        steps.add(new ResolveBuiltins(context));
        steps.add(new ResolveListOfK(context));
        steps.add(new AddInjections(context));

        steps.add(new FlattenTerms(context));
        steps.add(new FlattenSyntax(context));
        steps.add(new ResolveBlockingInput(context, true));
        steps.add(new InitializeConfigurationStructure(context));
        //steps.add(new AddKStringConversion(context));
        //steps.add(new AddKLabelConstant(context));
        steps.add(new ResolveHybrid(context));
        steps.add(new ResolveConfigurationAbstraction(context));
        steps.add(new ResolveOpenCells(context));
        steps.add(new ResolveRewrite(context));

        /* data structure related stuff */
       // steps.add(new CompileToBuiltins(context));
        steps.add(new CompileDataStructures(context));
        steps.add(new Cell2DataStructure(context));
        steps.add(new DataStructureToLookupUpdate(context));

        //steps.add(new ResolveSupercool(context));
        steps.add(new AddStrictStar(context));
        steps.add(new AddDefaultComputational(context));
        steps.add(new AddOptionalTags(context));
        steps.add(new DeclareCellLabels(context));

        /* remove rules that are from k dist */
        steps.add(new RemovePreincludedRules(context));

        steps.add(new LastStep(this, context));

        return steps;
    }
}
