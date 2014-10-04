// Copyright (c) 2014 K Team. All Rights Reserved.
package org.kframework.parser.generator;

import org.kframework.backend.Backends;
import org.kframework.compile.transformers.AddSymbolicK;
import org.kframework.kil.Definition;
import org.kframework.kil.DefinitionItem;
import org.kframework.kil.Import;
import org.kframework.kil.Lexical;
import org.kframework.kil.Module;
import org.kframework.kil.ModuleItem;
import org.kframework.kil.NonTerminal;
import org.kframework.kil.Production;
import org.kframework.kil.ProductionItem;
import org.kframework.kil.Restrictions;
import org.kframework.kil.Sort;
import org.kframework.kil.Terminal;
import org.kframework.kil.UserList;
import org.kframework.kil.loader.Context;
import org.kframework.parser.concrete2.Grammar;
import org.kframework.parser.concrete2.KSyntax2GrammarStatesFilter;
import org.kframework.utils.BinaryLoader;
import org.kframework.utils.StringUtil;

import java.io.File;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Collect the syntax module, call the syntax collector and print SDF for programs.
 */
public class ParsersPerModule {

    /**
     * Generates and saves to disk a Map of moduleName -> programParser for the entire definition
     * given as argument.
     * @param def        The definition for which to generate all the program parsers.
     * @param context    The Context object, with all the helper methods.
     */
    public static void generateParsersForModules(Definition def, Context context) {
        Map<String, Grammar> parsers = new HashMap<>();
        for (DefinitionItem di : def.getItems()) {
            if (di instanceof Module) {
                Set<String> imported = getIncludedModules(((Module) di).getName(), def);

                // collect the syntax from those modules
                CollectTerminalsVisitor ctv = new CollectTerminalsVisitor(context);
                // visit all modules to collect all Terminals first
                for (String modName : imported)
                    ctv.visitNode(def.getModulesMap().get(modName));
                KSyntax2GrammarStatesFilter ks2gsf = new KSyntax2GrammarStatesFilter(context, ctv);
                for (String modName : imported)
                    ks2gsf.visitNode(def.getModulesMap().get(modName));
                parsers.put(((Module) di).getName(), ks2gsf.getGrammar());
            }
        }

        // save the new parser info
        new File(context.kompiled, "pgm").mkdirs();
        BinaryLoader.instance().saveOrDie(context.kompiled.getPath()+ "/pgm/newModuleParsers.bin", parsers);
    }

    /**
     * Recursively go through all the imports and find the list of all included modules.
     * @param modName    Name of the root module.
     * @param def        The Definition object in which to search for all the modules.
     * @return A set of all module names included by the given module name.
     */
    private static Set<String> getIncludedModules(String modName, Definition def) {
        Set<String> visited = new HashSet<>();
        getIncludedModules2(modName, def, visited);
        return visited;
    }

    private static void getIncludedModules2(String modName, Definition def, Set<String> visited) {
        if (visited.contains(modName))
            return;
        Module m = def.getModulesMap().get(modName);
        // there are some mockup modules that don't really exist, like #BOOL-INTERFACE
        // from the old maude days
        if (m == null)
            return;
        visited.add(modName);
        for (ModuleItem mi : m.getItems()) {
            if (mi instanceof Import) {
                getIncludedModules2(((Import) mi).getName(), def, visited);
            }
        }
    }
}
