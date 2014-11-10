package org.kframework.kore;

import static org.junit.Assert.assertEquals;
import static org.kframework.kore.Interface.*;
import static org.kframework.kore.Interface1.*;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.util.HashSet;
import java.util.Set;
import java.util.stream.Collectors;

import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.TestName;
import org.kframework.kil.Attributes;
import org.kframework.kil.Definition;
import org.kframework.kil.Import;
import org.kframework.kil.Module;
import org.kframework.kil.ModuleItem;
import org.kframework.kil.PriorityBlock;
import org.kframework.kil.Require;
import org.kframework.kil.Sort;
import org.kframework.kil.Sources;
import org.kframework.kil.Syntax;
import org.kframework.kore.outer.Sentence;
import org.kframework.kore.outer.SyntaxSort;
import org.kframework.parser.outer.Outer;
import org.kframework.parser.utils.KoreIT;

public class TestKILtoKORE {

    static class KILtoKORE {
        public org.kframework.kore.outer.Definition convert(Definition d) {
            Set<org.kframework.kore.outer.Require> requires = d.getItems()
                    .stream().filter(i -> i instanceof Require)
                    .map(i -> convert((Require) i)).collect(Collectors.toSet());

            Set<org.kframework.kore.outer.Module> modules = d.getItems()
                    .stream().filter(i -> i instanceof Module)
                    .map(i -> convert((Module) i)).collect(Collectors.toSet());

            return new org.kframework.kore.outer.Definition(
                    immutable(requires), immutable(modules));
        }

        private org.kframework.kore.outer.Require convert(Require i) {
            return new org.kframework.kore.outer.Require(new java.io.File(
                    i.getValue()));
        }

        private org.kframework.kore.outer.Module convert(Module i) {
            Set<Sentence> items = i.getItems().stream()
                    .flatMap(j -> convert(j).stream())
                    .collect(Collectors.toSet());
            return new org.kframework.kore.outer.Module(i.getName(),
                    immutable(items), convert(i.getAttributes()));
        }

        private Set<org.kframework.kore.outer.Sentence> convert(ModuleItem i) {
            if (i instanceof Import) {
                Set<Sentence> res = new HashSet<org.kframework.kore.outer.Sentence>();
                res.add(new org.kframework.kore.outer.Import(((Import) i)
                        .getName(), Attributes()));
                return res;
            } else if (i instanceof Syntax) {
                return convert((Syntax) i);
            } else
                return null;
        }

        private org.kframework.kore.outer.Sentence convert(Import s) {
            return null;
        }

        private Set<org.kframework.kore.outer.Sentence> convert(Syntax s) {
            Set<Sentence> res = new HashSet<org.kframework.kore.outer.Sentence>();

            if (s.getPriorityBlocks().size() == 0)
                res.add(new SyntaxSort(convert(s.getDeclaredSort().getSort()),
                        convert(s.getAttributes())));

            for (PriorityBlock b : s.getPriorityBlocks()) {
            }

            return res;
        }

        private org.kframework.kore.Sort convert(Sort sort) {
            return Sort(sort.getName());
        }

        private org.kframework.kore.Attributes convert(Attributes attributes) {
            Set<K> attributesSet = attributes
                    .keySet()
                    .stream()
                    .map(key -> {
                        String keyString = key.toString();
                        String valueString = attributes.get(key).getValue()
                                .toString();

                        return (K) KApply(
                                KLabel(keyString),
                                KList(KToken(Sort("AttributeValue"),
                                        KString(valueString))));
                    }).collect(Collectors.toSet());

            return Attributes(KList(attributesSet));
        }
    }

    private static final String ROOT = "src/test/resources/convertor-tests/";

    @Rule
    public TestName name = new TestName();

    @Test
    public void emptyModule() throws IOException {
        standardTest();
    }

    @Test
    public void simpleSyntax() throws IOException {
        standardTest();
    }
    
    @Test
    public void syntaxWithAttributes() throws IOException {
        standardTest();
    }

    private void standardTest() throws IOException {
        File file = new File(ROOT + name.getMethodName() + ".k");
        String definitionText = Files.lines(file.toPath())
                .reduce((x, y) -> x + "\n" + y).get();

        org.kframework.kore.outer.Definition koreDefintion = toKORE(definitionText);
        assertEquals(definitionText.trim(), koreDefintion.toString().trim());
    }

    private org.kframework.kore.outer.Definition toKORE(String testedDefintion) {
        Definition def = new Definition();
        def.setItems(Outer.parse(Sources.generatedBy(KoreIT.class),
                testedDefintion, null));
        
        System.out.println(def);

        KILtoKORE convertor = new KILtoKORE();
        org.kframework.kore.outer.Definition converted = convertor.convert(def);
        return converted;
    }
}
