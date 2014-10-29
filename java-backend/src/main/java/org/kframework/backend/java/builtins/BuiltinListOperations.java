// Copyright (c) 2013-2014 K Team. All Rights Reserved.
package org.kframework.backend.java.builtins;

import org.kframework.backend.java.kil.BuiltinList;
import org.kframework.backend.java.kil.Sort;
import org.kframework.backend.java.kil.Term;
import org.kframework.backend.java.kil.TermContext;


/**
 * Table of {@code public static} methods on builtin lists.
 *
 * @author AndreiS
 */
public class BuiltinListOperations {

    public static Term constructor(Term list1, Term list2, TermContext context) {
        if (list1.sort() != Sort.LIST || list2.sort() != Sort.LIST) {
            throw new IllegalArgumentException();
        }
        return BuiltinList.concatenate(list1, list2);
    }

    public static Term unit(TermContext context) {
        return BuiltinList.EMPTY_LIST;
    }

    public static Term element(Term element, TermContext context) {
        BuiltinList.Builder builder = BuiltinList.builder();
        builder.addItem(element);
        return builder.build();
    }

}
