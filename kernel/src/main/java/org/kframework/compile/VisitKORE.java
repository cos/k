// Copyright (c) 2015 K Team. All Rights Reserved.
package org.kframework.compile;

import org.kframework.kore.*;

/**
 * Abstract visitor for K.
 */
public class VisitKORE extends AbstractKORETransformer<Void> {

    @Override
    public Void apply(KApply k) {
        k.klist().items().stream().forEach(this::apply);
        return null;
    }

    @Override
    public Void apply(KRewrite k) {
        apply(k.left());
        apply(k.right());
        return null;
    }

    @Override
    public Void apply(KToken k) {
        return null;
    }

    @Override
    public Void apply(KVariable k) {
        return null;
    }

    @Override
    public Void apply(KSequence k) {
        k.items().stream().forEach(this::apply);
        return null;
    }

    @Override
    public Void apply(InjectedKLabel k) {
        return null;
    }
}
