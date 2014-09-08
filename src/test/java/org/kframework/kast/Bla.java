package org.kframework.kast;

import scala.collection.Seq;

class Bla implements TermConstructor<Object> {

    @Override
    public Term convert(Term termToConvert, Object context) {
        // TODO Auto-generated method stub
        return TermConstructor$class.convert(this, termToConvert, context);
    }

    public Term apply(String klabel, Seq<Term> klist, Attributes as, Object context) {
        // TODO Auto-generated method stub
        return null;
    }

}