package org.kframework.kore;

import org.junit.Test;
import static org.kframework.kore.Interface.*;

public class VisitorTest {
	class FooTransformer extends AbstractKORETransformer<K> {

		@Override
		public K transform(KApply k) {
			return k.copy();
		}

		@Override
		public K transform(KRewrite k) {
			return k.copy();
		}

		@Override
		public K transform(KToken k) {
			return k.copy();
		}

		@Override
		public K transform(KVariable k) {
			return k.copy();
		}
	}

	@Test
	public void testFoo() {
		FooTransformer fooTransformer = new FooTransformer() {
			@Override
			public K transform(KToken k) {
				return KVariable(k.s().s());
			}
		};

		K transformed = fooTransformer.transform(KToken(Sort("foo"),
				KString("bla")));

		System.out.println(transformed);
	}
}