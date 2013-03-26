package org.clafer.ast.compiler;

import java.util.HashMap;
import java.util.Map;
import org.clafer.Check;

/**
 *
 * @author jimmy
 */
abstract class AstCacheMap<A, B> {

    private final Map<A, B> cache = new HashMap<A, B>();

    public B get(A a) {
        B b = cache.get(a);
        if (b == null) {
            b = Check.notNull(compile(a));
            cache.put(a, b);
        }
        return b;
    }

    abstract B compile(A a);
}
