package org.clafer.ir.compiler;

import java.util.Collection;
import org.clafer.generator.*;
import java.util.HashMap;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Stack;
import org.clafer.Check;

/**
 *
 * @author jimmy
 */
abstract class LazyCompiler<A, C> {

    private final Map<A, C> cache = new HashMap<A, C>();
    private final Stack<A> entered = new Stack<A>();
    private boolean lock = true;

    public boolean has(A a) {
        return cache.containsKey(a);
    }

    public C get(A a) {
        if (entered.contains(a)) {
            throw new CompilerException(getClass() + " reentered");
        }
        entered.push(a);
        C c = cache.get(a);
        if (c == null) {
            if (lock) {
                System.out.println(a);
                // No more changes.
                throw new CompilerException(getClass() + " is locked");
            }
            c = Check.notNull(compile(a));
            cache.put(a, c);
        }
        entered.pop();
        return c;
    }

    abstract C compile(A a);

    final void finish() {
        for (Entry<A, C> entry : cache.entrySet()) {
            finish(entry.getKey(), entry.getValue());
        }
    }

    void finish(A a, C c) {
    }

    void unlock() {
        lock = false;
    }

    void lock() {
        lock = true;
    }

    Collection<C> values() {
        return cache.values();
    }
}
