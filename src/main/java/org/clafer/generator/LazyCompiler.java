package org.clafer.generator;

import java.util.HashMap;
import java.util.Map;
import java.util.Stack;
import org.clafer.Check;

/**
 *
 * @author jimmy
 */
abstract class LazyCompiler<A, C> implements SolutionMap<A, C> {

    private final Map<A, C> cache = new HashMap<A, C>();
    private final Stack<A> entered = new Stack<A>();
    private boolean lock = true;

    @Override
    public boolean has(A a) {
        return cache.containsKey(a);
    }

    @Override
    public C get(A a) {
        if (entered.contains(a)) {
            throw new CompilerException(getClass() + " reentered");
        }
        entered.push(a);
        C c = cache.get(a);
        if (c == null) {
            if (lock) {
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

    void unlock() {
        lock = false;
    }

    void lock() {
        lock = true;
    }
}
