package org.clafer.tree;

import choco.kernel.model.Model;
import choco.kernel.solver.Solver;
import java.io.IOException;
import org.clafer.Check;

/**
 *
 * @author jimmy
 */
public abstract class Clafer {

    private final String name;
    private final int scope;

    public Clafer(String name, int scope) {
        this.name = Check.notNull(name);
        if (scope < 1) {
            throw new IllegalArgumentException("Scope has to be positive, received \"" + scope + "\"");
        }
        this.scope = scope;
    }

    public String getName() {
        return name;
    }

    public int getScope() {
        return scope;
    }

    public int getScopeLow() {
        return 0;
    }

    public int getScopeHigh() {
        return scope - 1;
    }

    protected abstract void print(Solver solver, String indent, int parent, Appendable output)
            throws IOException;
}
