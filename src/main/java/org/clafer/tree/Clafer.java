package org.clafer.tree;

import choco.kernel.solver.Solver;
import org.clafer.Check;

/**
 *
 * @author jimmy
 */
public abstract class Clafer {

    private final String name;

    public Clafer(String name) {
        this.name = Check.notNull(name);
    }

    public String getName() {
        return name;
    }

    protected abstract void print(Solver solver, String indent, int parent);
}
