package org.clafer.tree;

import choco.kernel.model.Model;
import choco.kernel.solver.Solver;
import java.io.IOException;
import org.clafer.Check;
import org.clafer.tree.analysis.Analysis;

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

    /**
     * Build this clafer. Does not build its children.
     * 
     * @param model 
     * @param analysis
     */
    protected abstract void build(Model model, Analysis analysis);

    /**
     * Print the clafers belonging to the parent and their children.
     * 
     * @param solver
     * @param indent - the prefix of the message
     * @param parent - the parent
     * @param output - stream the message here
     * @throws IOException 
     */
    protected abstract void print(Solver solver, String indent, int parent, Appendable output)
            throws IOException;
}
