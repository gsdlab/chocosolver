package org.clafer.tree;

import choco.Choco;
import choco.kernel.model.Model;
import choco.kernel.model.variables.set.SetVariable;
import choco.kernel.solver.Solver;
import java.io.IOException;

/**
 *
 * @author jimmy
 */
public class RootClafer extends AtomicClafer {

    public RootClafer(Model model) {
        super("root", 1, Choco.constant(new int[]{0}));
    }

    public RootClafer(String name, int scope, SetVariable set) {
        super(name, scope, set);
    }

    public void print(Solver solver, Appendable output)
            throws IOException {
        print(solver, "", 0, output);
    }

    @Override
    protected void print(Solver solver, String indent, int parent, Appendable output)
            throws IOException {
        for (Clafer child : getRefAndChildren()) {
            child.print(solver, indent, 0, output);
        }
    }
}
