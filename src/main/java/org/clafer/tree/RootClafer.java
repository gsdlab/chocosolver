package org.clafer.tree;

import choco.Choco;
import choco.kernel.model.Model;
import choco.kernel.model.variables.set.SetVariable;
import choco.kernel.solver.Solver;

/**
 *
 * @author jimmy
 */
public class RootClafer extends AtomicClafer {

    public RootClafer(Model model) {
        super("root", 1, Choco.constant(new int[]{0}));
        model.addVariable(getSet());
    }

    public RootClafer(String name, int scope, SetVariable set) {
        super(name, scope, set);
    }

    public void print(Solver solver) {
        print(solver, "", 0);
    }
    
    @Override
    protected void print(Solver solver, String indent, int parent) {
        for (Clafer child : getChildren()) {
            child.print(solver, indent, 0);
        }
    }
}
