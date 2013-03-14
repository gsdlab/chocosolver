package org.clafer.tree;

import choco.Choco;
import choco.kernel.model.Model;
import choco.kernel.model.variables.integer.IntegerVariable;
import choco.kernel.solver.Solver;
import java.io.IOException;

/**
 *
 * @author jimmy
 */
public class RootClafer extends AtomicClafer {

    public RootClafer() {
        super("root", 1, Choco.constant(new int[]{0}), new IntegerVariable[]{Choco.constant(1)});
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

    @Override
    protected void optimize(Model model, Card parentCard) {
        globalCard = new Card(1, 1);
        for (AtomicClafer child : getChildren()) {
            child.optimize(model, globalCard);
        }
    }

    @Override
    public void build(Model model) {
        optimize(model, null);
        super.build(model);
    }
}
