package org.clafer.tree;

import choco.Choco;
import choco.kernel.model.Model;
import choco.kernel.model.variables.integer.IntegerVariable;
import choco.kernel.solver.Solver;
import org.clafer.tree.analysis.Analysis;

/**
 *
 * @author jimmy
 */
public class IntClafer extends AtomicClafer {

    private final int low, high;

    public IntClafer(int low, int high) {
        super("int", high - low + 1, Choco.emptySet(), new IntegerVariable[0]);
        this.low = low;
        this.high = high;
    }

    @Override
    public int getScopeLow() {
        return low;
    }

    @Override
    public int getScopeHigh() {
        return high;
    }

    @Override
    protected void print(Solver solver, String indent, int parent, Appendable output) {
        throw new UnsupportedOperationException("Ccannot print primitive int clafer");
    }

    @Override
    public AtomicClafer extending(AbstractClafer superClafer) {
        throw new UnsupportedOperationException("Cannot add extend the primitive int clafer");
    }

    @Override
    public RefClafer refTo(AtomicClafer type) {
        throw new UnsupportedOperationException("Cannot add ref under the primitive int clafer");
    }

    @Override
    public RefClafer refToUnique(AtomicClafer type) {
        throw new UnsupportedOperationException("Cannot add ref under the primitive int clafer");
    }

    @Override
    public ConcreteClafer addChild(String name, int scope, Card card) {
        throw new UnsupportedOperationException("Cannot add children under the primitive int clafer");
    }

    @Override
    public void build(Model model, Analysis analysis) {
        throw new UnsupportedOperationException("Cannot build the primitive int clafer");
    }
}
