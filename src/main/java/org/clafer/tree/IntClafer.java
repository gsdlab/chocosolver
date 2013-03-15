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

    private final int bitwidth;

    public IntClafer(int bitwidth) {
        super("int", 1 << checkBitwidth(bitwidth), Choco.emptySet(), new IntegerVariable[0]);
        this.bitwidth = bitwidth;
    }

    private static int checkBitwidth(int bitwidth) {
        if (bitwidth < 1 || bitwidth > 30) {
            throw new IllegalArgumentException("Bitwidth has to be in [1,30], received \"" + bitwidth + "\"");
        }
        return bitwidth;
    }

    @Override
    public int getScopeLow() {
        return -(1 << (bitwidth - 1));
    }

    @Override
    public int getScopeHigh() {
        return (1 << (bitwidth - 1)) - 1;
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
    public void refTo(AtomicClafer type) {
        throw new UnsupportedOperationException("Cannot add ref under the primitive int clafer");
    }

    @Override
    public void refToUnique(AtomicClafer type) {
        throw new UnsupportedOperationException("Cannot add ref under the primitive int clafer");
    }

    @Override
    public ConcreteClafer addChildClafer(String name, int scope, Card card) {
        throw new UnsupportedOperationException("Cannot add children under the primitive int clafer");
    }

    @Override
    public void build(Model model, Analysis analysis) {
        throw new UnsupportedOperationException("Cannot build the primitive int clafer");
    }
}
