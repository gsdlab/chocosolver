package org.clafer.tree;

import choco.Choco;
import choco.kernel.solver.Solver;

/**
 *
 * @author jimmy
 */
public class IntClafer extends AtomicClafer {

    private final int bitwidth;

    public IntClafer(int bitwidth) {
        super("int", 1 << checkBitwidth(bitwidth), Choco.emptySet());
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
        // Do nothing
    }
}
