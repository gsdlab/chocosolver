package org.clafer.test;

import org.chocosolver.solver.Model;
import org.chocosolver.solver.variables.IntVar;
import org.clafer.ir.IrIntExpr;
import org.clafer.ir.IrIntVar;
import static org.clafer.ir.Irs.constant;
import org.clafer.ir.compiler.IrSolutionMap;

/**
 *
 * @author jimmy
 */
public class FixedTerm implements Term {

    private final int c;

    FixedTerm(int c) {
        this.c = c;
    }

    @Override
    public IrIntExpr toIrExpr() {
        return constant(c);
    }

    @Override
    public IrIntVar getIrVar() {
        return constant(c);
    }

    @Override
    public IntVar toChocoVar(Model model) {
        return model.intVar(c);
    }

    @Override
    public int getValue(IrSolutionMap map) {
        return c;
    }

    @Override
    public String toString() {
        return Integer.toString(c);
    }
}
