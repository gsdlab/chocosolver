package org.clafer.test;

import org.clafer.ir.IrIntExpr;
import org.clafer.ir.IrIntVar;
import org.clafer.ir.compiler.IrSolutionMap;
import solver.Solver;
import solver.variables.IntVar;

/**
 *
 * @author jimmy
 */
public interface Term {

    IrIntExpr toIrExpr();

    IrIntVar getIrVar();

    IntVar toChocoVar(Solver solver);

    int getValue(IrSolutionMap map);
}
