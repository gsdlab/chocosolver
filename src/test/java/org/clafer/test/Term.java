package org.clafer.test;

import org.chocosolver.solver.Model;
import org.chocosolver.solver.variables.IntVar;
import org.clafer.ir.IrIntExpr;
import org.clafer.ir.IrIntVar;
import org.clafer.ir.compiler.IrSolutionMap;

/**
 *
 * @author jimmy
 */
public interface Term {

    IrIntExpr toIrExpr();

    IrIntVar getIrVar();

    IntVar toChocoVar(Model model);

    int getValue(IrSolutionMap map);
}
