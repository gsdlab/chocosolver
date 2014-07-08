package org.clafer.ir.compiler;

import static org.clafer.domain.Domains.*;
import org.clafer.ir.IrIntVar;
import org.clafer.ir.IrModule;
import org.clafer.ir.IrSetVar;
import static org.clafer.ir.Irs.*;
import static org.junit.Assert.*;
import org.junit.Test;
import solver.Solver;
import solver.search.strategy.SetStrategyFactory;
import solver.variables.SetVar;

/**
 *
 * @author jimmy
 */
public class IrCompilerTest {

    @Test
    public void testSetVarCard() {
        IrModule module = new IrModule();
        IrSetVar var = set("set", boundDomain(0, 10), boundDomain(1, 2), boundDomain(2, 3));
        module.addVariable(var);

        Solver solver = new Solver();
        IrSolutionMap map = IrCompiler.compile(module, solver);
        solver.set(SetStrategyFactory.force_first(new SetVar[]{map.getVar(var).getRight()}));

        int count = 0;
        if (solver.findSolution()) {
            do {
                assertTrue(map.getValue(var).length <= 3);
                count++;
            } while (solver.nextSolution());
        }
        assertEquals(10, count);
    }

    @Test
    public void testTwoCard() {
        IrModule module = new IrModule();
        IrSetVar var = set("set", boundDomain(0, 3));
        IrIntVar card1 = domainInt("|set1|", boundDomain(0, 4));
        IrIntVar card2 = domainInt("|set2|", boundDomain(0, 4));
        module.addVariables(var, card1, card2);

        module.addConstraint(equal(card1, card(var)));
        module.addConstraint(equal(card2, card(var)));

        Solver solver = new Solver();
        IrSolutionMap map = IrCompiler.compile(module, solver);
        solver.set(SetStrategyFactory.force_first(new SetVar[]{map.getVar(var).getRight()}));

        int count = 0;
        if (solver.findSolution()) {
            do {
                assertEquals(map.getValue(var).length, map.getValue(card1));
                assertEquals(map.getValue(var).length, map.getValue(card2));
                count++;
            } while (solver.nextSolution());
        }
        assertEquals(16, count);
    }
}
