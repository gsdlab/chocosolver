package org.clafer.ir.compiler;

import org.chocosolver.solver.Model;
import static org.clafer.domain.Domains.*;
import org.clafer.ir.IrIntVar;
import org.clafer.ir.IrModule;
import org.clafer.ir.IrSetVar;
import static org.clafer.ir.Irs.*;
import static org.junit.Assert.*;
import org.junit.Test;
import org.chocosolver.solver.Solver;
import org.chocosolver.solver.search.strategy.Search;

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

        Model model = new Model();
        IrSolutionMap map = IrCompiler.compile(module, model);
        Solver solver = model.getSolver();
        solver.setSearch(Search.setVarSearch(map.getVar(var).getRight()));

        int count = 0;
        while (solver.solve()) {
            assertTrue(map.getValue(var).length <= 3);
            count++;
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

        Model model = new Model();
        IrSolutionMap map = IrCompiler.compile(module, model);
        Solver solver = model.getSolver();
        solver.setSearch(Search.setVarSearch(map.getVar(var).getRight()));

        int count = 0;
        while (solver.solve()) {
            assertEquals(map.getValue(var).length, map.getValue(card1));
            assertEquals(map.getValue(var).length, map.getValue(card2));
            count++;
        }
        assertEquals(16, count);
    }
}
