package org.clafer.ir.compiler;

import java.util.Arrays;
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
        module.addConstraint(nop(var));

        Solver solver = new Solver();
        IrSolutionMap map = IrCompiler.compile(module, solver);
        solver.set(SetStrategyFactory.setLex(new SetVar[]{map.getSetVar(var)}));

        int count = 0;
        if (solver.findSolution()) {
            do {
                assertTrue(Arrays.toString(map.getSetValue(var)), map.getSetValue(var).length <= 3);
                count++;
            } while (solver.nextSolution());
        }
        assertEquals(10, count);
    }
}
