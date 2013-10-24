package org.clafer.ir;

import static org.clafer.ir.Irs.*;
import org.clafer.ir.compiler.IrCompiler;
import org.clafer.ir.compiler.IrSolutionMap;
import static org.junit.Assert.*;
import org.junit.Test;
import solver.Solver;
import solver.constraints.ICF;
import solver.variables.IntVar;
import solver.variables.VF;

/**
 *
 * @author jimmy
 */
public class BasicIntExprTest extends ExprTest {

    @Test(timeout = 60000)
    public void testAdd() {
        for (int repeat = 0; repeat < 20; repeat++) {
            IrModule module = new IrModule();
            IrIntVar[] is = randInts(nextInt(5));
            IrIntExpr add = add(is);
            IrIntVar sumVar = domainInt("sum", add.getDomain());
            module.addConstraint(equal(sumVar, add));
            module.addVariables(is);
            module.addVariables(sumVar);

            Solver irSolver = new Solver();
            IrSolutionMap map = IrCompiler.compile(module, irSolver);
            int count = 0;
            if (randomizeStrategy(irSolver).findSolution()) {
                do {
                    int sum = 0;
                    for (IrIntVar i : is) {
                        sum += map.getIntValue(i);
                    }
                    assertEquals(sum, map.getIntValue(sumVar));
                    count++;
                } while (irSolver.nextSolution());
            }

            if (is.length == 0) {
                assertEquals(1, count);
            } else {
                Solver solver = new Solver();
                IntVar[] vs = toIntVars(is, solver);
                IntVar sum = VF.enumerated("sum", -100, 100, solver);
                solver.post(ICF.sum(vs, sum));
                assertEquals(randomizeStrategy(solver).findAllSolutions(), count);
            }
        }
    }

    @Test(timeout = 60000)
    public void testSub() {
        for (int repeat = 0; repeat < 200; repeat++) {
            IrModule module = new IrModule();
            IrIntVar[] is = randInts(nextInt(5));
            IrIntExpr sub = sub(is);
            IrIntVar diffVar = domainInt("diff", sub.getDomain());
            IrIntVar diffVar2 = domainInt("diff2", sub.getDomain());
            module.addConstraint(equal(diffVar, sub));
            module.addConstraint(equal(diffVar, diffVar2));
            module.addVariables(is);
            module.addVariables(diffVar, diffVar2);

            Solver irSolver = new Solver();
            IrSolutionMap map = IrCompiler.compile(module, irSolver);
            int count = 0;
            if (randomizeStrategy(irSolver).findSolution()) {
                do {
                    if (is.length == 0) {
                        assertEquals(0, map.getIntValue(diffVar));
                    } else {
                        int diff = map.getIntValue(is[0]);
                        for (int i = 1; i < is.length; i++) {
                            diff -= map.getIntValue(is[i]);
                        }
                        assertEquals(diff, map.getIntValue(diffVar));
                    }
                    count++;
                } while (irSolver.nextSolution());
            }

            if (is.length == 0) {
                assertEquals(1, count);
            } else {
                Solver solver = new Solver();
                IntVar[] vs = toIntVars(is, solver);
                IntVar diff = VF.enumerated("diff", -100, 100, solver);
                int[] coefficients = new int[vs.length];
                coefficients[0] = 1;
                for (int i = 1; i < coefficients.length; i++) {
                    coefficients[i] = -1;
                }
                solver.post(ICF.scalar(vs, coefficients, diff));
                assertEquals(randomizeStrategy(solver).findAllSolutions(), count);
            }
        }
    }
}
