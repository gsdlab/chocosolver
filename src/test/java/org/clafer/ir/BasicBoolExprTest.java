package org.clafer.ir;

import java.util.Arrays;
import org.clafer.common.Util;
import static org.clafer.ir.Irs.*;
import org.clafer.ir.compiler.IrCompiler;
import org.clafer.ir.compiler.IrSolutionMap;
import static org.junit.Assert.*;
import org.junit.Test;
import solver.Solver;
import solver.constraints.ICF;
import solver.constraints.LCF;
import solver.constraints.set.SCF;
import solver.variables.BoolVar;
import solver.variables.IntVar;
import solver.variables.SetVar;
import solver.variables.VF;

/**
 *
 * @author jimmy
 */
public class BasicBoolExprTest extends ExprTest {

    @Test(timeout = 60000)
    public void testLone() {
        for (int repeat = 0; repeat < 20; repeat++) {
            IrModule module = new IrModule();
            IrBoolVar[] is = randBools(nextInt(5));
            module.addConstraint(lone(is));
            module.addVariables(is);

            Solver irSolver = new Solver();
            IrSolutionMap map = IrCompiler.compile(module, irSolver);
            int count = 0;
            if (randomizeStrategy(irSolver).findSolution()) {
                do {
                    int sum = 0;
                    for (IrIntVar i : is) {
                        sum += map.getIntValue(i);
                    }
                    assertTrue(sum <= 1);
                    count++;
                } while (irSolver.nextSolution());
            }

            if (is.length == 0) {
                assertEquals(1, count);
            } else {
                Solver solver = new Solver();
                BoolVar[] vs = toBoolVars(is, solver);
                IntVar sum = VF.enumerated("sum", 0, 1, solver);
                solver.post(ICF.sum(vs, sum));
                assertEquals(randomizeStrategy(solver).findAllSolutions(), count);
            }
        }
    }

    @Test(timeout = 60000)
    public void testOne() {
        for (int repeat = 0; repeat < 20; repeat++) {
            IrModule module = new IrModule();
            IrBoolVar[] is = randBools(nextInt(5));
            module.addConstraint(one(is));
            module.addVariables(is);

            Solver irSolver = new Solver();
            IrSolutionMap map = IrCompiler.compile(module, irSolver);
            int count = 0;
            if (randomizeStrategy(irSolver).findSolution()) {
                do {
                    int sum = 0;
                    for (IrIntVar i : is) {
                        sum += map.getIntValue(i);
                    }
                    assertEquals(1, sum);
                    count++;
                } while (irSolver.nextSolution());
            }

            if (is.length == 0) {
                assertEquals(0, count);
            } else {
                Solver solver = new Solver();
                BoolVar[] vs = toBoolVars(is, solver);
                solver.post(ICF.sum(vs, VF.one(solver)));
                assertEquals(randomizeStrategy(solver).findAllSolutions(), count);
            }
        }
    }

    @Test(timeout = 60000)
    public void testIfThenElse() {
        for (int repeat = 0; repeat < 20; repeat++) {
            IrModule module = new IrModule();
            IrBoolVar b1 = randBool();
            IrBoolVar b2 = randBool();
            IrBoolVar b3 = randBool();
            module.addConstraint(ifThenElse(b1, b2, b3));
            module.addVariables(b1, b2, b3);

            Solver irSolver = new Solver();
            IrSolutionMap map = IrCompiler.compile(module, irSolver);
            int count = 0;
            if (randomizeStrategy(irSolver).findSolution()) {
                do {
                    assertTrue(map.getBoolValue(b1)
                            ? map.getBoolValue(b2)
                            : map.getBoolValue(b3));
                    count++;
                } while (irSolver.nextSolution());
            }

            Solver solver = new Solver();
            BoolVar v1 = toBoolVar(b1, solver);
            BoolVar v2 = toBoolVar(b2, solver);
            BoolVar v3 = toBoolVar(b3, solver);
            solver.post(LCF.ifThenElse(v1,
                    ICF.arithm(v2, "=", 1),
                    ICF.arithm(v3, "=", 1)));

            assertEquals(randomizeStrategy(solver).findAllSolutions(), count);
        }
    }

    @Test(timeout = 60000)
    public void testEqual() {
        for (int repeat = 0; repeat < 20; repeat++) {
            IrModule module = new IrModule();
            IrIntVar i1 = randInt();
            IrIntVar i2 = randInt();
            module.addConstraint(equal(i1, i2));
            module.addVariables(i1, i2);

            Solver irSolver = new Solver();
            IrSolutionMap map = IrCompiler.compile(module, irSolver);
            int count = 0;
            if (randomizeStrategy(irSolver).findSolution()) {
                do {
                    assertEquals(map.getIntValue(i1), map.getIntValue(i2));
                    count++;
                } while (irSolver.nextSolution());
            }

            Solver solver = new Solver();
            IntVar v1 = toIntVar(i1, solver);
            IntVar v2 = toIntVar(i2, solver);
            solver.post(ICF.arithm(v1, "=", v2));

            assertEquals(randomizeStrategy(solver).findAllSolutions(), count);
        }
    }

    @Test(timeout = 60000)
    public void testNotEqual() {
        for (int repeat = 0; repeat < 20; repeat++) {
            IrModule module = new IrModule();
            IrIntVar i1 = randInt();
            IrIntVar i2 = randInt();
            module.addConstraint(notEqual(i1, i2));
            module.addVariables(i1, i2);

            Solver irSolver = new Solver();
            IrSolutionMap map = IrCompiler.compile(module, irSolver);
            int count = 0;
            if (randomizeStrategy(irSolver).findSolution()) {
                do {
                    assertNotEquals(map.getIntValue(i1), map.getIntValue(i2));
                    count++;
                } while (irSolver.nextSolution());
            }

            Solver solver = new Solver();
            IntVar v1 = toIntVar(i1, solver);
            IntVar v2 = toIntVar(i2, solver);
            solver.post(ICF.arithm(v1, "!=", v2));

            assertEquals(randomizeStrategy(solver).findAllSolutions(), count);
        }
    }

    @Test(timeout = 60000)
    public void testLessThan() {
        for (int repeat = 0; repeat < 20; repeat++) {
            IrModule module = new IrModule();
            IrIntVar i1 = randInt();
            IrIntVar i2 = randInt();
            module.addConstraint(lessThan(i1, i2));
            module.addVariables(i1, i2);

            Solver irSolver = new Solver();
            IrSolutionMap map = IrCompiler.compile(module, irSolver);
            int count = 0;
            if (randomizeStrategy(irSolver).findSolution()) {
                do {
                    assertTrue(map.getIntValue(i1) < map.getIntValue(i2));
                    count++;
                } while (irSolver.nextSolution());
            }

            Solver solver = new Solver();
            IntVar v1 = toIntVar(i1, solver);
            IntVar v2 = toIntVar(i2, solver);
            solver.post(ICF.arithm(v1, "<", v2));

            assertEquals(randomizeStrategy(solver).findAllSolutions(), count);
        }
    }

    @Test(timeout = 60000)
    public void testLessThanEqual() {
        for (int repeat = 0; repeat < 20; repeat++) {
            IrModule module = new IrModule();
            IrIntVar i1 = randInt();
            IrIntVar i2 = randInt();
            module.addConstraint(lessThanEqual(i1, i2));
            module.addVariables(i1, i2);

            Solver irSolver = new Solver();
            IrSolutionMap map = IrCompiler.compile(module, irSolver);
            int count = 0;
            if (randomizeStrategy(irSolver).findSolution()) {
                do {
                    assertTrue(map.getIntValue(i1) <= map.getIntValue(i2));
                    count++;
                } while (irSolver.nextSolution());
            }

            Solver solver = new Solver();
            IntVar v1 = toIntVar(i1, solver);
            IntVar v2 = toIntVar(i2, solver);
            solver.post(ICF.arithm(v1, "<=", v2));

            assertEquals(randomizeStrategy(solver).findAllSolutions(), count);
        }
    }

    @Test(timeout = 60000)
    public void testGreaterThan() {
        for (int repeat = 0; repeat < 20; repeat++) {
            IrModule module = new IrModule();
            IrIntVar i1 = randInt();
            IrIntVar i2 = randInt();
            module.addConstraint(greaterThan(i1, i2));
            module.addVariables(i1, i2);

            Solver irSolver = new Solver();
            IrSolutionMap map = IrCompiler.compile(module, irSolver);
            int count = 0;
            if (randomizeStrategy(irSolver).findSolution()) {
                do {
                    assertTrue(map.getIntValue(i1) > map.getIntValue(i2));
                    count++;
                } while (irSolver.nextSolution());
            }

            Solver solver = new Solver();
            IntVar v1 = toIntVar(i1, solver);
            IntVar v2 = toIntVar(i2, solver);
            solver.post(ICF.arithm(v1, ">", v2));

            assertEquals(randomizeStrategy(solver).findAllSolutions(), count);
        }
    }

    @Test(timeout = 60000)
    public void testGreaterThanEqual() {
        for (int repeat = 0; repeat < 20; repeat++) {
            IrModule module = new IrModule();
            IrIntVar i1 = randInt();
            IrIntVar i2 = randInt();
            module.addConstraint(greaterThanEqual(i1, i2));
            module.addVariables(i1, i2);

            Solver irSolver = new Solver();
            IrSolutionMap map = IrCompiler.compile(module, irSolver);
            int count = 0;
            if (randomizeStrategy(irSolver).findSolution()) {
                do {
                    assertTrue(map.getIntValue(i1) >= map.getIntValue(i2));
                    count++;
                } while (irSolver.nextSolution());
            }

            Solver solver = new Solver();
            IntVar v1 = toIntVar(i1, solver);
            IntVar v2 = toIntVar(i2, solver);
            solver.post(ICF.arithm(v1, ">=", v2));

            assertEquals(randomizeStrategy(solver).findAllSolutions(), count);
        }
    }

    @Test(timeout = 60000)
    public void testSetEqual() {
        for (int repeat = 0; repeat < 20; repeat++) {
            IrModule module = new IrModule();
            IrSetVar s1 = randSet();
            IrSetVar s2 = randSet();

            module.addConstraint(equal(s1, s2));
            module.addVariables(s1, s2);

            Solver irSolver = new Solver();
            IrSolutionMap map = IrCompiler.compile(module, irSolver);
            int count = 0;
            if (randomizeStrategy(irSolver).findSolution()) {
                do {
                    assertArrayEquals(map.getSetValue(s1), map.getSetValue(s2));
                    count++;
                } while (irSolver.nextSolution());
            }

            Solver solver = new Solver();
            SetVar v1 = toSetVar(s1, solver);
            SetVar v2 = toSetVar(s2, solver);
            solver.post(SCF.all_equal(new SetVar[]{v1, v2}));

            assertEquals(randomizeStrategy(solver).findAllSolutions(), count);
        }
    }

    @Test(timeout = 60000)
    public void testSetNotEqual() {
        for (int repeat = 0; repeat < 20; repeat++) {
            IrModule module = new IrModule();
            IrSetVar s1 = randSet();
            IrSetVar s2 = randSet();
            module.addConstraint(notEqual(s1, s2));
            module.addVariables(s1, s2);

            Solver irSolver = new Solver();
            IrSolutionMap map = IrCompiler.compile(module, irSolver);
            int count = 0;
            if (randomizeStrategy(irSolver).findSolution()) {
                do {
                    assertFalse(Arrays.equals(map.getSetValue(s1), map.getSetValue(s2)));
                    count++;
                } while (irSolver.nextSolution());
            }

            Solver solver = new Solver();
            SetVar v1 = toSetVar(s1, solver);
            SetVar v2 = toSetVar(s2, solver);
            solver.post(SCF.all_different(new SetVar[]{v1, v2}));

            assertEquals(randomizeStrategy(solver).findAllSolutions(), count);
        }
    }

    @Test(timeout = 60000)
    public void testBoolChannel() {
        for (int repeat = 0; repeat < 20; repeat++) {
            IrModule module = new IrModule();
            IrBoolVar[] bs = randBools(nextInt(5));
            IrSetVar s = randPositiveSet();
            module.addConstraint(boolChannel(bs, s));
            module.addVariables(bs);
            module.addVariable(s);

            Solver irSolver = new Solver();
            IrSolutionMap map = IrCompiler.compile(module, irSolver);
            int count = 0;
            if (randomizeStrategy(irSolver).findSolution()) {
                do {
                    for (int i = 0; i < bs.length; i++) {
                        assertEquals(map.getBoolValue(bs[i]), Util.in(i, map.getSetValue(s)));
                    }
                    count++;
                } while (irSolver.nextSolution());
            }

            Solver solver = new Solver();
            solver.post(SCF.bool_channel(
                    toBoolVars(bs, solver),
                    toSetVar(s, solver),
                    0));

            assertEquals(randomizeStrategy(solver).findAllSolutions(), count);
        }
    }

    @Test(timeout = 60000)
    public void testAllDifferent() {
        for (int repeat = 0; repeat < 20; repeat++) {
            IrModule module = new IrModule();
            IrIntVar[] is = randInts(1 + nextInt(5));
            module.addConstraint(allDifferent(is));
            module.addVariables(is);

            Solver irSolver = new Solver();
            IrSolutionMap map = IrCompiler.compile(module, irSolver);
            int count = 0;
            if (randomizeStrategy(irSolver).findSolution()) {
                do {
                    for (int i = 0; i < is.length; i++) {
                        for (int j = i + 1; j < is.length; j++) {
                            assertNotEquals(map.getIntValue(is[i]), map.getIntValue(is[j]));
                        }
                    }
                    count++;
                } while (irSolver.nextSolution());
            }

            Solver solver = new Solver();
            solver.post(ICF.alldifferent(toIntVars(is, solver), "AC"));

            assertEquals(randomizeStrategy(solver).findAllSolutions(), count);
        }
    }
}
