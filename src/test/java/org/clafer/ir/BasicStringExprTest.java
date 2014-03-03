package org.clafer.ir;

import org.clafer.ClaferTest;
import org.clafer.choco.constraint.Constraints;
import static org.clafer.ir.Irs.*;
import org.clafer.ir.compiler.IrCompiler;
import org.clafer.ir.compiler.IrSolutionMap;
import static org.hamcrest.CoreMatchers.*;
import static org.junit.Assert.*;
import org.junit.Test;
import solver.Solver;
import solver.constraints.ICF;
import solver.variables.IntVar;

/**
 *
 * @author jimmy
 */
public class BasicStringExprTest extends ClaferTest {

    @Test(timeout = 60000)
    public void testLength() {
        for (int repeat = 0; repeat < 20; repeat++) {
            IrModule module = new IrModule();
            IrIntVar length = randInt();
            IrStringVar word = randString();
            module.addConstraint(equal(length, length(word)));
            module.addVariables(length, word);

            Solver irSolver = new Solver();
            IrSolutionMap map = IrCompiler.compile(module, irSolver);
            int count = 0;
            if (randomizeStrategy(irSolver).findSolution()) {
                do {
                    assertEquals(map.getIntValue(length), map.getStringValue(word).length());
                    count++;
                } while (irSolver.nextSolution());
            }

            Solver solver = new Solver();
            IntVar clength = toIntVar(length, solver);
            CStringVar cword = toCStringVar(word, solver);
            solver.post(ICF.arithm(clength, "=", cword.getLength()));
            assertEquals(randomizeStrategy(solver).findAllSolutions(), count);
        }
    }

    @Test(timeout = 60000)
    public void testPrefix() {
        for (int repeat = 0; repeat < 20; repeat++) {
            IrModule module = new IrModule();
            IrStringVar prefix = randString();
            IrStringVar word = randString();
            module.addConstraint(prefix(prefix, word));
            module.addVariables(prefix, word);

            Solver irSolver = new Solver();
            IrSolutionMap map = IrCompiler.compile(module, irSolver);
            int count = 0;
            if (randomizeStrategy(irSolver).findSolution()) {
                do {
                    assertThat(map.getStringValue(word), startsWith(map.getStringValue(prefix)));
                    count++;
                } while (irSolver.nextSolution());
            }

            Solver solver = new Solver();
            CStringVar cprefix = toCStringVar(prefix, solver);
            CStringVar cword = toCStringVar(word, solver);
            solver.post(Constraints.prefix(
                    cprefix.getChars(), cprefix.getLength(),
                    cword.getChars(), cword.getLength()));
            assertEquals(randomizeStrategy(solver).findAllSolutions(), count);
        }
    }

    @Test(timeout = 60000)
    public void testSuffix() {
        for (int repeat = 0; repeat < 20; repeat++) {
            IrModule module = new IrModule();
            IrStringVar suffix = randString();
            IrStringVar word = randString();
            module.addConstraint(suffix(suffix, word));
            module.addVariables(suffix, word);

            Solver irSolver = new Solver();
            IrSolutionMap map = IrCompiler.compile(module, irSolver);
            int count = 0;
            if (randomizeStrategy(irSolver).findSolution()) {
                do {
                    assertThat(map.getStringValue(word), endsWith(map.getStringValue(suffix)));
                    count++;
                } while (irSolver.nextSolution());
            }

            Solver solver = new Solver();
            CStringVar csuffix = toCStringVar(suffix, solver);
            CStringVar cword = toCStringVar(word, solver);
            solver.post(Constraints.suffix(
                    csuffix.getChars(), csuffix.getLength(),
                    cword.getChars(), cword.getLength()));
            assertEquals(randomizeStrategy(solver).findAllSolutions(), count);
        }
    }

    @Test(timeout = 60000)
    public void testConcat() {
        for (int repeat = 0; repeat < 20; repeat++) {
            IrModule module = new IrModule();
            IrStringVar concat = randString();
            IrStringVar left = randString();
            IrStringVar right = randString();
            module.addConstraint(equal(concat, concat(left, right)));
            module.addVariables(concat, left, right);

            Solver irSolver = new Solver();
            IrSolutionMap map = IrCompiler.compile(module, irSolver);
            int count = 0;
            if (randomizeStrategy(irSolver).findSolution()) {
                do {
                    assertEquals(map.getStringValue(concat),
                            map.getStringValue(left) + map.getStringValue(right));
                    count++;
                } while (irSolver.nextSolution());
            }

            Solver solver = new Solver();
            CStringVar cconcat = toCStringVar(concat, solver);
            CStringVar cleft = toCStringVar(left, solver);
            CStringVar cright = toCStringVar(right, solver);
            solver.post(Constraints.concat(
                    cleft.getChars(), cleft.getLength(),
                    cright.getChars(), cright.getLength(),
                    cconcat.getChars(), cconcat.getLength()));
            assertEquals(randomizeStrategy(solver).findAllSolutions(), count);
        }
    }
}
