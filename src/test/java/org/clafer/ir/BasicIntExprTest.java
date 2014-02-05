package org.clafer.ir;

import org.clafer.ClaferTest;
import org.clafer.common.Util;
import static org.clafer.ir.Irs.*;
import org.clafer.ir.compiler.IrCompiler;
import org.clafer.ir.compiler.IrSolutionMap;
import static org.junit.Assert.*;
import org.junit.Test;
import solver.Solver;
import solver.constraints.ICF;
import solver.variables.BoolVar;
import solver.variables.IntVar;
import solver.variables.VF;
import solver.variables.Variable;
import solver.variables.impl.FixedBoolVarImpl;
import solver.variables.impl.FixedIntVarImpl;
import solver.variables.view.IntView;

/**
 *
 * @author jimmy
 */
public class BasicIntExprTest extends ClaferTest {

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
        for (int repeat = 0; repeat < 20; repeat++) {
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

    @Test(timeout = 60000)
    public void testEqualXY() {
        for (int i = 0; i < 20; i++) {
            IrModule module = new IrModule();
            Term[] vars = new Term[]{randTerm(), randTerm()};
            Util.shuffle(vars, rand);
            module.addVariables(vars[0].getIrVar(), vars[1].getIrVar());
            module.addConstraint(equal(vars[0].toIrExpr(), vars[1].toIrExpr()));

            Solver irSolver = new Solver();
            IrSolutionMap map = IrCompiler.compile(module, irSolver);
            int count = 0;
            if (randomizeStrategy(irSolver).findSolution()) {
                do {
                    assertEquals(vars[0].getValue(map), vars[1].getValue(map));
                    count++;
                } while (irSolver.nextSolution());
            }
            assertTrue("Correct but not optimized.", irSolver.getNbCstrs() <= 1);
            assertTrue("Correct but not optimized.", irSolver.getNbVars() <= 4);
            for (Variable var : irSolver.getVars()) {
                assertFalse("Correct but not optimized.",
                        (var instanceof FixedIntVarImpl && !(var instanceof FixedBoolVarImpl))
                        || var instanceof IntView);
            }

            Solver solver = new Solver();
            solver.post(ICF.arithm(vars[0].toChocoVar(solver), "=", vars[1].toChocoVar(solver)));
            assertEquals(module.toString(), randomizeStrategy(solver).findAllSolutions(), count);
        }
    }

    @Test(timeout = 60000)
    public void testNotEqualXY() {
        for (int i = 0; i < 20; i++) {
            IrModule module = new IrModule();
            Term[] vars = new Term[]{randTerm(), randTerm()};
            Util.shuffle(vars, rand);
            module.addVariables(vars[0].getIrVar(), vars[1].getIrVar());
            module.addConstraint(notEqual(vars[0].toIrExpr(), vars[1].toIrExpr()));

            Solver irSolver = new Solver();
            IrSolutionMap map = IrCompiler.compile(module, irSolver);
            int count = 0;
            if (randomizeStrategy(irSolver).findSolution()) {
                do {
                    assertNotEquals(vars[0].getValue(map), vars[1].getValue(map));
                    count++;
                } while (irSolver.nextSolution());
            }
            assertTrue("Correct but not optimized.", irSolver.getNbCstrs() <= 1);
            assertTrue("Correct but not optimized.", irSolver.getNbVars() <= 4);
            for (Variable var : irSolver.getVars()) {
                assertFalse("Correct but not optimized.",
                        (var instanceof FixedIntVarImpl && !(var instanceof FixedBoolVarImpl))
                        || var instanceof IntView);
            }

            Solver solver = new Solver();
            solver.post(ICF.arithm(vars[0].toChocoVar(solver), "!=", vars[1].toChocoVar(solver)));
            assertEquals(module.toString(), randomizeStrategy(solver).findAllSolutions(), count);
        }
    }

    @Test(timeout = 60000)
    public void testLessThanXY() {
        for (int i = 0; i < 20; i++) {
            IrModule module = new IrModule();
            Term[] vars = new Term[]{randTerm(), randTerm()};
            Util.shuffle(vars, rand);
            module.addVariables(vars[0].getIrVar(), vars[1].getIrVar());
            module.addConstraint(lessThan(vars[0].toIrExpr(), vars[1].toIrExpr()));

            Solver irSolver = new Solver();
            IrSolutionMap map = IrCompiler.compile(module, irSolver);
            int count = 0;
            if (randomizeStrategy(irSolver).findSolution()) {
                do {
                    assertTrue(vars[0].getValue(map) < vars[1].getValue(map));
                    count++;
                } while (irSolver.nextSolution());
            }
            assertTrue("Correct but not optimized.", irSolver.getNbCstrs() <= 1);
            assertTrue("Correct but not optimized.", irSolver.getNbVars() <= 4);
            for (Variable var : irSolver.getVars()) {
                assertFalse("Correct but not optimized.",
                        (var instanceof FixedIntVarImpl && !(var instanceof FixedBoolVarImpl))
                        || var instanceof IntView);
            }

            Solver solver = new Solver();
            solver.post(ICF.arithm(vars[0].toChocoVar(solver), "<", vars[1].toChocoVar(solver)));
            assertEquals(module.toString(), randomizeStrategy(solver).findAllSolutions(), count);
        }
    }

    @Test(timeout = 60000)
    public void testLessThanEqualXY() {
        for (int i = 0; i < 20; i++) {
            IrModule module = new IrModule();
            Term[] vars = new Term[]{randTerm(), randTerm()};
            Util.shuffle(vars, rand);
            module.addVariables(vars[0].getIrVar(), vars[1].getIrVar());
            module.addConstraint(lessThanEqual(vars[0].toIrExpr(), vars[1].toIrExpr()));

            Solver irSolver = new Solver();
            IrSolutionMap map = IrCompiler.compile(module, irSolver);
            int count = 0;
            if (randomizeStrategy(irSolver).findSolution()) {
                do {
                    assertTrue(vars[0].getValue(map) <= vars[1].getValue(map));
                    count++;
                } while (irSolver.nextSolution());
            }
            assertTrue("Correct but not optimized.", irSolver.getNbCstrs() <= 1);
            assertTrue("Correct but not optimized.", irSolver.getNbVars() <= 4);
            for (Variable var : irSolver.getVars()) {
                assertFalse("Correct but not optimized.",
                        (var instanceof FixedIntVarImpl && !(var instanceof FixedBoolVarImpl))
                        || var instanceof IntView);
            }

            Solver solver = new Solver();
            solver.post(ICF.arithm(vars[0].toChocoVar(solver), "<=", vars[1].toChocoVar(solver)));
            assertEquals(module.toString(), randomizeStrategy(solver).findAllSolutions(), count);
        }
    }

    @Test(timeout = 60000)
    public void testEqualXYC() {
        for (int i = 0; i < 20; i++) {
            IrModule module = new IrModule();
            Term[] vars = new Term[]{randFixedTerm(), randTerm(), randTerm()};
            Util.shuffle(vars, rand);
            IrIntExpr add = Irs.add(vars[0].toIrExpr(), vars[1].toIrExpr());
            module.addVariables(vars[0].getIrVar(), vars[1].getIrVar(), vars[2].getIrVar());
            module.addConstraint(nextBool()
                    ? Irs.equal(add, vars[2].toIrExpr())
                    : Irs.equal(vars[2].toIrExpr(), add));

            Solver irSolver = new Solver();
            IrSolutionMap map = IrCompiler.compile(module, irSolver);
            int count = 0;
            if (randomizeStrategy(irSolver).findSolution()) {
                do {
                    assertEquals(vars[0].getValue(map) + vars[1].getValue(map), vars[2].getValue(map));
                    count++;
                } while (irSolver.nextSolution());
            }
            assertTrue("Correct but not optimized.", irSolver.getNbCstrs() <= 1);
            assertTrue("Correct but not optimized.", irSolver.getNbVars() <= 4);
            for (Variable var : irSolver.getVars()) {
                assertFalse("Correct but not optimized.",
                        (var instanceof FixedIntVarImpl && !(var instanceof FixedBoolVarImpl))
                        || var instanceof IntView);
            }

            Solver solver = new Solver();
            solver.post(ICF.sum(new IntVar[]{vars[0].toChocoVar(solver), vars[1].toChocoVar(solver)},
                    vars[2].toChocoVar(solver)));
            assertEquals(randomizeStrategy(solver).findAllSolutions(), count);
        }
    }

    @Test(timeout = 60000)
    public void testNotEqualXYC() {
        for (int i = 0; i < 20; i++) {
            IrModule module = new IrModule();
            Term[] vars = new Term[]{randFixedTerm(), randTerm(), randTerm()};
            Util.shuffle(vars, rand);
            IrIntExpr add = Irs.add(vars[0].toIrExpr(), vars[1].toIrExpr());
            module.addVariables(vars[0].getIrVar(), vars[1].getIrVar(), vars[2].getIrVar());
            module.addConstraint(nextBool()
                    ? Irs.notEqual(add, vars[2].toIrExpr())
                    : Irs.notEqual(vars[2].toIrExpr(), add));

            Solver irSolver = new Solver();
            IrSolutionMap map = IrCompiler.compile(module, irSolver);
            int count = 0;
            if (randomizeStrategy(irSolver).findSolution()) {
                do {
                    assertNotEquals(vars[0].getValue(map) + vars[1].getValue(map), vars[2].getValue(map));
                    count++;
                } while (irSolver.nextSolution());
            }
            assertTrue("Correct but not optimized.", irSolver.getNbCstrs() <= 1);
            assertTrue("Correct but not optimized.", irSolver.getNbVars() <= 4);
            for (Variable var : irSolver.getVars()) {
                assertFalse("Correct but not optimized.",
                        (var instanceof FixedIntVarImpl && !(var instanceof FixedBoolVarImpl))
                        || var instanceof IntView);
            }

            Solver solver = new Solver();
            IntVar v0 = vars[0].toChocoVar(solver);
            IntVar v1 = vars[1].toChocoVar(solver);
            IntVar v2 = vars[2].toChocoVar(solver);
            IntVar sum = VF.enumerated("sum", v0.getLB() + v1.getLB(), v0.getUB() + v1.getUB(), solver);
            solver.post(ICF.sum(new IntVar[]{v0, v1}, sum));
            solver.post(ICF.arithm(sum, "!=", v2));
            assertEquals(randomizeStrategy(solver).findAllSolutions(), count);
        }
    }

    @Test(timeout = 60000)
    public void testLessThanXYC() {
        for (int i = 0; i < 20; i++) {
            boolean left = nextBool();
            IrModule module = new IrModule();
            Term[] vars = new Term[]{randFixedTerm(), randTerm(), randTerm()};
            Util.shuffle(vars, rand);
            IrIntExpr add = Irs.add(vars[0].toIrExpr(), vars[1].toIrExpr());
            module.addVariables(vars[0].getIrVar(), vars[1].getIrVar(), vars[2].getIrVar());
            module.addConstraint(left
                    ? Irs.lessThan(add, vars[2].toIrExpr())
                    : Irs.lessThan(vars[2].toIrExpr(), add));

            Solver irSolver = new Solver();
            IrSolutionMap map = IrCompiler.compile(module, irSolver);
            int count = 0;
            if (randomizeStrategy(irSolver).findSolution()) {
                do {
                    assertTrue(left
                            ? vars[0].getValue(map) + vars[1].getValue(map) < vars[2].getValue(map)
                            : vars[2].getValue(map) < vars[0].getValue(map) + vars[1].getValue(map));
                    count++;
                } while (irSolver.nextSolution());
            }
            assertTrue("Correct but not optimized.", irSolver.getNbCstrs() <= 1);
            assertTrue("Correct but not optimized.", irSolver.getNbVars() <= 4);
            for (Variable var : irSolver.getVars()) {
                assertFalse("Correct but not optimized.",
                        (var instanceof FixedIntVarImpl && !(var instanceof FixedBoolVarImpl))
                        || var instanceof IntView);
            }

            Solver solver = new Solver();
            IntVar v0 = vars[0].toChocoVar(solver);
            IntVar v1 = vars[1].toChocoVar(solver);
            IntVar v2 = vars[2].toChocoVar(solver);
            IntVar sum = VF.enumerated("sum", v0.getLB() + v1.getLB(), v0.getUB() + v1.getUB(), solver);
            solver.post(ICF.sum(new IntVar[]{v0, v1}, sum));
            solver.post(left
                    ? ICF.arithm(sum, "<", v2)
                    : ICF.arithm(v2, "<", sum));
            assertEquals(randomizeStrategy(solver).findAllSolutions(), count);
        }
    }

    @Test(timeout = 60000)
    public void testLessThanEqualXYC() {
        for (int i = 0; i < 20; i++) {
            boolean left = nextBool();
            IrModule module = new IrModule();
            Term[] vars = new Term[]{randFixedTerm(), randTerm(), randTerm()};
            Util.shuffle(vars, rand);
            IrIntExpr add = Irs.add(vars[0].toIrExpr(), vars[1].toIrExpr());
            module.addVariables(vars[0].getIrVar(), vars[1].getIrVar(), vars[2].getIrVar());
            module.addConstraint(left
                    ? Irs.lessThanEqual(add, vars[2].toIrExpr())
                    : Irs.lessThanEqual(vars[2].toIrExpr(), add));

            Solver irSolver = new Solver();
            IrSolutionMap map = IrCompiler.compile(module, irSolver);
            int count = 0;
            if (randomizeStrategy(irSolver).findSolution()) {
                do {
                    assertTrue(left
                            ? vars[0].getValue(map) + vars[1].getValue(map) <= vars[2].getValue(map)
                            : vars[2].getValue(map) <= vars[0].getValue(map) + vars[1].getValue(map));
                    count++;
                } while (irSolver.nextSolution());
            }
            assertTrue("Correct but not optimized.", irSolver.getNbCstrs() <= 1);
            assertTrue("Correct but not optimized.", irSolver.getNbVars() <= 4);
            for (Variable var : irSolver.getVars()) {
                assertFalse("Correct but not optimized.",
                        (var instanceof FixedIntVarImpl && !(var instanceof FixedBoolVarImpl))
                        || var instanceof IntView);
            }

            Solver solver = new Solver();
            IntVar v0 = vars[0].toChocoVar(solver);
            IntVar v1 = vars[1].toChocoVar(solver);
            IntVar v2 = vars[2].toChocoVar(solver);
            IntVar sum = VF.enumerated("sum", v0.getLB() + v1.getLB(), v0.getUB() + v1.getUB(), solver);
            solver.post(ICF.sum(new IntVar[]{v0, v1}, sum));
            solver.post(left
                    ? ICF.arithm(sum, "<=", v2)
                    : ICF.arithm(v2, "<=", sum));
            assertEquals(randomizeStrategy(solver).findAllSolutions(), count);
        }
    }

    private Term randTerm() {
        switch (nextInt(3)) {
            case 0:
                return addRandTerm(randIntTerm());
            case 1:
                return addRandTerm(randBoolTerm());
            case 2:
                return addRandTerm(randFixedTerm());
            default:
                throw new IllegalStateException();
        }
    }

    private Term randIntTerm() {
        return new IntTerm(randInt(-5, 5));
    }

    private BoolTerm randBoolTerm() {
        return new BoolTerm(randBool());
    }

    private FixedTerm randFixedTerm() {
        return new FixedTerm(nextIntBetween(-5, 5));
    }

    private Term addRandTerm(Term view) {
        switch (nextInt(10)) {
            case 0:
            case 1:
                return new MinusTerm(view);
            case 3:
            case 4:
                return new OffsetTerm(view, nextIntBetween(-5, 5));
            default:
                return view;
        }
    }

    private Term addRandTerm(BoolTerm bool) {
        if (nextBool()) {
            return addRandTerm(new NotTerm(bool));
        }
        return addRandTerm((Term) bool);
    }

    private static interface Term {

        IrIntExpr toIrExpr();

        IrIntVar getIrVar();

        IntVar toChocoVar(Solver solver);

        int getValue(IrSolutionMap map);
    }

    private static class IntTerm implements Term {

        private final IrIntVar var;

        IntTerm(IrIntVar var) {
            this.var = var;
        }

        @Override
        public IrIntExpr toIrExpr() {
            return var;
        }

        @Override
        public IrIntVar getIrVar() {
            return var;
        }

        @Override
        public IntVar toChocoVar(Solver solver) {
            return toIntVar(var, solver);
        }

        @Override
        public int getValue(IrSolutionMap map) {
            return map.getIntValue(var);
        }
    }

    private static class BoolTerm implements Term {

        private final IrBoolVar var;

        BoolTerm(IrBoolVar var) {
            this.var = var;
        }

        @Override
        public IrBoolExpr toIrExpr() {
            return var;
        }

        @Override
        public IrIntVar getIrVar() {
            return var;
        }

        @Override
        public BoolVar toChocoVar(Solver solver) {
            return toBoolVar(var, solver);
        }

        @Override
        public int getValue(IrSolutionMap map) {
            return map.getIntValue(var);
        }
    }

    private static class FixedTerm implements Term {

        private final int c;

        FixedTerm(int c) {
            this.c = c;
        }

        @Override
        public IrIntExpr toIrExpr() {
            return Irs.constant(c);
        }

        @Override
        public IrIntVar getIrVar() {
            return Irs.constant(c);
        }

        @Override
        public IntVar toChocoVar(Solver solver) {
            return VF.fixed(c, solver);
        }

        @Override
        public int getValue(IrSolutionMap map) {
            return c;
        }
    }

    private static class MinusTerm implements Term {

        private final Term view;

        MinusTerm(Term view) {
            this.view = view;
        }

        @Override
        public IrIntExpr toIrExpr() {
            return Irs.minus(view.toIrExpr());
        }

        @Override
        public IrIntVar getIrVar() {
            return view.getIrVar();
        }

        @Override
        public IntVar toChocoVar(Solver solver) {
            return VF.minus(view.toChocoVar(solver));
        }

        @Override
        public int getValue(IrSolutionMap map) {
            return -view.getValue(map);
        }
    }

    private static class OffsetTerm implements Term {

        private final Term view;
        private final int offset;

        OffsetTerm(Term view, int offset) {
            this.view = view;
            this.offset = offset;
        }

        @Override
        public IrIntExpr toIrExpr() {
            return Irs.add(view.toIrExpr(), offset);
        }

        @Override
        public IrIntVar getIrVar() {
            return view.getIrVar();
        }

        @Override
        public IntVar toChocoVar(Solver solver) {
            return VF.offset(view.toChocoVar(solver), offset);
        }

        @Override
        public int getValue(IrSolutionMap map) {
            return view.getValue(map) + offset;
        }
    }

    private static class NotTerm implements Term {

        private final Term view;

        NotTerm(Term view) {
            this.view = view;
        }

        @Override
        public IrIntExpr toIrExpr() {
            return Irs.not((IrBoolExpr) view.toIrExpr());
        }

        @Override
        public IrIntVar getIrVar() {
            return view.getIrVar();
        }

        @Override
        public IntVar toChocoVar(Solver solver) {
            return VF.not((BoolVar) view.toChocoVar(solver));
        }

        @Override
        public int getValue(IrSolutionMap map) {
            return 1 - view.getValue(map);
        }
    }
}
