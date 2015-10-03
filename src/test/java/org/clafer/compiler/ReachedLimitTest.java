package org.clafer.compiler;

import org.clafer.ast.AstConcreteClafer;
import org.clafer.ast.AstModel;
import static org.clafer.ast.Asts.*;
import org.clafer.objective.Objective;
import org.clafer.scope.Scope;
import static org.junit.Assert.*;
import org.junit.Test;

/**
 *
 * @author jimmy
 */
public class ReachedLimitTest {

    @Test(timeout = 60000)
    public void testLimitTime() {
        AstModel model = newModel();

        model.addChild("A").refTo(IntType);
        model.addChild("B").refTo(IntType);
        model.addChild("C").refTo(IntType);
        model.addChild("D").refTo(IntType);
        model.addChild("E").refTo(IntType);
        model.addChild("F").refTo(IntType);
        model.addChild("G").refTo(IntType);
        model.addChild("H").refTo(IntType);
        model.addChild("I").refTo(IntType);
        model.addChild("J").refTo(IntType);

        ClaferSolver solver = ClaferCompiler.compile(model, Scope.defaultScope(10));
        long start = System.currentTimeMillis();
        solver.limitTime(1000);
        try {
            solver.allInstances();
            fail("Expected timeout.");
        } catch (ReachedLimitException rle) {
            assertTrue(System.currentTimeMillis() - start >= 1000);
        }
    }

    @Test(timeout = 60000)
    public void testLimitTimeSingleObjective() {
        AstModel model = newModel();

        AstConcreteClafer a = model.addChild("A").refTo(IntType);
        model.addChild("B").refTo(IntType);
        model.addChild("C").refTo(IntType);
        model.addChild("D").refTo(IntType);
        model.addChild("E").refTo(IntType);
        model.addChild("F").refTo(IntType);
        model.addChild("G").refTo(IntType);
        model.addChild("H").refTo(IntType);
        model.addChild("I").refTo(IntType);
        model.addChild("J").refTo(IntType);

        ClaferOptimizer solver = ClaferCompiler.compile(model, Scope.defaultScope(10), Objective.maximize(joinRef(a)));
        long start = System.currentTimeMillis();
        solver.limitTime(1000);
        try {
            solver.allInstances();
            fail("Expected timeout.");
        } catch (ReachedLimitException rle) {
            assertTrue(System.currentTimeMillis() - start >= 1000);
        }
    }

    @Test(timeout = 60000)
    public void testLimitTimeSingleObjectiveBestKnown() {
        AstModel model = newModel();

        AstConcreteClafer a = model.addChild("A").refTo(IntType);
        AstConcreteClafer b = model.addChild("B").refTo(IntType);
        AstConcreteClafer c = model.addChild("C").refToUnique(IntType);
        AstConcreteClafer d = model.addChild("D").refTo(IntType);
        AstConcreteClafer e = model.addChild("E").refToUnique(IntType);
        model.addConstraint(equal(add(joinRef(a), joinRef(b), minus(joinRef(c)), joinRef(d), minus(joinRef(e))), card(global(a))));

        ClaferOptimizer solver = ClaferCompiler.compile(model, Scope.defaultScope(10).intLow(-100).intHigh(100), Objective.maximize(joinRef(a)));
        long start = System.currentTimeMillis();
        solver.limitTime(1000);
        try {
            solver.find();
            fail("Expected timeout.");
        } catch (ReachedLimitBestKnownException rle) {
            assertTrue(System.currentTimeMillis() - start >= 1000);
            while (rle.find()) {
                assertEquals(1, rle.optimalValues().length);
            }
        }
    }

    @Test(timeout = 60000)
    public void testLimitTimeMultiObjective() {
        AstModel model = newModel();

        AstConcreteClafer a = model.addChild("A").refTo(IntType);
        AstConcreteClafer b = model.addChild("B").refTo(IntType);
        model.addChild("C").refTo(IntType);
        model.addChild("D").refTo(IntType);
        model.addChild("E").refTo(IntType);
        model.addChild("F").refTo(IntType);
        model.addChild("G").refTo(IntType);
        model.addChild("H").refTo(IntType);
        model.addChild("I").refTo(IntType);
        model.addChild("J").refTo(IntType);

        ClaferOptimizer solver = ClaferCompiler.compile(model, Scope.defaultScope(10),
                Objective.maximize(joinRef(a)), Objective.maximize(joinRef(b)));
        long start = System.currentTimeMillis();
        solver.limitTime(1000);
        try {
            solver.allInstances();
            fail("Expected timeout.");
        } catch (ReachedLimitException rle) {
            assertTrue(System.currentTimeMillis() - start >= 1000);
        }
    }

    @Test(timeout = 60000)
    public void testLimitTimeMultipleObjectiveBestKnown() {
        AstModel model = newModel();

        AstConcreteClafer a = model.addChild("A").refTo(IntType);
        AstConcreteClafer b = model.addChild("B").refTo(IntType);
        AstConcreteClafer c = model.addChild("C").refToUnique(IntType);
        AstConcreteClafer d = model.addChild("D").refTo(IntType);
        AstConcreteClafer e = model.addChild("E").refToUnique(IntType);
        model.addConstraint(equal(add(joinRef(a), joinRef(b), minus(joinRef(c)), joinRef(d), minus(joinRef(e))), card(global(a))));

        ClaferOptimizer solver = ClaferCompiler.compile(model, Scope.defaultScope(10),
                Objective.maximize(joinRef(a)), Objective.maximize(joinRef(b)));
        long start = System.currentTimeMillis();
        solver.limitTime(1000);
        try {
            solver.find();
            fail("Expected timeout.");
        } catch (ReachedLimitBestKnownException rle) {
            while (rle.find()) {
                assertTrue(System.currentTimeMillis() - start >= 1000);
                assertEquals(2, rle.optimalValues().length);
            }
        }
    }

    @Test(timeout = 60000)
    public void testLimitTimeMinUnsat() {
        AstModel model = newModel();

        AstConcreteClafer a = model.addChild("A").withCard(1).refTo(IntType);
        AstConcreteClafer b = model.addChild("B").withCard(1).refTo(IntType);
        AstConcreteClafer c = model.addChild("C").withCard(1).refTo(IntType);
        model.addConstraint(equal(
                mul(joinRef(a), joinRef(a), joinRef(a)),
                add(
                        mul(joinRef(b), joinRef(b), joinRef(b)),
                        mul(joinRef(c), joinRef(c), joinRef(c)))));

        ClaferUnsat solver = ClaferCompiler.compileUnsat(model, Scope.defaultScope(10).intLow(2).intHigh(100));
        long start = System.currentTimeMillis();
        solver.limitTime(1000);
        try {
            solver.minUnsat();
            fail("Expected timeout.");
        } catch (ReachedLimitException rle) {
            assertTrue(System.currentTimeMillis() - start >= 1000);
        }
    }

    @Test(timeout = 60000)
    public void testLimitTimeUnsatCore() {
        AstModel model = newModel();

        AstConcreteClafer a = model.addChild("A").withCard(1).refTo(IntType);
        AstConcreteClafer b = model.addChild("B").withCard(1).refTo(IntType);
        AstConcreteClafer c = model.addChild("C").withCard(1).refTo(IntType);
        model.addConstraint(equal(
                mul(joinRef(a), joinRef(a), joinRef(a)),
                add(
                        mul(joinRef(b), joinRef(b), joinRef(b)),
                        mul(joinRef(c), joinRef(c), joinRef(c)))));

        ClaferUnsat solver = ClaferCompiler.compileUnsat(model, Scope.defaultScope(10).intLow(2).intHigh(100));
        long start = System.currentTimeMillis();
        solver.limitTime(1000);
        try {
            solver.unsatCore();
            fail("Expected timeout.");
        } catch (ReachedLimitException rle) {
            assertTrue(System.currentTimeMillis() - start >= 1000);
        }
    }
}
