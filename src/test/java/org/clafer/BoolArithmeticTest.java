package org.clafer;

import org.clafer.ast.AstConcreteClafer;
import org.clafer.ast.AstModel;
import static org.clafer.ast.Asts.*;
import org.clafer.scope.Scope;
import org.clafer.compiler.ClaferCompiler;
import org.clafer.compiler.ClaferSolver;
import static org.junit.Assert.assertEquals;
import org.junit.Test;

/**
 *
 * @author jimmy
 */
public class BoolArithmeticTest {

    /**
     * <pre>
     * A -> integer
     * B -> integer
     * C ?
     * [some C => (A.ref = 1 && B.ref = 2)]
     * </pre>
     */
    @Test(timeout = 60000)
    public void testAnd() {
        /*
         * import Control.Monad
         *
         * solutions = do
         *     a <- [-2..2]
         *     b <- [-2..2]
         *     c <- [True, False]
         *     when c $ guard $ a == 1 && b == 2
         *     return (a, b, c)
         */
        AstModel model = newModel();

        AstConcreteClafer a = model.addChild("A").withCard(1, 1).refTo(IntType);
        AstConcreteClafer b = model.addChild("B").withCard(1, 1).refTo(IntType);
        AstConcreteClafer c = model.addChild("C").withCard(0, 1);
        model.addConstraint(implies(some(global(c)), and(
                equal(joinRef(global(a)), constant(1)), equal(joinRef(global(b)), constant(2)))));

        ClaferSolver solver = ClaferCompiler.compile(model, Scope.defaultScope(1).intLow(-2).intHigh(2).toScope());
        assertEquals(26, solver.allInstances().length);
    }

    /**
     * <pre>
     * A -> integer
     * B -> integer
     * C ?
     * [some C => (A.ref = 1 &lt;=&gt; B.ref = 2)]
     * </pre>
     */
    @Test(timeout = 60000)
    public void testIfOnlyIf() {
        /*
         * import Control.Monad
         *
         * solutions = do
         *     a <- [-2..2]
         *     b <- [-2..2]
         *     c <- [True, False]
         *     when c $ guard $ (a == 1) == (b == 2)
         *     return (a, b, c)
         */
        AstModel model = newModel();

        AstConcreteClafer a = model.addChild("A").withCard(1, 1).refTo(IntType);
        AstConcreteClafer b = model.addChild("B").withCard(1, 1).refTo(IntType);
        AstConcreteClafer c = model.addChild("C").withCard(0, 1);
        model.addConstraint(implies(some(global(c)), ifOnlyIf(
                equal(joinRef(global(a)), constant(1)), equal(joinRef(global(b)), constant(2)))));

        ClaferSolver solver = ClaferCompiler.compile(model, Scope.defaultScope(1).intLow(-2).intHigh(2).toScope());
        assertEquals(42, solver.allInstances().length);
    }

    /**
     * <pre>
     * A -> integer
     * B -> integer
     * C ?
     * [some C => (A.ref = 1 =&gt; B.ref = 2)]
     * </pre>
     */
    @Test(timeout = 60000)
    public void testImplies() {
        /*
         * import Control.Monad
         *
         * solutions = do
         *     a <- [-2..2]
         *     b <- [-2..2]
         *     c <- [True, False]
         *     when c $ guard $ (a == 1) `implies` (b == 2)
         *     return (a, b, c)
         *     where
         *         implies True False = False
         *         implies _ _ = True
         */
        AstModel model = newModel();

        AstConcreteClafer a = model.addChild("A").withCard(1, 1).refTo(IntType);
        AstConcreteClafer b = model.addChild("B").withCard(1, 1).refTo(IntType);
        AstConcreteClafer c = model.addChild("C").withCard(0, 1);
        model.addConstraint(implies(some(global(c)), implies(
                equal(joinRef(global(a)), constant(1)), equal(joinRef(global(b)), constant(2)))));

        ClaferSolver solver = ClaferCompiler.compile(model, Scope.defaultScope(1).intLow(-2).intHigh(2).toScope());
        assertEquals(46, solver.allInstances().length);
    }

    /**
     * <pre>
     * A -> integer
     * B -> integer
     * C ?
     * [some C => (A.ref = 1 || B.ref = 2)]
     * </pre>
     */
    @Test(timeout = 60000)
    public void testOr() {
        /*
         * import Control.Monad
         *
         * solutions = do
         *     a <- [-2..2]
         *     b <- [-2..2]
         *     c <- [True, False]
         *     when c $ guard $ (a == 1) || (b == 2)
         *     return (a, b, c)
         */
        AstModel model = newModel();

        AstConcreteClafer a = model.addChild("A").withCard(1, 1).refTo(IntType);
        AstConcreteClafer b = model.addChild("B").withCard(1, 1).refTo(IntType);
        AstConcreteClafer c = model.addChild("C").withCard(0, 1);
        model.addConstraint(implies(some(global(c)), or(
                equal(joinRef(global(a)), constant(1)), equal(joinRef(global(b)), constant(2)))));

        ClaferSolver solver = ClaferCompiler.compile(model, Scope.defaultScope(1).intLow(-2).intHigh(2).toScope());
        assertEquals(34, solver.allInstances().length);
    }

    /**
     * <pre>
     * A -> integer
     * B -> integer
     * C ?
     * [some C => (A.ref = 1 xor B.ref = 2)]
     * </pre>
     */
    @Test(timeout = 60000)
    public void testXor() {
        /*
         * import Control.Monad
         *
         * solutions = do
         *     a <- [-2..2]
         *     b <- [-2..2]
         *     c <- [True, False]
         *     when c $ guard $ (a == 1) /= (b == 2)
         *     return (a, b, c)
         */
        AstModel model = newModel();

        AstConcreteClafer a = model.addChild("A").withCard(1, 1).refTo(IntType);
        AstConcreteClafer b = model.addChild("B").withCard(1, 1).refTo(IntType);
        AstConcreteClafer c = model.addChild("C").withCard(0, 1);
        model.addConstraint(implies(some(global(c)), xor(
                equal(joinRef(global(a)), constant(1)), equal(joinRef(global(b)), constant(2)))));

        ClaferSolver solver = ClaferCompiler.compile(model, Scope.defaultScope(1).intLow(-2).intHigh(2).toScope());
        System.out.println(solver.solver);
        assertEquals(33, solver.allInstances().length);
    }
}
