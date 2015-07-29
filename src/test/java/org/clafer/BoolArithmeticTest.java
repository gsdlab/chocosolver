package org.clafer;

import org.clafer.ast.AstConcreteClafer;
import org.clafer.ast.AstModel;
import static org.clafer.ast.Asts.*;
import org.clafer.compiler.ClaferCompiler;
import org.clafer.compiler.ClaferSolver;
import org.clafer.instance.InstanceModel;
import org.clafer.scope.Scope;
import static org.junit.Assert.*;
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
        model.addConstraint(implies(some(c), and(
                equal(joinRef(a), constant(1)), equal(joinRef(b), constant(2)))));

        ClaferSolver solver = ClaferCompiler.compile(model, Scope.defaultScope(1).intLow(-2).intHigh(2));
        while (solver.find()) {
            InstanceModel instance = solver.instance();
            if (instance.getTopClafers(c).length > 0) {
                assertEquals(1, instance.getTopClafer(a).getRef());
                assertEquals(2, instance.getTopClafer(b).getRef());
            }
        }
        assertEquals(26, solver.instanceCount());
    }

    /**
     * <pre>
     * A -> integer
     * B -> integer
     * C ?
     * [some C => !(A.ref = 1 && B.ref = 2)]
     * </pre>
     */
    @Test(timeout = 60000)
    public void testNotAnd() {
        /*
         * import Control.Monad
         *
         * solutions = do
         *     a <- [-2..2]
         *     b <- [-2..2]
         *     c <- [True, False]
         *     when c $ guard $ not $ a == 1 && b == 2
         *     return (a, b, c)
         */
        AstModel model = newModel();

        AstConcreteClafer a = model.addChild("A").withCard(1, 1).refTo(IntType);
        AstConcreteClafer b = model.addChild("B").withCard(1, 1).refTo(IntType);
        AstConcreteClafer c = model.addChild("C").withCard(0, 1);
        model.addConstraint(implies(some(c), not(and(
                equal(joinRef(a), constant(1)), equal(joinRef(b), constant(2))))));

        ClaferSolver solver = ClaferCompiler.compile(model, Scope.defaultScope(1).intLow(-2).intHigh(2));
        while (solver.find()) {
            InstanceModel instance = solver.instance();
            if (instance.getTopClafers(c).length > 0) {
                assertFalse(instance.getTopClafer(a).getRef().equals(1) && instance.getTopClafer(b).getRef().equals(2));
            }
        }
        assertEquals(49, solver.instanceCount());
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
        model.addConstraint(implies(some(c), ifOnlyIf(
                equal(joinRef(a), constant(1)), equal(joinRef(b), constant(2)))));

        ClaferSolver solver = ClaferCompiler.compile(model, Scope.defaultScope(1).intLow(-2).intHigh(2));
        while (solver.find()) {
            InstanceModel instance = solver.instance();
            if (instance.getTopClafers(c).length > 0) {
                assertEquals(instance.getTopClafer(a).getRef().equals(1), instance.getTopClafer(b).getRef().equals(2));
            }
        }
        assertEquals(42, solver.instanceCount());
    }

    /**
     * <pre>
     * A -> integer
     * B -> integer
     * C ?
     * [some C => !(A.ref = 1 &lt;=&gt; B.ref = 2)]
     * </pre>
     */
    @Test(timeout = 60000)
    public void testNotIfOnlyIf() {
        /*
         * import Control.Monad
         *
         * solutions = do
         *     a <- [-2..2]
         *     b <- [-2..2]
         *     c <- [True, False]
         *     when c $ guard $ not $ (a == 1) == (b == 2)
         *     return (a, b, c)
         */
        AstModel model = newModel();

        AstConcreteClafer a = model.addChild("A").withCard(1, 1).refTo(IntType);
        AstConcreteClafer b = model.addChild("B").withCard(1, 1).refTo(IntType);
        AstConcreteClafer c = model.addChild("C").withCard(0, 1);
        model.addConstraint(implies(some(c), not(ifOnlyIf(
                equal(joinRef(a), constant(1)), equal(joinRef(b), constant(2))))));

        ClaferSolver solver = ClaferCompiler.compile(model, Scope.defaultScope(1).intLow(-2).intHigh(2));
        while (solver.find()) {
            InstanceModel instance = solver.instance();
            if (instance.getTopClafers(c).length > 0) {
                assertNotEquals(instance.getTopClafer(a).getRef().equals(1), instance.getTopClafer(b).getRef().equals(2));
            }
        }
        assertEquals(33, solver.instanceCount());
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
        model.addConstraint(implies(some(c), implies(
                equal(joinRef(a), constant(1)), equal(joinRef(b), constant(2)))));

        ClaferSolver solver = ClaferCompiler.compile(model, Scope.defaultScope(1).intLow(-2).intHigh(2));
        while (solver.find()) {
            InstanceModel instance = solver.instance();
            if (instance.getTopClafers(c).length > 0) {
                assertTrue(!instance.getTopClafer(a).getRef().equals(1) || instance.getTopClafer(b).getRef().equals(2));
            }
        }
        assertEquals(46, solver.instanceCount());
    }

    /**
     * <pre>
     * A -> integer
     * B -> integer
     * C ?
     * [some C => !(A.ref = 1 =&gt; B.ref = 2)]
     * </pre>
     */
    @Test(timeout = 60000)
    public void testNotImplies() {
        /*
         * import Control.Monad
         *
         * solutions = do
         *     a <- [-2..2]
         *     b <- [-2..2]
         *     c <- [True, False]
         *     when c $ guard $ not $ (a == 1) `implies` (b == 2)
         *     return (a, b, c)
         *     where
         *         implies True False = False
         *         implies _ _ = True
         */
        AstModel model = newModel();

        AstConcreteClafer a = model.addChild("A").withCard(1, 1).refTo(IntType);
        AstConcreteClafer b = model.addChild("B").withCard(1, 1).refTo(IntType);
        AstConcreteClafer c = model.addChild("C").withCard(0, 1);
        model.addConstraint(implies(some(c), not(implies(
                equal(joinRef(a), constant(1)), equal(joinRef(b), constant(2))))));

        ClaferSolver solver = ClaferCompiler.compile(model, Scope.defaultScope(1).intLow(-2).intHigh(2));
        while (solver.find()) {
            InstanceModel instance = solver.instance();
            if (instance.getTopClafers(c).length > 0) {
                assertTrue(instance.getTopClafer(a).getRef().equals(1));
                assertFalse(instance.getTopClafer(b).getRef().equals(2));
            }
        }
        assertEquals(29, solver.instanceCount());
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
        model.addConstraint(implies(some(c), or(
                equal(joinRef(a), constant(1)), equal(joinRef(b), constant(2)))));

        ClaferSolver solver = ClaferCompiler.compile(model, Scope.defaultScope(1).intLow(-2).intHigh(2));
        while (solver.find()) {
            InstanceModel instance = solver.instance();
            if (instance.getTopClafers(c).length > 0) {
                assertTrue(instance.getTopClafer(a).getRef().equals(1) || instance.getTopClafer(b).getRef().equals(2));
            }
        }
        assertEquals(34, solver.instanceCount());
    }

    /**
     * <pre>
     * A -> integer
     * B -> integer
     * C ?
     * [some C => !(A.ref = 1 || B.ref = 2)]
     * </pre>
     */
    @Test(timeout = 60000)
    public void testNotOr() {
        /*
         * import Control.Monad
         *
         * solutions = do
         *     a <- [-2..2]
         *     b <- [-2..2]
         *     c <- [True, False]
         *     when c $ guard $ not $ (a == 1) || (b == 2)
         *     return (a, b, c)
         */
        AstModel model = newModel();

        AstConcreteClafer a = model.addChild("A").withCard(1, 1).refTo(IntType);
        AstConcreteClafer b = model.addChild("B").withCard(1, 1).refTo(IntType);
        AstConcreteClafer c = model.addChild("C").withCard(0, 1);
        model.addConstraint(implies(some(c), not(or(
                equal(joinRef(a), constant(1)), equal(joinRef(b), constant(2))))));

        ClaferSolver solver = ClaferCompiler.compile(model, Scope.defaultScope(1).intLow(-2).intHigh(2));
        while (solver.find()) {
            InstanceModel instance = solver.instance();
            if (instance.getTopClafers(c).length > 0) {
                assertFalse(instance.getTopClafer(a).getRef().equals(1));
                assertFalse(instance.getTopClafer(b).getRef().equals(2));
            }
        }
        assertEquals(41, solver.instanceCount());
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
        model.addConstraint(implies(some(c), xor(
                equal(joinRef(a), constant(1)), equal(joinRef(b), constant(2)))));

        ClaferSolver solver = ClaferCompiler.compile(model, Scope.defaultScope(1).intLow(-2).intHigh(2));
        while (solver.find()) {
            InstanceModel instance = solver.instance();
            if (instance.getTopClafers(c).length > 0) {
                assertNotEquals(instance.getTopClafer(a).getRef().equals(1), instance.getTopClafer(b).getRef().equals(2));
            }
        }
        assertEquals(33, solver.instanceCount());
    }

    /**
     * <pre>
     * A -> integer
     * B -> integer
     * C ?
     * [some C => !(A.ref = 1 xor B.ref = 2)]
     * </pre>
     */
    @Test(timeout = 60000)
    public void testNotXor() {
        /*
         * import Control.Monad
         *
         * solutions = do
         *     a <- [-2..2]
         *     b <- [-2..2]
         *     c <- [True, False]
         *     when c $ guard $ not $ (a == 1) /= (b == 2)
         *     return (a, b, c)
         */
        AstModel model = newModel();

        AstConcreteClafer a = model.addChild("A").withCard(1, 1).refTo(IntType);
        AstConcreteClafer b = model.addChild("B").withCard(1, 1).refTo(IntType);
        AstConcreteClafer c = model.addChild("C").withCard(0, 1);
        model.addConstraint(implies(some(c), not(xor(
                equal(joinRef(a), constant(1)), equal(joinRef(b), constant(2))))));

        ClaferSolver solver = ClaferCompiler.compile(model, Scope.defaultScope(1).intLow(-2).intHigh(2));
        while (solver.find()) {
            InstanceModel instance = solver.instance();
            if (instance.getTopClafers(c).length > 0) {
                assertEquals(instance.getTopClafer(a).getRef().equals(1), instance.getTopClafer(b).getRef().equals(2));
            }
        }
        assertEquals(42, solver.instanceCount());
    }

    /**
     * <pre>
     * A -> integer
     * B -> integer
     * C ?
     * [if some C then A.ref = 1 else B.ref = 2]
     * </pre>
     */
    @Test(timeout = 60000)
    public void testIfThenElse() {
        /*
         * import Control.Monad
         * 
         * solutions = do
         *     a <- [-2..2]
         *     b <- [-2..2]
         *     c <- [True, False]
         *     guard $ if c then a == 1 else b == 1
         *     return (a, b, c)
         */
        AstModel model = newModel();

        AstConcreteClafer a = model.addChild("A").withCard(1, 1).refTo(IntType);
        AstConcreteClafer b = model.addChild("B").withCard(1, 1).refTo(IntType);
        AstConcreteClafer c = model.addChild("C").withCard(0, 1);
        model.addConstraint(ifThenElse(some(c),
                equal(joinRef(a), constant(1)),
                equal(joinRef(b), constant(2))));

        ClaferSolver solver = ClaferCompiler.compile(model, Scope.defaultScope(1).intLow(-2).intHigh(2));
        while (solver.find()) {
            InstanceModel instance = solver.instance();
            if (instance.getTopClafers(c).length > 0) {
                assertTrue(instance.getTopClafer(a).getRef().equals(1));
            } else {
                assertTrue(instance.getTopClafer(b).getRef().equals(2));
            }
        }
        assertEquals(10, solver.instanceCount());
    }

    /**
     * <pre>
     * A -> integer
     * B -> integer
     * C ?
     * [!(if some C then A.ref = 1 else B.ref = 2)]
     * </pre>
     */
    @Test(timeout = 60000)
    public void testNotIfThenElse() {
        /*
         * import Control.Monad
         * 
         * solutions = do
         *     a <- [-2..2]
         *     b <- [-2..2]
         *     c <- [True, False]
         *     guard $ not $ if c then a == 1 else b == 1
         *     return (a, b, c)
         */
        AstModel model = newModel();

        AstConcreteClafer a = model.addChild("A").withCard(1, 1).refTo(IntType);
        AstConcreteClafer b = model.addChild("B").withCard(1, 1).refTo(IntType);
        AstConcreteClafer c = model.addChild("C").withCard(0, 1);
        model.addConstraint(not(ifThenElse(some(c),
                equal(joinRef(a), constant(1)),
                equal(joinRef(b), constant(2)))));

        ClaferSolver solver = ClaferCompiler.compile(model, Scope.defaultScope(1).intLow(-2).intHigh(2));
        while (solver.find()) {
            InstanceModel instance = solver.instance();
            if (instance.getTopClafers(c).length > 0) {
                assertFalse(instance.getTopClafer(a).getRef().equals(1));
            } else {
                assertFalse(instance.getTopClafer(b).getRef().equals(2));
            }
        }
        assertEquals(40, solver.instanceCount());
    }

    /**
     * <pre>
     * A -> integer
     * B -> integer
     * C ?
     * [!(some C => (A.ref = 1 =&gt; B.ref = 2))]
     * </pre>
     */
    @Test(timeout = 60000)
    public void testNot() {
        /*
         * import Control.Monad
         *
         * solutions = do
         *     a <- [-2..2]
         *     b <- [-2..2]
         *     c <- [True, False]
         *     guard $ not (c `implies` ((a == 1) `implies` (b == 2)))
         *     return (a, b, c)
         *     where
         *         implies True False = False
         *         implies _ _ = True
         */
        AstModel model = newModel();

        AstConcreteClafer a = model.addChild("A").withCard(1, 1).refTo(IntType);
        AstConcreteClafer b = model.addChild("B").withCard(1, 1).refTo(IntType);
        AstConcreteClafer c = model.addChild("C").withCard(0, 1);
        model.addConstraint(not(implies(some(c), implies(
                equal(joinRef(a), constant(1)), equal(joinRef(b), constant(2))))));

        ClaferSolver solver = ClaferCompiler.compile(model, Scope.defaultScope(1).intLow(-2).intHigh(2));
        while (solver.find()) {
            InstanceModel instance = solver.instance();
            assertTrue(instance.getTopClafers(c).length > 0);
            assertTrue(instance.getTopClafer(a).getRef().equals(1));
            assertFalse(instance.getTopClafer(b).getRef().equals(2));
        }
        assertEquals(4, solver.instanceCount());
    }

    /**
     * <pre>
     * A -> integer
     * B -> integer
     * C ?
     * [!!(some C => (A.ref = 1 =&gt; B.ref = 2))]
     * </pre>
     */
    @Test(timeout = 60000)
    public void testNotNot() {
        /*
         * import Control.Monad
         *
         * solutions = do
         *     a <- [-2..2]
         *     b <- [-2..2]
         *     c <- [True, False]
         *     guard $ not $ not (c `implies` ((a == 1) `implies` (b == 2)))
         *     return (a, b, c)
         *     where
         *         implies True False = False
         *         implies _ _ = True
         */
        AstModel model = newModel();

        AstConcreteClafer a = model.addChild("A").withCard(1, 1).refTo(IntType);
        AstConcreteClafer b = model.addChild("B").withCard(1, 1).refTo(IntType);
        AstConcreteClafer c = model.addChild("C").withCard(0, 1);
        model.addConstraint(not(not(implies(some(c), implies(
                equal(joinRef(a), constant(1)), equal(joinRef(b), constant(2)))))));

        ClaferSolver solver = ClaferCompiler.compile(model, Scope.defaultScope(1).intLow(-2).intHigh(2));
        while (solver.find()) {
            InstanceModel instance = solver.instance();
            if (instance.getTopClafers(c).length > 0) {
                assertTrue(!instance.getTopClafer(a).getRef().equals(1) || instance.getTopClafer(b).getRef().equals(2));
            }
        }
        assertEquals(46, solver.instanceCount());
    }

    /**
     * <pre>
     * A ?
     * B ?
     * C ?
     * D ?
     * E ?
     * [(E xor ((A &lt;=&gt; B) && (A || C || D) && (B =&gt; C)))
     *     && (D || ((A &lt;=&gt; D) xor ((B =&gt; C) =&gt; E)))]
     * </pre>
     */
    @Test(timeout = 60000)
    public void testBooleanSpaghetti() {
        AstModel model = newModel();

        AstConcreteClafer a = model.addChild("A").withCard(0, 1);
        AstConcreteClafer b = model.addChild("B").withCard(0, 1);
        AstConcreteClafer c = model.addChild("C").withCard(0, 1);
        AstConcreteClafer d = model.addChild("D").withCard(0, 1);
        AstConcreteClafer e = model.addChild("E").withCard(0, 1);
        model.addConstraint(
                and(
                        xor(some(e), and(ifOnlyIf(some(a), some(b)), or(some(a), some(c), some(d)), implies(some(b), some(c)))),
                        or(some(d), xor(ifOnlyIf(some(a), some(d)), implies(implies(some(b), some(c)), some(e))))));

        ClaferSolver solver = ClaferCompiler.compile(model, Scope.defaultScope(1));
        assertEquals(12, solver.allInstances().length);
    }
}
