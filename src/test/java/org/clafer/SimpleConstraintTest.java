package org.clafer;

import org.clafer.scope.Scope;
import org.clafer.ast.AstAbstractClafer;
import org.clafer.compiler.ClaferCompiler;
import org.clafer.ast.AstConcreteClafer;
import org.clafer.ast.AstModel;
import static org.clafer.ast.Asts.*;
import org.clafer.compiler.ClaferSolver;
import static org.junit.Assert.*;
import org.junit.Test;

/**
 *
 * @author jimmy
 */
public class SimpleConstraintTest {

    /**
     * <pre>
     * Age ->> integer 2
     * [Age.ref = 3]
     * </pre>
     */
    @Test
    public void testGlobal() {
        AstModel model = newModel();

        AstConcreteClafer age = model.addChild("Age").withCard(2, 2).refTo(IntType);
        model.addConstraint(equal(joinRef(global(age)), constant(3)));

        ClaferSolver solver = ClaferCompiler.compile(model, Scope.defaultScope(2));
        assertEquals(1, solver.allInstances().length);
    }

    /**
     * <pre>
     * Person
     *     Hand *
     *         Finger *
     *     [#this.Hand.Finger = 3]
     * </pre>
     */
    @Test
    public void testVariableJoin() {
        AstModel model = newModel();

        AstConcreteClafer person = model.addChild("Person").withCard(1, 1);
        AstConcreteClafer hand = person.addChild("Hand");
        AstConcreteClafer finger = hand.addChild("Finger");
        person.addConstraint(equal(card(join(join($this(), hand), finger)), constant(3)));

        ClaferSolver solver = ClaferCompiler.compile(model, Scope.defaultScope(3));
        assertEquals(6, solver.allInstances().length);
    }

    /**
     * <pre>
     * Age ->> integer 2
     *     [this.ref = 3]
     * </pre>
     */
    @Test
    public void testFixedJoinRef() {
        AstModel model = newModel();

        AstConcreteClafer age = model.addChild("Age").withCard(2, 2).refTo(IntType);
        age.addConstraint(equal(joinRef($this()), constant(3)));

        ClaferSolver solver = ClaferCompiler.compile(model, Scope.defaultScope(2));
        assertEquals(1, solver.allInstances().length);
    }

    /**
     * <pre>
     * abstract Feature
     *     Cost -> integer
     * Backup : Feature ?
     *     [this.Cost.ref = 3]
     * Firewall : Feature ?
     *     [this.Cost.ref = 5]
     * </pre>
     */
    @Test
    public void testFixedJoinAndJoinRefOverAbstract() {
        AstModel model = newModel();

        AstAbstractClafer feature = model.addAbstractClafer("Feature");
        AstConcreteClafer cost = feature.addChild("Cost").withCard(1, 1).refToUnique(IntType);
        AstConcreteClafer backup = model.addChild("Backup").withCard(0, 1).extending(feature);
        AstConcreteClafer firewall = model.addChild("Firewall").withCard(0, 1).extending(feature);
        backup.addConstraint(equal(joinRef(join($this(), cost)), constant(3)));
        firewall.addConstraint(equal(joinRef(join($this(), cost)), constant(5)));

        ClaferSolver solver = ClaferCompiler.compile(model, Scope.defaultScope(2));
        assertEquals(4, solver.allInstances().length);
    }

    /**
     * <pre>
     * abstract Feature
     *     Cost ->> integer
     * Backup : Feature 2..3
     * [Backup.Cost.ref = 3]
     * Firewall : Feature ?
     *     [this.Cost.ref = 5]
     * </pre>
     */
    @Test
    public void testMaybeJoinAndJoinRefOverAbstract() {
        AstModel model = newModel();

        AstAbstractClafer feature = model.addAbstractClafer("Feature");
        AstConcreteClafer cost = feature.addChild("Cost").withCard(1, 1).refTo(IntType);
        AstConcreteClafer backup = model.addChild("Backup").withCard(2, 3).extending(feature);
        AstConcreteClafer firewall = model.addChild("Firewall").withCard(0, 1).extending(feature);
        model.addConstraint(equal(joinRef(join(global(backup), cost)), constant(3)));
        firewall.addConstraint(equal(joinRef(join($this(), cost)), constant(5)));

        ClaferSolver solver = ClaferCompiler.compile(model, Scope.defaultScope(4));
        assertEquals(4, solver.allInstances().length);
    }

    /**
     * <pre>
     * abstract Product
     *     Cost -> integer
     * abstract Feature : Product
     * Backup : Feature ?
     *     [this.Cost.ref = 3]
     * Firewall : Feature ?
     *     [this.Cost.ref = 5]
     * </pre>
     */
    @Test
    public void testFixedJoinAndJoinRefOverMultipleAbstract() {
        AstModel model = newModel();

        AstAbstractClafer product = model.addAbstractClafer("Product");
        AstConcreteClafer cost = product.addChild("Cost").withCard(1, 1).refToUnique(IntType);
        AstAbstractClafer feature = model.addAbstractClafer("Feature").extending(product);
        AstConcreteClafer backup = model.addChild("Backup").withCard(0, 1).extending(feature);
        AstConcreteClafer firewall = model.addChild("Firewall").withCard(0, 1).extending(feature);
        backup.addConstraint(equal(joinRef(join($this(), cost)), constant(3)));
        firewall.addConstraint(equal(joinRef(join($this(), cost)), constant(5)));

        ClaferSolver solver = ClaferCompiler.compile(model, Scope.defaultScope(2));
        assertEquals(4, solver.allInstances().length);
    }

    /**
     * <pre>
     * abstract Product
     *     Cost ->> integer
     * abstract Feature : Product
     * Backup : Feature 2..3
     * [Backup.Cost.ref = 3]
     * Firewall : Feature ?
     *     [this.Cost.ref = 5]
     * </pre>
     */
    @Test
    public void testMaybeJoinAndJoinRefOverMultipleAbstract() {
        AstModel model = newModel();

        AstAbstractClafer product = model.addAbstractClafer("Product");
        AstConcreteClafer cost = product.addChild("Cost").withCard(1, 1).refToUnique(IntType);
        AstAbstractClafer feature = model.addAbstractClafer("Feature").extending(product);
        AstConcreteClafer backup = model.addChild("Backup").withCard(2, 3).extending(feature);
        AstConcreteClafer firewall = model.addChild("Firewall").withCard(0, 1).extending(feature);
        model.addConstraint(equal(joinRef(join(global(backup), cost)), constant(3)));
        firewall.addConstraint(equal(joinRef(join($this(), cost)), constant(5)));

        ClaferSolver solver = ClaferCompiler.compile(model, Scope.defaultScope(4));
        assertEquals(4, solver.allInstances().length);
    }

    /**
     * <pre>
     * abstract Product
     *     Cost ->> integer
     * abstract Feature : Product
     * abstract Service : Product
     * TV : Feature
     *     [this.Cost.ref = 2]
     * Internet : Service
     *     [this.Cost.ref = 2]
     * Computer
     *     Software : Feature 3..4
     *     [this.Software.Cost.ref = 3]
     * </pre>
     */
    @Test
    public void testVariableJoinAndJoinRefOverMultipleAbstract() {
        AstModel model = newModel();

        AstAbstractClafer product = model.addAbstractClafer("Product");
        AstConcreteClafer cost = product.addChild("Cost").withCard(1, 1).refTo(IntType);
        AstAbstractClafer feature = model.addAbstractClafer("Feature").extending(product);
        AstAbstractClafer serivce = model.addAbstractClafer("Service").extending(product);
        AstConcreteClafer tv = model.addChild("TV").withCard(1, 1).extending(feature);
        AstConcreteClafer internet = model.addChild("Internet").withCard(1, 1).extending(serivce);
        tv.addConstraint(equal(joinRef(join($this(), cost)), constant(2)));
        internet.addConstraint(equal(joinRef(join($this(), cost)), constant(2)));
        AstConcreteClafer computer = model.addChild("Computer").withCard(1, 1);
        AstConcreteClafer software = computer.addChild("Software").withCard(3, 4).extending(feature);
        computer.addConstraint(equal(joinRef(join(join($this(), software), cost)), constant(3)));

        ClaferSolver solver = ClaferCompiler.compile(model, Scope.defaultScope(6));
        assertEquals(2, solver.allInstances().length);
    }

    /**
     * <pre>
     * Feature ?
     *     Cost -> integer ?
     *     [this.Cost.ref = 3]
     * </pre>
     */
    @Test
    public void testMaybeJoinRef() {
        AstModel model = newModel();

        AstConcreteClafer feature = model.addChild("Feature").withCard(0, 1);
        AstConcreteClafer cost = feature.addChild("Cost").withCard(0, 1).refToUnique(IntType);
        feature.addConstraint(equal(joinRef(join($this(), cost)), constant(3)));

        ClaferSolver solver = ClaferCompiler.compile(model, Scope.defaultScope(2));
        assertEquals(2, solver.allInstances().length);
    }

    /**
     * <pre>
     * Feature
     *     Cost ->> integer 2..3
     *     [this.Cost.ref = 5]
     * </pre>
     */
    @Test
    public void testJoinRefSingleValue() {
        AstModel model = newModel();

        AstConcreteClafer feature = model.addChild("Feature").withCard(1, 1);
        AstConcreteClafer cost = feature.addChild("Cost").withCard(2, 3).refTo(IntType);
        feature.addConstraint(equal(joinRef(join($this(), cost)), constant(5)));

        ClaferSolver solver = ClaferCompiler.compile(model, Scope.defaultScope(3));
        assertEquals(2, solver.allInstances().length);
    }

    /**
     * <pre>
     * abstract Feature ->> integer
     * Backup : Feature 2..3
     * [Backup.Feature.ref = 4]
     * FireWall : Feature ?
     *     [this.ref = 5]
     * </pre>
     */
    @Test
    public void testVariableJoinAbstractRef() {
        AstModel model = newModel();

        AstAbstractClafer feature = model.addAbstractClafer("Feature").refTo(IntType);
        AstConcreteClafer backup = model.addChild("Backup").withCard(2, 3).extending(feature);
        AstConcreteClafer firewall = model.addChild("Firewall").withCard(0, 1).extending(feature);
        model.addConstraint(equal(joinRef(global(backup)), constant(3)));
        firewall.addConstraint(equal(joinRef($this()), constant(5)));

        ClaferSolver solver = ClaferCompiler.compile(model, Scope.defaultScope(4));
        assertEquals(4, solver.allInstances().length);
    }

    /**
     * <pre>
     * A 2
     *     B ?
     *     C ?
     *         [some this.parent.B]
     * </pre>
     */
    @Test(timeout = 60000)
    public void testFixedJoinParent() {
        AstModel model = newModel();

        AstConcreteClafer a = model.addChild("A").withCard(2, 2);
        AstConcreteClafer b = a.addChild("B").withCard(0, 1);
        AstConcreteClafer c = a.addChild("C").withCard(0, 1);
        c.addConstraint(some(join(joinParent($this()), b)));

        ClaferSolver solver = ClaferCompiler.compile(model, Scope.defaultScope(4));
        // Due to symmetry breaking.
        assertEquals(6, solver.allInstances().length);
    }

    /**
     * <pre>
     * A 4
     *     B 0..2
     * [#B.parent = 3]
     * </pre>
     */
    @Test(timeout = 60000)
    public void testVariableJoinParent() {
        AstModel model = newModel();

        AstConcreteClafer a = model.addChild("A").withCard(4, 4);
        AstConcreteClafer b = a.addChild("B").withCard(0, 2);
        model.addConstraint(equal(card(joinParent(global(b))), constant(3)));

        ClaferSolver solver = ClaferCompiler.compile(model, Scope.defaultScope(8));
        // Due to symmetry breaking.
        assertEquals(4, solver.allInstances().length);
    }

    /**
     * <pre>
     * A 4
     *     B 0..2
     *     C ?
     * [#B.parent.C = 3]
     * </pre>
     */
    @Test(timeout = 60000)
    public void testVariableJoinParentAndJoin() {
        AstModel model = newModel();

        AstConcreteClafer a = model.addChild("A").withCard(4, 4);
        AstConcreteClafer b = a.addChild("B").withCard(0, 2);
        AstConcreteClafer c = a.addChild("C").withCard(0, 1);
        model.addConstraint(equal(card(join(joinParent(global(b)), c)), constant(3)));

        ClaferSolver solver = ClaferCompiler.compile(model, Scope.defaultScope(8));
        // Due to symmetry breaking.
        assertEquals(16, solver.allInstances().length);
    }

    /**
     * <pre>
     * A 1..2
     *     B 1..2
     *         C ->> integer 3..4
     *     [this.B.C.ref = 3]
     * </pre>
     */
    @Test(timeout = 60000)
    public void testVariableJoinJoinJoinRef() {
        /*
         * import Control.Monad
         * import Data.List
         * 
         * solutions = genA
         *     where
         *         genA = nub $ do
         *             a <- [1..2]
         *             map sort $ sequence $ replicate a genB
         *         genB = nub $ do
         *             b <- [1..2]
         *             map sort $ sequence $ replicate b genC
         *         genC = [3, 4]
         */
        AstModel model = newModel();

        AstConcreteClafer a = model.addChild("A").withCard(1, 2);
        AstConcreteClafer b = a.addChild("B").withCard(1, 2);
        AstConcreteClafer c = b.addChild("C").withCard(3, 4).refTo(IntType);
        a.addConstraint(equal(joinRef(join(join($this(), b), c)), constant(3)));

        ClaferSolver solver = ClaferCompiler.compile(model, Scope.defaultScope(24).intLow(-5).intHigh(5));
        // Due to symmetry breaking.
        assertEquals(20, solver.allInstances().length);
    }

    /**
     * <pre>
     * A -> integer 3..4
     * B -> integer 3..4
     * [this.A.ref = this.B.ref]
     * [this.A.ref > 5000]
     * </pre>
     */
    @Test(timeout = 60000)
    public void testJoinRefLargeDomain() {
        AstModel model = newModel();

        AstConcreteClafer a = model.addChild("A").refToUnique(IntType).withCard(3, 4);
        AstConcreteClafer b = model.addChild("B").refToUnique(IntType).withCard(3, 4);
        model.addConstraint(equal(joinRef(global(a)), joinRef(global(b))));
        model.addConstraint(greaterThan(joinRef(global(a)), constant(5000)));

        ClaferSolver solver = ClaferCompiler.compile(model, Scope.defaultScope(4).intLow(-1).intHigh(10000));
        assertTrue(solver.find());
    }

    /**
     * <pre>
     * abstract A
     * abstract B
     * C : B
     * </pre>
     */
    @Test(timeout = 60000)
    public void testUnusedAbstract() {
        AstModel model = newModel();

        AstAbstractClafer a = model.addAbstractClafer("A");
        AstAbstractClafer b = model.addAbstractClafer("B");
        AstConcreteClafer c = model.addChild("C").extending(b);

        ClaferSolver solver = ClaferCompiler.compile(model, Scope.set(c, 1));
        assertTrue(solver.find());
    }

    /**
     * <pre>
     * abstract A
     * abstract B
     * C : B
     *     D -> A ?
     * </pre>
     */
    @Test(timeout = 60000)
    public void testRefToUnusedAbstract() {
        AstModel model = newModel();

        AstAbstractClafer a = model.addAbstractClafer("A");
        AstAbstractClafer b = model.addAbstractClafer("B");
        AstConcreteClafer c = model.addChild("C").extending(b);
        AstConcreteClafer d = model.addChild("D").refTo(a).withCard(Optional);

        ClaferSolver solver = ClaferCompiler.compile(model, Scope.set(c, 1).set(d, 1));
        assertTrue(solver.find());
    }
}
