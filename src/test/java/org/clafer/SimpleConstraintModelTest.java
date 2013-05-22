package org.clafer;

import org.clafer.ast.scope.Scope;
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
public class SimpleConstraintModelTest {

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

        ClaferSolver solver = ClaferCompiler.compile(model, Scope.defaultScope(2).toScope());
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

        ClaferSolver solver = ClaferCompiler.compile(model, Scope.defaultScope(3).toScope());
        assertEquals(6, solver.allInstances().length);
    }

    /**
     * Age ->> integer 2 [this.ref = 3]
     */
    @Test
    public void testFixedJoinRef() {
        AstModel model = newModel();

        AstConcreteClafer age = model.addChild("Age").withCard(2, 2).refTo(IntType);
        age.addConstraint(equal(joinRef($this()), constant(3)));

        ClaferSolver solver = ClaferCompiler.compile(model, Scope.defaultScope(2).toScope());
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

        ClaferSolver solver = ClaferCompiler.compile(model, Scope.defaultScope(2).toScope());
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

        ClaferSolver solver = ClaferCompiler.compile(model, Scope.defaultScope(2).toScope());
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
    public void testVariableJoinAndJoinRefOverAbstract() {
        AstModel model = newModel();

        AstAbstractClafer feature = model.addAbstractClafer("Feature");
        AstConcreteClafer cost = feature.addChild("Cost").withCard(1, 1).refTo(IntType);
        AstConcreteClafer backup = model.addChild("Backup").withCard(2, 3).extending(feature);
        AstConcreteClafer firewall = model.addChild("Firewall").withCard(0, 1).extending(feature);
        model.addConstraint(equal(joinRef(join(global(backup), cost)), constant(3)));
        firewall.addConstraint(equal(joinRef(join($this(), cost)), constant(5)));

        ClaferSolver solver = ClaferCompiler.compile(model, Scope.defaultScope(4).toScope());
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
    public void testVariableJoinAndJoinRefOverMultipleAbstract() {
        AstModel model = newModel();

        AstAbstractClafer product = model.addAbstractClafer("Product");
        AstConcreteClafer cost = product.addChild("Cost").withCard(1, 1).refToUnique(IntType);
        AstAbstractClafer feature = model.addAbstractClafer("Feature").extending(product);
        AstConcreteClafer backup = model.addChild("Backup").withCard(2, 3).extending(feature);
        AstConcreteClafer firewall = model.addChild("Firewall").withCard(0, 1).extending(feature);
        model.addConstraint(equal(joinRef(join(global(backup), cost)), constant(3)));
        firewall.addConstraint(equal(joinRef(join($this(), cost)), constant(5)));

        ClaferSolver solver = ClaferCompiler.compile(model, Scope.defaultScope(4).toScope());
        assertEquals(4, solver.allInstances().length);
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

        ClaferSolver solver = ClaferCompiler.compile(model, Scope.defaultScope(2).toScope());
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

        ClaferSolver solver = ClaferCompiler.compile(model, Scope.defaultScope(3).toScope());
        assertEquals(2, solver.allInstances().length);
    }
}
