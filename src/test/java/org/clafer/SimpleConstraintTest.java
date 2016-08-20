package org.clafer;

import org.clafer.ast.AstAbstractClafer;
import org.clafer.ast.AstConcreteClafer;
import org.clafer.ast.AstModel;
import static org.clafer.ast.Asts.$this;
import static org.clafer.ast.Asts.IntType;
import static org.clafer.ast.Asts.Mandatory;
import static org.clafer.ast.Asts.Optional;
import static org.clafer.ast.Asts.card;
import static org.clafer.ast.Asts.constant;
import static org.clafer.ast.Asts.equal;
import static org.clafer.ast.Asts.global;
import static org.clafer.ast.Asts.greaterThan;
import static org.clafer.ast.Asts.implies;
import static org.clafer.ast.Asts.in;
import static org.clafer.ast.Asts.join;
import static org.clafer.ast.Asts.joinParent;
import static org.clafer.ast.Asts.joinRef;
import static org.clafer.ast.Asts.lessThan;
import static org.clafer.ast.Asts.newModel;
import static org.clafer.ast.Asts.or;
import static org.clafer.ast.Asts.some;
import static org.clafer.ast.Asts.union;
import org.clafer.compiler.ClaferCompiler;
import org.clafer.compiler.ClaferSolver;
import org.clafer.scope.Scope;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
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
    @Test(timeout = 60000)
    public void testGlobal() {
        AstModel model = newModel();

        AstConcreteClafer age = model.addChild("Age").withCard(2, 2).refTo(IntType);
        model.addConstraint(equal(joinRef(age), constant(3)));

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
    @Test(timeout = 60000)
    public void testVariableJoin() {
        AstModel model = newModel();

        AstConcreteClafer person = model.addChild("Person").withCard(1, 1);
        AstConcreteClafer hand = person.addChild("Hand");
        AstConcreteClafer finger = hand.addChild("Finger");
        person.addConstraint(equal(card(join(join($this(), hand), finger)), constant(3)));

        ClaferSolver solver = ClaferCompiler.compile(model, Scope.defaultScope(3));
        assertEquals(3, solver.allInstances().length);
    }

    /**
     * <pre>
     * Age ->> integer 2
     *     [this.ref = 3]
     * </pre>
     */
    @Test(timeout = 60000)
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
    @Test(timeout = 60000)
    public void testFixedJoinAndJoinRefOverAbstract() {
        AstModel model = newModel();

        AstAbstractClafer feature = model.addAbstract("Feature");
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
    @Test(timeout = 60000)
    public void testMaybeJoinAndJoinRefOverAbstract() {
        AstModel model = newModel();

        AstAbstractClafer feature = model.addAbstract("Feature");
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
    @Test(timeout = 60000)
    public void testFixedJoinAndJoinRefOverMultipleAbstract() {
        AstModel model = newModel();

        AstAbstractClafer product = model.addAbstract("Product");
        AstConcreteClafer cost = product.addChild("Cost").withCard(1, 1).refToUnique(IntType);
        AstAbstractClafer feature = model.addAbstract("Feature").extending(product);
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
    @Test(timeout = 60000)
    public void testMaybeJoinAndJoinRefOverMultipleAbstract() {
        AstModel model = newModel();

        AstAbstractClafer product = model.addAbstract("Product");
        AstConcreteClafer cost = product.addChild("Cost").withCard(1, 1).refToUnique(IntType);
        AstAbstractClafer feature = model.addAbstract("Feature").extending(product);
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
    @Test(timeout = 60000)
    public void testVariableJoinAndJoinRefOverMultipleAbstract() {
        AstModel model = newModel();

        AstAbstractClafer product = model.addAbstract("Product");
        AstConcreteClafer cost = product.addChild("Cost").withCard(1, 1).refTo(IntType);
        AstAbstractClafer feature = model.addAbstract("Feature").extending(product);
        AstAbstractClafer serivce = model.addAbstract("Service").extending(product);
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
    @Test(timeout = 60000)
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
    @Test(timeout = 60000)
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
     * Feature
     *     Cost -> integer*
     *     [this.Cost.ref = 5 ++ 2 ++ 3 ++ 4]
     * </pre>
     */
    @Test(timeout = 60000)
    public void testJoinRefMultiValue() {
        AstModel model = newModel();

        AstConcreteClafer feature = model.addChild("Feature").withCard(1, 1);
        AstConcreteClafer cost = feature.addChild("Cost").refToUnique(IntType);
        feature.addConstraint(equal(joinRef(join($this(), cost)), union(union(union(constant(5), constant(2)), constant(3)), constant(4))));

        ClaferSolver solver = ClaferCompiler.compile(model, Scope.defaultScope(20));
        assertEquals(1, solver.allInstances().length);
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
    @Test(timeout = 60000)
    public void testVariableJoinAbstractRef() {
        AstModel model = newModel();

        AstAbstractClafer feature = model.addAbstract("Feature").refTo(IntType);
        AstConcreteClafer backup = model.addChild("Backup").withCard(2, 3).extending(feature);
        AstConcreteClafer firewall = model.addChild("Firewall").withCard(0, 1).extending(feature);
        model.addConstraint(equal(joinRef(backup), constant(3)));
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
    public void testVariableJoinParentJoin() {
        AstModel model = newModel();

        AstConcreteClafer a = model.addChild("A").withCard(4, 4);
        AstConcreteClafer b = a.addChild("B").withCard(0, 2);
        AstConcreteClafer c = a.addChild("C").withCard(Optional);
        model.addConstraint(equal(card(join(joinParent(global(b)), c)), constant(3)));

        ClaferSolver solver = ClaferCompiler.compile(model, Scope.defaultScope(8));
        // Due to symmetry breaking.
        assertEquals(16, solver.allInstances().length);
    }

    /**
     * <pre>
     * A 0..2
     *     B ?
     *     C ?
     *         D 0..2
     *             [some this.parent.parent.B]
     * </pre>
     */
    @Test(timeout = 60000)
    public void testVariableJoinParentJoinParentJoin() {
        AstModel model = newModel();

        AstConcreteClafer a = model.addChild("A").withCard(0, 2);
        AstConcreteClafer b = a.addChild("B").withCard(Optional);
        AstConcreteClafer c = a.addChild("C").withCard(Optional);
        AstConcreteClafer d = c.addChild("D").withCard(0, 2);
        d.addConstraint(some(join(joinParent(joinParent($this())), b)));

        ClaferSolver solver = ClaferCompiler.compile(model, Scope.defaultScope(2));
        // Due to symmetry breaking.
        assertEquals(26, solver.allInstances().length);
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
     * [A.ref = B.ref]
     * [A.ref > 5000]
     * </pre>
     */
    @Test(timeout = 60000)
    public void testJoinRefLargeDomain() {
        AstModel model = newModel();

        AstConcreteClafer a = model.addChild("A").refToUnique(IntType).withCard(3, 4);
        AstConcreteClafer b = model.addChild("B").refToUnique(IntType).withCard(3, 4);
        model.addConstraint(equal(joinRef(a), joinRef(b)));
        model.addConstraint(greaterThan(joinRef(a), constant(5000)));

        ClaferSolver solver = ClaferCompiler.compile(model, Scope.defaultScope(4).intLow(-1).intHigh(10000));
        assertTrue(solver.find());
    }

    /**
     * <pre>
     * abstract A
     *     B -> C *
     * abstract C *
     * D : A 2
     *     E : C
     *     F : C
     *     G : C
     *     [this.B.ref = E ++ F ++ C]
     * </pre>
     */
    @Test(timeout = 60000)
    public void testJoinRefStar() {
        AstModel model = newModel();

        AstAbstractClafer a = model.addAbstract("A");
        AstAbstractClafer c = model.addAbstract("C");
        AstConcreteClafer b = a.addChild("B").refToUnique(c);

        AstConcreteClafer d = model.addChild("D").extending(a).withCard(2, 2);
        AstConcreteClafer e = d.addChild("E").extending(c).withCard(Mandatory);
        AstConcreteClafer f = d.addChild("F").extending(c).withCard(Mandatory);
        AstConcreteClafer g = d.addChild("G").extending(c).withCard(Mandatory);
        d.addConstraint(equal(joinRef(join($this(), b)), union(union(join($this(), e), join($this(), f)), join($this(), g))));

        ClaferSolver solver = ClaferCompiler.compile(model, Scope.defaultScope(20));
        assertTrue(solver.find());
    }

    /**
     * <pre>
     * abstract A
     *     B -> C *
     * abstract C *
     * D : A 2
     *     E : C
     *     F : C
     *     G : C
     *     H ?
     *     I -> C *
     *     [this.I.ref = E ++ F ++ C]
     *     [some this.H => this.I.ref = this.B.ref]
     * </pre>
     */
    @Test(timeout = 60000)
    public void testJoinRefStarCondition() {
        AstModel model = newModel();

        AstAbstractClafer a = model.addAbstract("A");
        AstAbstractClafer c = model.addAbstract("C");
        AstConcreteClafer b = a.addChild("B").refToUnique(c);

        AstConcreteClafer d = model.addChild("D").extending(a).withCard(2, 2);
        AstConcreteClafer e = d.addChild("E").extending(c).withCard(Mandatory);
        AstConcreteClafer f = d.addChild("F").extending(c).withCard(Mandatory);
        AstConcreteClafer g = d.addChild("G").extending(c).withCard(Mandatory);
        AstConcreteClafer h = d.addChild("H").withCard(Optional);
        AstConcreteClafer i = d.addChild("I").refToUnique(c);
        d.addConstraint(equal(joinRef(join($this(), i)), union(union(join($this(), e), join($this(), f)), join($this(), g))));
        d.addConstraint(implies(some(join($this(), h)),
                equal(joinRef(join($this(), i)), joinRef(join($this(), b)))));

        ClaferSolver solver = ClaferCompiler.compile(model, Scope.defaultScope(20));
        assertTrue(solver.find());
    }

    /**
     * <pre>
     * abstract Person
     *     age -> int
     *     child -> Person *
     *         [this.ref.age.ref &lt; 6]
     *
     * Alice : Person
     *     [ this.age.ref = 6 ]
     * Bob: Person
     * </pre>
     */
    @Test(timeout = 60000)
    public void testPredicate() {
        AstModel model = newModel();

        AstAbstractClafer person = model.addAbstract("Person");
        AstConcreteClafer age = person.addChild("age").refToUnique(IntType).withCard(Mandatory);
        AstConcreteClafer child = person.addChild("child").refToUnique(person);
        child.addConstraint(lessThan(joinRef(join(joinRef($this()), age)),
                constant(6)));
        AstConcreteClafer alice = model.addChild("Alice").extending(person).withCard(Mandatory);
        alice.addConstraint(equal(joinRef(join($this(), age)), constant(6)));
        AstConcreteClafer bob = model.addChild("Bob").extending(person).withCard(Mandatory);

        ClaferSolver solver = ClaferCompiler.compile(model, Scope.defaultScope(10));
        assertTrue(solver.find());
    }

    /**
     * <pre>
     * [4 = 5]
     * </pre>
     */
    @Test(timeout = 60000)
    public void testConstantEqualUnsat() {
        AstModel model = newModel();

        model.addConstraint(equal(constant(4), constant(5)));

        ClaferSolver solver = ClaferCompiler.compile(model, Scope.defaultScope(1));
        assertFalse(solver.find());
    }

    /**
     * <pre>
     * A
     *     B *
     *         C *
     * D -> int
     * [ #(A.B) = D.ref ]
     * </pre>
     */
    @Test(timeout = 60000)
    public void testNoScopeJoin() {
        AstModel model = newModel();

        AstConcreteClafer a = model.addChild("A").withCard(Mandatory);
        AstConcreteClafer b = a.addChild("B");
        AstConcreteClafer c = b.addChild("C");
        AstConcreteClafer d = model.addChild("D").refToUnique(IntType).withCard(Mandatory);
        a.addConstraint(equal(card(join(join($this(), b), c)), joinRef(d)));

        ClaferSolver solver = ClaferCompiler.compile(model, Scope.defaultScope(1).setScope(b, 0));
        while (solver.find()) {
            assertEquals(0, solver.instance().getTopClafer(d).getRef());
        }
        assertEquals(1, solver.instanceCount());
    }

    /**
     * <pre>
     * abstract A
     *     B -> int
     *     [ this.B.ref = 0 || this.B.ref = 1 ]
     * X : A
     * Y : A
     * </pre>
     */
    @Test(timeout = 60000)
    public void testOrPartialInt() {
        AstModel model = newModel();

        AstAbstractClafer a = model.addAbstract("A");
        AstConcreteClafer b = a.addChild("B").refToUnique(IntType).withCard(Mandatory);
        AstConcreteClafer x = model.addChild("X").extending(a).withCard(Mandatory);
        AstConcreteClafer y = model.addChild("Y").extending(a).withCard(Mandatory);
        a.addConstraint(or(
                equal(joinRef(join($this(), b)), 0),
                equal(joinRef(join($this(), b)), 1)));

        ClaferSolver solver = ClaferCompiler.compile(model, Scope.defaultScope(2));
        assertEquals(4, solver.allInstances().length);
    }

    /**
     * <pre>
     * abstract Message
     *     abstract Content -> int
     * Mail : Message ?
     *     Letter : Content
     *     Gift : Content *
     *     [ 1 in this.Content.ref ]
     * </pre>
     */
    @Test(timeout = 60000)
    public void testNestedAbstract() {
        AstModel model = newModel();

        AstAbstractClafer message = model.addAbstract("Message");
        AstAbstractClafer content = message.addAbstractChild("Content").refToUnique(IntType);
        AstConcreteClafer mail = model.addChild("Mail").extending(message).withCard(Optional);
        AstConcreteClafer letter = mail.addChild("Letter").extending(content).withCard(Mandatory);
        AstConcreteClafer gift = mail.addChild("Gift").extending(content);
        mail.addConstraint(in(constant(1), joinRef(join($this(), content))));

        ClaferSolver solver = ClaferCompiler.compile(model, Scope.defaultScope(2).intLow(0).intHigh(1));
        assertEquals(7, solver.allInstances().length);
    }
}
