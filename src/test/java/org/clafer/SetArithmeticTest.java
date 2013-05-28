package org.clafer;

import org.clafer.ast.AstAbstractClafer;
import org.clafer.ast.AstConcreteClafer;
import org.clafer.ast.AstModel;
import static org.clafer.ast.Asts.*;
import static org.clafer.ast.Asts.newModel;
import org.clafer.scope.Scope;
import org.clafer.compiler.ClaferCompiler;
import org.clafer.compiler.ClaferSolver;
import static org.junit.Assert.assertEquals;
import org.junit.Test;

/**
 *
 * @author jimmy
 */
public class SetArithmeticTest {

    /**
     * <pre>
     * abstract Feature
     *     Cost ->> integer
     * Backup : Feature 1..2
     * Firewall : Feature 1..2
     * [(Feature -- Firewall).Cost.ref = 1]
     * </pre>
     */
    @Test(timeout = 60000)
    public void testDifference() {
        AstModel model = newModel();

        AstAbstractClafer feature = model.addAbstractClafer("Feature");
        AstConcreteClafer cost = feature.addChild("Cost").withCard(1, 1).refTo(IntType);
        AstConcreteClafer backup = model.addChild("Backup").extending(feature).withCard(1, 2);
        AstConcreteClafer firewall = model.addChild("Firewall").extending(feature).withCard(1, 2);
        model.addConstraint(equal(joinRef(join(diff(global(backup), global(firewall)), cost)), constant(1)));

        ClaferSolver solver = ClaferCompiler.compile(model, Scope.defaultScope(4).intLow(-1).intHigh(1));
        assertEquals(24, solver.allInstances().length);
    }

    /**
     * <pre>
     * abstract Feature
     *     Cost ->> integer
     * Backup : Feature 1..2
     * Free -> Feature 1..2
     * [(Backup & Free.ref).Cost.ref = 0]
     * </pre>
     */
    @Test(timeout = 60000)
    public void testIntersection() {
        AstModel model = newModel();

        AstAbstractClafer feature = model.addAbstractClafer("Feature");
        AstConcreteClafer cost = feature.addChild("Cost").withCard(1, 1).refTo(IntType);
        AstConcreteClafer backup = model.addChild("Backup").extending(feature).withCard(1, 2);
        AstConcreteClafer free = model.addChild("Free").refToUnique(feature).withCard(1, 2);
        model.addConstraint(equal(joinRef(join(inter(global(backup), joinRef(global(free))), cost)), constant(0)));

        ClaferSolver solver = ClaferCompiler.compile(model, Scope.defaultScope(2).intLow(-1).intHigh(1));
        // Can be reduced with better symmetry breaking
        assertEquals(9, solver.allInstances().length);
    }

    /**
     * <pre>
     * abstract Feature
     *     Cost ->> integer
     * Backup : Feature 1..2
     * Firewall : Feature 1..2
     * [(Backup ++ Firewall).Cost.ref = 4]
     * </pre>
     */
    @Test(timeout = 60000)
    public void testUnion() {
        AstModel model = newModel();

        AstAbstractClafer feature = model.addAbstractClafer("Feature");
        AstConcreteClafer cost = feature.addChild("Cost").withCard(1, 1).refTo(IntType);
        AstConcreteClafer backup = model.addChild("Backup").extending(feature).withCard(1, 2);
        AstConcreteClafer firewall = model.addChild("Firewall").extending(feature).withCard(1, 2);
        model.addConstraint(equal(joinRef(join(union(global(backup), global(firewall)), cost)), constant(4)));

        ClaferSolver solver = ClaferCompiler.compile(model, Scope.defaultScope(4));
        assertEquals(4, solver.allInstances().length);
    }

    /**
     * <pre>
     * Backup 1..2
     * Firewall 1..2
     * [|Backup ++ Firewall| = 3]
     * </pre>
     */
    @Test(timeout = 60000)
    public void testUnionOnClaferType() {
        AstModel model = newModel();

        AstConcreteClafer backup = model.addChild("Backup").withCard(1, 2);
        AstConcreteClafer firewall = model.addChild("Firewall").withCard(1, 2);
        model.addConstraint(equal(card(union(global(backup), global(firewall))), constant(3)));

        ClaferSolver solver = ClaferCompiler.compile(model, Scope.defaultScope(4));
        assertEquals(2, solver.allInstances().length);
    }

    /**
     * <pre>
     * abstract Feature
     * Backup : Feature 1..2
     * Firewall : Feature *
     * [(Backup ++ Firewall) = Backup]
     * </pre>
     */
    @Test(timeout = 60000)
    public void testEqualityOnSubTypes() {
        AstModel model = newModel();

        AstAbstractClafer feature = model.addAbstractClafer("Feature");
        AstConcreteClafer backup = model.addChild("Backup").extending(feature).withCard(1, 2);
        AstConcreteClafer firewall = model.addChild("Firewall").extending(feature);
        model.addConstraint(equal(union(global(backup), global(firewall)), global(backup)));

        ClaferSolver solver = ClaferCompiler.compile(model, Scope.defaultScope(4));
        assertEquals(2, solver.allInstances().length);
    }

    /**
     * <pre>
     * Backup 1..2
     * Feature ->> Backup 3..4
     * [Feature.ref = Backup]
     * </pre>
     */
    @Test(timeout = 60000)
    public void testEqualityOnRefs() {
        AstModel model = newModel();

        AstConcreteClafer backup = model.addChild("Backup").withCard(1, 2);
        AstConcreteClafer feature = model.addChild("Feature").withCard(3, 4).refTo(backup);
        model.addConstraint(equal(joinRef(global(feature)), global(backup)));

        ClaferSolver solver = ClaferCompiler.compile(model, Scope.defaultScope(4));
        // Assuming no reference symmetry breaking.
        assertEquals(22, solver.allInstances().length);
    }

    /**
     * <pre>
     * Feature
     *     Cost ->> Int 2
     *     Performance ->> Int 2..3
     *     Frugal ?
     *     [(if Frugal then this.Cost.ref else this.Performance.ref) &lt; 2]
     * </pre>
     */
    @Test(timeout = 60000)
    public void testTernary() {
        /*
         * import Control.Monad
         * import Data.List
         *
         * solutions = do
         *     cost <- sequence $ replicate 2 [0..2]
         *     numPerformance <- [2..3]
         *     performance <- sequence $ replicate numPerformance [0..2]
         *     frugal <- [True, False]
         *     guard $ sum (if nub frugal then nub cost else performance) < 2
         *     return (cost, performance, frugal)
         */
        AstModel model = newModel();

        AstConcreteClafer feature = model.addChild("Feature").withCard(1, 1);
        AstConcreteClafer cost = feature.addChild("Cost").withCard(2, 2).refTo(IntType);
        AstConcreteClafer performance = feature.addChild("Performance").withCard(2, 3).refTo(IntType);
        AstConcreteClafer frugal = feature.addChild("Frugal").withCard(0, 1);
        feature.addConstraint(lessThan(ifThenElse(some(frugal),
                joinRef(join($this(), cost)), joinRef(join($this(), performance))), constant(2)));

        ClaferSolver solver = ClaferCompiler.compile(model, Scope.defaultScope(3).intLow(0).intHigh(2));
        assertEquals(252, solver.allInstances().length);
    }

    /**
     * <pre>
     * Feature
     *     Cost ->> Int 2
     *     Frugal ?
     *     [(if Frugal then this.Cost.ref else 0) &lt; 2]
     * </pre>
     */
    @Test(timeout = 60000)
    public void testTernaryWithConstant() {
        /*
         * import Control.Monad
         * import Data.List
         *
         * solutions = do
         *     cost <- sequence $ replicate 2 [0..2]
         *     frugal <- [True, False]
         *     guard $ sum (if frugal then nub cost else [0]) < 2
         *     return (cost, frugal)
         */
        AstModel model = newModel();

        AstConcreteClafer feature = model.addChild("Feature").withCard(1, 1);
        AstConcreteClafer cost = feature.addChild("Cost").withCard(2, 2).refTo(IntType);
        AstConcreteClafer frugal = feature.addChild("Frugal").withCard(0, 1);
        feature.addConstraint(lessThan(ifThenElse(some(frugal),
                joinRef(join($this(), cost)), constant(0)), constant(2)));

        ClaferSolver solver = ClaferCompiler.compile(model, Scope.defaultScope(3).intLow(0).intHigh(2));
        assertEquals(13, solver.allInstances().length);
    }
}
