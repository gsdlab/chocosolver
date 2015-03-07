package org.clafer;

import org.clafer.ast.AstAbstractClafer;
import org.clafer.ast.AstConcreteClafer;
import org.clafer.ast.AstModel;
import static org.clafer.ast.Asts.*;
import org.clafer.compiler.ClaferCompiler;
import org.clafer.compiler.ClaferSolver;
import org.clafer.instance.InstanceClafer;
import org.clafer.instance.InstanceModel;
import org.clafer.scope.Scope;
import static org.junit.Assert.*;
import org.junit.Test;

/**
 *
 * @author jimmy
 */
public class ArithmeticTest {

    /**
     * <pre>
     * Feature
     *     Cost -> Int
     *     [this.Cost + 3 = 5]
     * </pre>
     */
    @Test(timeout = 60000)
    public void testAdd() {
        AstModel model = newModel();

        AstConcreteClafer feature = model.addChild("Feature").withCard(1, 1);
        AstConcreteClafer cost = feature.addChild("Cost").withCard(1, 1).refTo(IntType);
        feature.addConstraint(equal(add(joinRef(join($this(), cost)), constant(3)), constant(5)));

        ClaferSolver solver = ClaferCompiler.compile(model, Scope.defaultScope(3).intLow(-10).intHigh(10));
        int count = 0;
        while (solver.find()) {
            InstanceModel instance = solver.instance();
            for (InstanceClafer f : instance.getTopClafers(feature)) {
                for (InstanceClafer c : f.getChildren(cost)) {
                    assertEquals(2, c.getRef());
                }
            }
            count++;
        }
        assertEquals(1, count);
    }

    /**
     * <pre>
     * Feature
     *     Cost ->> Int 2
     *     [this.Cost + 3 = 5]
     * </pre>
     */
    @Test(timeout = 60000)
    public void testAddSumSet() {
        /*
         * import Control.Monad
         * 
         * solutions = do
         *     cost1 <- [-10 .. 10]
         *     cost2 <- [cost1 .. 10]
         *     -- Set sum
         *     let sum = if cost1 == cost2 then cost1 else cost1 + cost2
         *     guard $ sum + 3 == 5
         *     return (cost1, cost2)
         */
        AstModel model = newModel();

        AstConcreteClafer feature = model.addChild("Feature").withCard(1, 1);
        AstConcreteClafer cost = feature.addChild("Cost").withCard(2, 2).refTo(IntType);
        feature.addConstraint(equal(add(joinRef(join($this(), cost)), constant(3)), constant(5)));

        ClaferSolver solver = ClaferCompiler.compile(model, Scope.defaultScope(3).intLow(-10).intHigh(10));
        assertEquals(10, solver.allInstances().length);
    }

    /**
     * <pre>
     * Feature
     *     Cost ->> Int 2
     *     Profit ->> Int 2
     *     [this.Cost + 3 = this.Profit - 2]
     * </pre>
     */
    @Test(timeout = 60000)
    public void testAddSubSumSet() {
        /*
         * import Control.Monad
         * 
         * solutions = do
         *     cost1 <- [-3 .. 3]
         *     cost2 <- [cost1 .. 3]
         *     profit1 <- [-3 .. 3]
         *     profit2 <- [profit1 .. 3]
         *     -- Set sum
         *     let costSum = if cost1 == cost2 then cost1 else cost1 + cost2
         *     let profitSum = if profit1 == profit2 then profit1 else profit1 + profit2
         *     guard $ costSum + 3 == profitSum - 2
         *     return (cost1, cost2, profit1, profit2)
         */
        AstModel model = newModel();

        AstConcreteClafer feature = model.addChild("Feature").withCard(1, 1);
        AstConcreteClafer cost = feature.addChild("Cost").withCard(2, 2).refTo(IntType);
        AstConcreteClafer profit = feature.addChild("Profit").withCard(2, 2).refTo(IntType);
        feature.addConstraint(equal(add(joinRef(join($this(), cost)), constant(3)),
                sub(joinRef(join($this(), profit)), constant(2))));

        ClaferSolver solver = ClaferCompiler.compile(model, Scope.defaultScope(3).intLow(-3).intHigh(3));
        assertEquals(34, solver.allInstances().length);
    }

    /**
     * <pre>
     * A -> integer
     * B -> integer
     * [ A.ref * B.ref = 12]
     * </pre>
     */
    @Test(timeout = 60000)
    public void testMul() {
        /*
         * 1, 12
         * 2, 6
         * 3, 4
         * 4, 3
         * 6, 2
         * 12, 1
         * -1, -12
         * -2, -6
         * -3, -4
         * -4, -3
         * -6, -2
         * -12, -1
         */
        AstModel model = newModel();

        AstConcreteClafer a = model.addChild("A").withCard(1, 1).refTo(IntType);
        AstConcreteClafer b = model.addChild("B").withCard(1, 1).refTo(IntType);
        model.addConstraint(equal(mul(joinRef(global(a)), joinRef(global(b))), constant(12)));

        ClaferSolver solver = ClaferCompiler.compile(model, Scope.defaultScope(1));
        assertEquals(12, solver.allInstances().length);
    }

    /**
     * <pre>
     * A -> integer
     * B -> integer
     * [ 12 * A.ref = B.ref]
     * </pre>
     */
    @Test(timeout = 60000)
    public void testDiv() {
        AstModel model = newModel();

        AstConcreteClafer a = model.addChild("A").withCard(1, 1).refTo(IntType);
        AstConcreteClafer b = model.addChild("B").withCard(1, 1).refTo(IntType);
        model.addConstraint(equal(div(constant(12), joinRef(global(a))), joinRef(global(b))));

        ClaferSolver solver = ClaferCompiler.compile(model, Scope.defaultScope(1));
        assertEquals(32, solver.allInstances().length);
    }

    /**
     * <pre>
     * A -> integer
     * B -> integer
     * [ A.ref = -B.ref]
     * </pre>
     */
    @Test(timeout = 60000)
    public void testMinus() {
        AstModel model = newModel();

        AstConcreteClafer a = model.addChild("A").withCard(1, 1).refTo(IntType);
        AstConcreteClafer b = model.addChild("B").withCard(1, 1).refTo(IntType);
        model.addConstraint(equal(joinRef(global(a)), minus(joinRef(global(b)))));

        ClaferSolver solver = ClaferCompiler.compile(model, Scope.defaultScope(1).intLow(-3).intHigh(3));
        assertEquals(7, solver.allInstances().length);
    }

    /**
     * <pre>
     * A 2..3
     *     B ->> Int 2
     *     total -> int
     *     [this.total.ref = sum(this.B)]
     * </pre>
     */
    @Test(timeout = 60000)
    public void testSum() {
        /*
         * import Control.Monad
         * import Data.List
         *
         * solutions = genAs 2 ++ genAs 3
         *
         * genAs n = nub $ map sort $ replicateM n genA
         *
         * genA = do
         *     b1 <- [-2..2]
         *     b2 <- [b1..2]
         *     totalCost <- [-2..2]
         *     guard $ b1 + b2 == totalCost
         *     return (b1, b2, totalCost)
         */
        AstModel model = newModel();

        AstConcreteClafer a = model.addChild("A").withCard(2, 3);
        AstConcreteClafer b = a.addChild("B").refTo(IntType).withCard(2, 2);
        AstConcreteClafer total = a.addChild("total").refToUnique(IntType).withCard(Mandatory);
        a.addConstraint(equal(joinRef(join($this(), total)), sum(join($this(), b))));

        ClaferSolver solver = ClaferCompiler.compile(model, Scope.defaultScope(6).intLow(-2).intHigh(2));
        int count = 0;
        while (solver.find()) {
            for (InstanceClafer ai : solver.instance().getTopClafers(a)) {
                int sum = 0;
                for (InstanceClafer bi : ai.getChildren(b)) {
                    sum += (int) bi.getRef();
                }
                for (InstanceClafer ti : ai.getChildren(total)) {
                    assertEquals(sum, ti.getRef());
                }
            }
            count++;
        }
        assertEquals(352, count);
    }

    /**
     * <pre>
     * A 2
     *     B ->> Int 2..3
     *     total -> int
     *     [this.total.ref = product(this.B)]
     * </pre>
     */
    @Test(timeout = 60000)
    public void testProduct() {
        /*
         * import Control.Monad
         * import Data.List
         * 
         * solutions = genAs 2 ++ genAs 3
         *
         * genAs n = nub $ map sort $ replicateM n genA2B ++ replicateM n genA3B
         *
         * genA2B = do
         *     b1 <- [-2..2]
         *     b2 <- [b1..2]
         *     totalCost <- [-2..2]
         *     guard $ b1 * b2 == totalCost
         *     return ([b1, b2], totalCost)
         *
         * genA3B = do
         *     b1 <- [-2..2]
         *     b2 <- [b1..2]
         *     b3 <- [b2..2]
         *     totalCost <- [-2..2]
         *     guard $ b1 * b2 * b3 == totalCost
         *     return ([b1, b2, b3], totalCost)
         */
        AstModel model = newModel();

        AstConcreteClafer a = model.addChild("A").withCard(2, 3);
        AstConcreteClafer b = a.addChild("B").refTo(IntType).withCard(2, 3);
        AstConcreteClafer total = a.addChild("total").refToUnique(IntType).withCard(Mandatory);
        a.addConstraint(equal(joinRef(join($this(), total)), product(join($this(), b))));

        ClaferSolver solver = ClaferCompiler.compile(model, Scope.defaultScope(9).intLow(-2).intHigh(2));
        int count = 0;
        while (solver.find()) {
            for (InstanceClafer ai : solver.instance().getTopClafers(a)) {
                int product = 1;
                for (InstanceClafer bi : ai.getChildren(b)) {
                    product *= (int) bi.getRef();
                }
                for (InstanceClafer ti : ai.getChildren(total)) {
                    assertEquals(product, ti.getRef());
                }
            }
            count++;
        }
        // TODO: should be 3692 with perfect symmetry breaking
        assertEquals(9842, count);
    }

    /**
     * <pre>
     * Feature
     *     Cost -> Int
     * A : Feature 2
     *     [this.Cost.ref = 3]
     * B : Feature 2
     *     [this.Cost.ref = 2]
     * C : Feature
     *     [this.Cost.ref = 3]
     *
     * totalCost -> Int
     *     [this.ref = sum (Feature.Cost)]
     * </pre>
     */
    @Test(timeout = 60000)
    public void testSumGlobal() {
        AstModel model = newModel();

        AstAbstractClafer feature = model.addAbstract("Feature");
        AstConcreteClafer cost = feature.addChild("Cost").refToUnique(IntType).withCard(Mandatory);
        AstConcreteClafer a = model.addChild("A").extending(feature).withCard(2, 2);
        a.addConstraint(equal(joinRef(join($this(), cost)), constant(3)));
        AstConcreteClafer b = model.addChild("B").extending(feature).withCard(2, 2);
        b.addConstraint(equal(joinRef(join($this(), cost)), constant(2)));
        AstConcreteClafer c = model.addChild("C").extending(feature).withCard(Mandatory);
        c.addConstraint(equal(joinRef(join($this(), cost)), constant(3)));
        AstConcreteClafer totalCost = model.addChild("totalCost").refToUnique(IntType).withCard(Mandatory);
        totalCost.addConstraint(equal(joinRef($this()), sum(join(global(feature), cost))));

        ClaferSolver solver = ClaferCompiler.compile(model, Scope.defaultScope(5));
        int count = 0;
        while (solver.find()) {
            for (InstanceClafer tc : solver.instance().getTopClafers(totalCost)) {
                assertEquals(13, tc.getRef());
            }
            count++;
        }
        assertEquals(1, count);
    }

    /**
     * <pre>
     * Feature
     *     Cost -> Int
     *     Performance -> Int
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
         *     cost <- [0..2]
         *     performance <- [0..2]
         *     frugal <- [True, False]
         *     guard $ (if frugal then cost else performance) < 2
         *     return (cost, performance, frugal)
         */
        AstModel model = newModel();

        AstConcreteClafer feature = model.addChild("Feature").withCard(Mandatory);
        AstConcreteClafer cost = feature.addChild("Cost").withCard(Mandatory).refTo(IntType);
        AstConcreteClafer performance = feature.addChild("Performance").withCard(Mandatory).refTo(IntType);
        AstConcreteClafer frugal = feature.addChild("Frugal").withCard(Optional);
        feature.addConstraint(lessThan(ifThenElse(some(frugal),
                joinRef(join($this(), cost)), joinRef(join($this(), performance))), constant(2)));

        ClaferSolver solver = ClaferCompiler.compile(model, Scope.defaultScope(3).intLow(0).intHigh(2));
        assertEquals(12, solver.allInstances().length);
    }

    /**
     * <pre>
     * abstract Feature
     *     Cost -> Int
     * Backup : Feature
     * Firewall : Feature
     * Frugal ?
     * [(if Frugal then Backup else Firewall).Cost.ref &lt; 2]
     * </pre>
     */
    @Test(timeout = 60000)
    public void testTernaryJoin() {
        /*
         * import Control.Monad
         * import Data.List
         *
         * solutions = do
         *     cost <- [0..2]
         *     performance <- [0..2]
         *     frugal <- [True, False]
         *     guard $ (if frugal then cost else performance) < 2
         *     return (cost, performance, frugal)
         */
        AstModel model = newModel();

        AstAbstractClafer feature = model.addAbstract("Feature");
        AstConcreteClafer cost = feature.addChild("Cost").withCard(Mandatory).refTo(IntType);
        AstConcreteClafer backup = model.addChild("Backup").extending(feature).withCard(Mandatory);
        AstConcreteClafer firewall = model.addChild("Fireall").extending(feature).withCard(Mandatory);
        AstConcreteClafer frugal = model.addChild("Frugal").withCard(Optional);
        feature.addConstraint(lessThan(
                joinRef(join(ifThenElse(some(frugal), global(backup), global(firewall)), cost)),
                constant(2)));

        ClaferSolver solver = ClaferCompiler.compile(model, Scope.defaultScope(3).intLow(0).intHigh(2));
        assertEquals(12, solver.allInstances().length);
    }
}
