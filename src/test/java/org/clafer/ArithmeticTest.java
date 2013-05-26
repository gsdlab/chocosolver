package org.clafer;

import org.clafer.ast.AstConcreteClafer;
import org.clafer.ast.AstModel;
import static org.clafer.ast.Asts.*;
import org.clafer.scope.Scope;
import org.clafer.compiler.ClaferCompiler;
import org.clafer.compiler.ClaferSolver;
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

        ClaferSolver solver = ClaferCompiler.compile(model, Scope.defaultScope(3).intLow(-10).intHigh(10).toScope());
        assertEquals(1, solver.allInstances().length);
    }

    /**
     * <pre>
     * Feature
     *     Cost -> Int 2
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
         *     cost2 <- [-10 .. 10]
         *     -- Set sum
         *     let sum = if cost1 == cost2 then cost1 else cost1 + cost2
         *     guard $ sum + 3 == 5
         *     return (cost1, cost2)
         */
        AstModel model = newModel();

        AstConcreteClafer feature = model.addChild("Feature").withCard(1, 1);
        AstConcreteClafer cost = feature.addChild("Cost").withCard(2, 2).refTo(IntType);
        feature.addConstraint(equal(add(joinRef(join($this(), cost)), constant(3)), constant(5)));

        ClaferSolver solver = ClaferCompiler.compile(model, Scope.defaultScope(3).intLow(-10).intHigh(10).toScope());
        assertEquals(19, solver.allInstances().length);
    }

    /**
     * <pre>
     * Feature
     *     Cost -> Int 2
     *     Profit -> Int 2
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
         *     cost2 <- [-3 .. 3]
         *     profit1 <- [-3 .. 3]
         *     profit2 <- [-3 .. 3]
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

        ClaferSolver solver = ClaferCompiler.compile(model, Scope.defaultScope(3).intLow(-3).intHigh(3).toScope());
        assertEquals(106, solver.allInstances().length);
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

        ClaferSolver solver = ClaferCompiler.compile(model, Scope.defaultScope(1).toScope());
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

        ClaferSolver solver = ClaferCompiler.compile(model, Scope.defaultScope(1).toScope());
        assertEquals(32, solver.allInstances().length);
    }

    /**
     * <pre>
     * Feature
     *     Cost -> Int 2
     *     [this.Cost > 5]
     * </pre>
     */
    @Test(timeout = 60000)
    public void testSumSetGreaterThan() {
        /*
         * import Control.Monad
         * 
         * solutions = do
         *     cost1 <- [-10 .. 10]
         *     cost2 <- [-10 .. 10]
         *     -- Set sum
         *     let sum = if cost1 == cost2 then cost1 else cost1 + cost2
         *     guard $ sum > 5
         *     return (cost1, cost2)
         */
        AstModel model = newModel();

        AstConcreteClafer feature = model.addChild("Feature").withCard(1, 1);
        AstConcreteClafer cost = feature.addChild("Cost").withCard(2, 2).refTo(IntType);
        feature.addConstraint(greaterThan(joinRef(join($this(), cost)), constant(5)));

        ClaferSolver solver = ClaferCompiler.compile(model, Scope.defaultScope(3).intLow(-10).intHigh(10).toScope());
        assertEquals(117, solver.allInstances().length);
    }
}
