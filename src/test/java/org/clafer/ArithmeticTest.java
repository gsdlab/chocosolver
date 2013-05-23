package org.clafer;

import org.clafer.ast.AstConcreteClafer;
import org.clafer.ast.AstModel;
import static org.clafer.ast.Asts.*;
import org.clafer.ast.scope.Scope;
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
}
