package org.clafer;

import org.clafer.ast.AstConcreteClafer;
import org.clafer.ast.AstModel;
import static org.clafer.ast.Asts.$this;
import static org.clafer.ast.Asts.IntType;
import static org.clafer.ast.Asts.constant;
import static org.clafer.ast.Asts.equal;
import static org.clafer.ast.Asts.greaterThan;
import static org.clafer.ast.Asts.greaterThanEqual;
import static org.clafer.ast.Asts.join;
import static org.clafer.ast.Asts.joinRef;
import static org.clafer.ast.Asts.lessThan;
import static org.clafer.ast.Asts.lessThanEqual;
import static org.clafer.ast.Asts.newModel;
import static org.clafer.ast.Asts.notEqual;
import org.clafer.compiler.ClaferCompiler;
import org.clafer.compiler.ClaferSolver;
import org.clafer.scope.Scope;
import static org.junit.Assert.assertEquals;
import org.junit.Test;

/**
 *
 * @author jimmy
 */
public class InequalityTest {

    /**
     * <pre>
     * Feature
     *     Cost -> Int 2..3
     *     Performance -> Int 2..3
     *     [this.Cost.ref = this.Performance.ref]
     * </pre>
     */
    @Test(timeout = 60000)
    public void testSetEqual() {
        /*
         * import Control.Monad
         * import Data.List
         *
         * isUnique [] = True
         * isUnique (x : xs) = x `notElem` xs && isUnique xs
         *
         * choose 0 _ = return []
         * choose _ [] = mzero
         * choose n (x:xs) =
         *     do
         *         xs' <- choose (n-1) (x:xs)
         *         return $ x : xs'
         *     `mplus` choose n xs 
         *    
         * solutions = do
         *     numCost <- [2..3]
         *     cost <- choose numCost [-1..1]
         *     guard $ isUnique cost
         *     numPerformance <- [2..3]
         *     performance <- choose numPerformance [-1..1]
         *     guard $ isUnique performance
         *     guard $ nub (sort cost) == nub (sort performance)
         *     return (cost, performance)
         */
        AstModel model = newModel();

        AstConcreteClafer feature = model.addChild("Feature").withCard(1, 1);
        AstConcreteClafer cost = feature.addChild("Cost").withCard(2, 3).refToUnique(IntType);
        AstConcreteClafer performance = feature.addChild("Performance").withCard(2, 3).refToUnique(IntType);
        feature.addConstraint(equal(joinRef(join($this(), cost)), joinRef(join($this(), performance))));

        ClaferSolver solver = ClaferCompiler.compile(model, Scope.defaultScope(3).intLow(-1).intHigh(1));
        assertEquals(4, solver.allInstances().length);
    }

    /**
     * <pre>
     * Feature
     *     Cost -> Int 2..3
     *     Performance -> Int 2..3
     *     [this.Cost.ref != this.Performance.ref]
     * </pre>
     */
    @Test(timeout = 60000)
    public void testSetNotEqual() {
        /* 
         * import Control.Monad
         * import Data.List
         * 
         * isUnique [] = True
         * isUnique (x : xs) = x `notElem` xs && isUnique xs
         * 
         * choose 0 _ = return []
         * choose _ [] = mzero
         * choose n (x:xs) =
         *     do
         *         xs' <- choose (n-1) xs
         *         return $ x : xs'
         *    `mplus` choose n xs 
         *
         * solutions = do
         *     numCost <- [2..3]
         *     cost <- choose numCost [-1..1]
         *     guard $ isUnique cost
         *     numPerformance <- [2..3]
         *     performance <- choose numPerformance [-1..1]
         *     guard $ isUnique performance
         *     guard $ nub (sort cost) /= nub (sort performance)
         *     return (cost, performance)
         */
        AstModel model = newModel();

        AstConcreteClafer feature = model.addChild("Feature").withCard(1, 1);
        AstConcreteClafer cost = feature.addChild("Cost").withCard(2, 3).refToUnique(IntType);
        AstConcreteClafer performance = feature.addChild("Performance").withCard(2, 3).refToUnique(IntType);
        feature.addConstraint(notEqual(joinRef(join($this(), cost)), joinRef(join($this(), performance))));

        ClaferSolver solver = ClaferCompiler.compile(model, Scope.defaultScope(3).intLow(-1).intHigh(1));
        assertEquals(12, solver.allInstances().length);
    }

    /**
     * <pre>
     * Feature
     *     Cost -> Int 2..3
     *     [this.Cost.ref &lt; 2]
     * </pre>
     */
    @Test(timeout = 60000)
    public void testSumSetLessThan() {
        /*
         * import Control.Monad
         * import Data.List
         *
         * isUnique [] = True
         * isUnique (x : xs) = x `notElem` xs && isUnique xs
         *
         * choose 0 _ = return []
         * choose _ [] = mzero
         * choose n (x:xs) =
         *     do
         *         xs' <- choose (n-1) (x:xs)
         *         return $ x : xs'
         *     `mplus` choose n xs 
         * 
         * solutions = do
         *     numCost <- [2..3]
         *     cost <- choose numCost [-3..3]
         *     guard $ isUnique cost
         *     guard $ sum cost < 2
         *     return cost
         */
        AstModel model = newModel();

        AstConcreteClafer feature = model.addChild("Feature").withCard(1, 1);
        AstConcreteClafer cost = feature.addChild("Cost").withCard(2, 3).refToUnique(IntType);
        feature.addConstraint(lessThan(joinRef(join($this(), cost)), constant(2)));

        ClaferSolver solver = ClaferCompiler.compile(model, Scope.defaultScope(3).intLow(-3).intHigh(3));
        assertEquals(39, solver.allInstances().length);
    }

    /**
     * <pre>
     * Feature
     *     Cost -> Int 2..3
     *     [this.Cost.ref &lt;= 2]
     * </pre>
     */
    @Test(timeout = 60000)
    public void testSumSetLessThanEqual() {
        /*
         * import Control.Monad
         * import Data.List
         * 
         * isUnique [] = True
         * isUnique (x : xs) = x `notElem` xs && isUnique xs
         *
         * choose 0 _ = return []
         * choose _ [] = mzero
         * choose n (x:xs) =
         *     do
         *         xs' <- choose (n-1) (x:xs)
         *         return $ x : xs'
         *     `mplus` choose n xs 
         *    
         * solutions = do
         *     numCost <- [2..3]
         *     cost <- choose numCost [-3..3]
         *     guard $ isUnique cost
         *     guard $ sum cost <= 2
         *     return cost
         */
        AstModel model = newModel();

        AstConcreteClafer feature = model.addChild("Feature").withCard(1, 1);
        AstConcreteClafer cost = feature.addChild("Cost").withCard(2, 3).refToUnique(IntType);
        feature.addConstraint(lessThanEqual(joinRef(join($this(), cost)), constant(2)));

        ClaferSolver solver = ClaferCompiler.compile(model, Scope.defaultScope(3).intLow(-3).intHigh(3));
        assertEquals(45, solver.allInstances().length);
    }

    /**
     * <pre>
     * Feature
     *     Cost -> Int 2..3
     *     [this.Cost.ref > 2]
     * </pre>
     */
    @Test(timeout = 60000)
    public void testSumSetGreaterThan() {
        /*
         * import Control.Monad
         * import Data.List
         *
         * isUnique [] = True
         * isUnique (x : xs) = x `notElem` xs && isUnique xs
         *
         * choose 0 _ = return []
         * choose _ [] = mzero
         * choose n (x:xs) =
         *     do
         *         xs' <- choose (n-1) (x:xs)
         *         return $ x : xs'
         *     `mplus` choose n xs 
         *    
         * solutions = do
         *     numCost <- [2..3]
         *     cost <- choose numCost [-3..3]
         *     guard $ isUnique cost
         *     guard $ sum cost > 2
         *     return cost
         */
        AstModel model = newModel();

        AstConcreteClafer feature = model.addChild("Feature").withCard(1, 1);
        AstConcreteClafer cost = feature.addChild("Cost").withCard(2, 3).refToUnique(IntType);
        feature.addConstraint(greaterThan(joinRef(join($this(), cost)), constant(2)));

        ClaferSolver solver = ClaferCompiler.compile(model, Scope.defaultScope(3).intLow(-3).intHigh(3));
        assertEquals(11, solver.allInstances().length);
    }

    /**
     * <pre>
     * Feature
     *     Cost -> Int 2..3
     *     [this.Cost.ref >= 2]
     * </pre>
     */
    @Test(timeout = 60000)
    public void testSumSetGreaterThanEqual() {
        /*
         * import Control.Monad
         * import Data.List
         *
         * isUnique [] = True
         * isUnique (x : xs) = x `notElem` xs && isUnique xs
         *
         * choose 0 _ = return []
         * choose _ [] = mzero
         * choose n (x:xs) =
         *     do
         *         xs' <- choose (n-1) (x:xs)
         *         return $ x : xs'
         *     `mplus` choose n xs 
         *    
         * solutions = do
         *     numCost <- [2..3]
         *     cost <- choose numCost [-3..3]
         *     guard $ isUnique cost
         *     guard $ sum cost >= 2
         *     return cost
         */
        AstModel model = newModel();

        AstConcreteClafer feature = model.addChild("Feature").withCard(1, 1);
        AstConcreteClafer cost = feature.addChild("Cost").withCard(2, 3).refToUnique(IntType);
        feature.addConstraint(greaterThanEqual(joinRef(join($this(), cost)), constant(2)));

        ClaferSolver solver = ClaferCompiler.compile(model, Scope.defaultScope(3).intLow(-3).intHigh(3));
        assertEquals(17, solver.allInstances().length);
    }
}
