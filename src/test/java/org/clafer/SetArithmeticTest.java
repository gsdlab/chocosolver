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
        /*
         * import Control.Monad
         * import Data.List
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
         *     numBackup <- [1,2]
         *     numFeature <- [1,2]
         *     feature <- choose numFeature [-1..1]
         *     return (numBackup, feature)
         */
        AstModel model = newModel();

        AstAbstractClafer feature = model.addAbstractClafer("Feature");
        AstConcreteClafer cost = feature.addChild("Cost").withCard(1, 1).refTo(IntType);
        AstConcreteClafer backup = model.addChild("Backup").extending(feature).withCard(1, 2);
        AstConcreteClafer firewall = model.addChild("Firewall").extending(feature).withCard(1, 2);
        model.addConstraint(equal(joinRef(join(diff(global(backup), global(firewall)), cost)), constant(1)));

        ClaferSolver solver = ClaferCompiler.compile(model, Scope.defaultScope(4).intLow(-1).intHigh(1));
        assertEquals(18, solver.allInstances().length);
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
        // Should be 5 with better symmetry breaking.
        assertEquals(6, solver.allInstances().length);
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
        /*
         * import Control.Monad
         * import Data.List
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
         *     backup <- [[1], [1,2]]
         *     numFeature <- [3,4]
         *     feature <- choose numFeature backup
         *     guard $ backup == nub feature
         *     return (backup, feature)
         */
        AstModel model = newModel();

        AstConcreteClafer backup = model.addChild("Backup").withCard(1, 2);
        AstConcreteClafer feature = model.addChild("Feature").withCard(3, 4).refTo(backup);
        model.addConstraint(equal(joinRef(global(feature)), global(backup)));

        ClaferSolver solver = ClaferCompiler.compile(model, Scope.defaultScope(4));
        // Assuming no reference symmetry breaking.
        assertEquals(7, solver.allInstances().length);
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
         * choose 0 _ = return []
         * choose _ [] = mzero
         * choose n (x:xs) =
         *     do
         *         xs' <- choose (n-1) (x:xs)
         *         return $ x : xs'
         *     `mplus` choose n xs 
         *    
         * solutions = do
         *     cost <- choose 2 [0..2]
         *     numPerformance <- [2..3]
         *     performance <- choose numPerformance [0..2]
         *     frugal <- [True, False]
         *     guard $ sum (if frugal then nub cost else nub performance) < 2
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
        assertEquals(90, solver.allInstances().length);
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
         * choose 0 _ = return []
         * choose _ [] = mzero
         * choose n (x:xs) =
         *     do
         *         xs' <- choose (n-1) (x:xs)
         *         return $ x : xs'
         *     `mplus` choose n xs 
         *    
         * solutions = do
         *     cost <- choose 2 [0..2]
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
        assertEquals(9, solver.allInstances().length);
    }

    /**
     * <pre>
     * Cost -> Int 2..3
     * [2 in Cost.ref]
     * </pre>
     */
    @Test(timeout = 60000)
    public void testConstantIn() {
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
         *     cost <- choose numCost [-2..2]
         *     guard $ 2 `elem` cost
         *     guard $ isUnique cost
         return cost
         */
        AstModel model = newModel();

        AstConcreteClafer cost = model.addChild("Cost").withCard(2, 3).refToUnique(IntType);
        model.addConstraint(in(constant(2), joinRef(global(cost))));

        ClaferSolver solver = ClaferCompiler.compile(model, Scope.defaultScope(3).intLow(-2).intHigh(2));
        assertEquals(10, solver.allInstances().length);
    }

    /**
     * <pre>
     * Cost -> Int 2..3
     * [2 not in Cost.ref]
     * </pre>
     */
    @Test(timeout = 60000)
    public void testConstantNotIn() {
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
         *     cost <- choose numCost [-2..2]
         *     guard $ 2 `notElem` cost
         *     guard $ isUnique cost
         *     return cost
         */
        AstModel model = newModel();

        AstConcreteClafer cost = model.addChild("Cost").withCard(2, 3).refToUnique(IntType);
        model.addConstraint(notIn(constant(2), joinRef(global(cost))));

        ClaferSolver solver = ClaferCompiler.compile(model, Scope.defaultScope(3).intLow(-2).intHigh(2));
        assertEquals(10, solver.allInstances().length);
    }

    /**
     * <pre>
     * Cost ->> Int 2..3
     * [Cost.ref in 2]
     * </pre>
     */
    @Test(timeout = 60000)
    public void testInConstant() {
        AstModel model = newModel();

        AstConcreteClafer cost = model.addChild("Cost").withCard(2, 3).refTo(IntType);
        model.addConstraint(in(joinRef(global(cost)), constant(2)));

        ClaferSolver solver = ClaferCompiler.compile(model, Scope.defaultScope(3).intLow(-2).intHigh(2));
        assertEquals(2, solver.allInstances().length);
    }

    /**
     * <pre>
     * Cost ->> Int 2..3
     * [Cost.ref not in 2]
     * </pre>
     */
    @Test(timeout = 60000)
    public void testNotInConstant() {
        /*
         * import Control.Monad
         * import Data.List
         *
         * isSubsetOf = flip $ all . flip elem
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
         *     cost <- choose numCost [-2..2]
         *     guard $ not (sort (nub cost) `isSubsetOf` [2])
         *     return cost
         */
        AstModel model = newModel();

        AstConcreteClafer cost = model.addChild("Cost").withCard(2, 3).refTo(IntType);
        model.addConstraint(notIn(joinRef(global(cost)), constant(2)));

        ClaferSolver solver = ClaferCompiler.compile(model, Scope.defaultScope(3).intLow(-2).intHigh(2));
        assertEquals(48, solver.allInstances().length);
    }

    /**
     * <pre>
     * Cost ->> Int 2..3
     * Payment ->> Int 2..3
     * [Cost.ref in Payment.ref]
     * </pre>
     */
    @Test(timeout = 60000)
    public void testSetInSet() {
        /*
         * import Control.Monad
         * import Data.List
         * 
         * isSubsetOf = flip $ all . flip elem
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
         *     numCost <- [2,3]
         *     numPayment <- [2,3]
         *     cost <- choose numCost [-1..1]
         *     payment <- choose numPayment [-1..1]
         *     guard $ cost `isSubsetOf` payment
         *     return (cost, payment)
         */
        AstModel model = newModel();

        AstConcreteClafer cost = model.addChild("Cost").withCard(2, 3).refTo(IntType);
        AstConcreteClafer payment = model.addChild("Payment").withCard(2, 3).refTo(IntType);
        model.addConstraint(in(joinRef(global(cost)), joinRef(global(payment))));

        ClaferSolver solver = ClaferCompiler.compile(model, Scope.defaultScope(3).intLow(-1).intHigh(1));
        assertEquals(91, solver.allInstances().length);
    }

    /**
     * <pre>
     * Cost ->> Int 2..3
     * Payment ->> Int 2..3
     * [Cost.ref not in Payment.ref]
     * </pre>
     */
    @Test(timeout = 60000)
    public void testSetNotInSet() {
        /*
         * import Control.Monad
         * 
         * isSubsetOf = all . flip elem
         * 
         * solutions = do
         *     numCost <- [2,3]
         *     numPayment <- [2,3]
         *     cost <- sequence $ replicate numCost [-1..1]
         *     payment <- sequence $ replicate numPayment [-1..1]
         *     guard $ not (cost `isSubsetOf` payment)
         *     return (cost, payment)
         */
        AstModel model = newModel();

        AstConcreteClafer cost = model.addChild("Cost").withCard(2, 3).refTo(IntType);
        AstConcreteClafer payment = model.addChild("Payment").withCard(2, 3).refTo(IntType);
        model.addConstraint(notIn(joinRef(global(cost)), joinRef(global(payment))));

        ClaferSolver solver = ClaferCompiler.compile(model, Scope.defaultScope(3).intLow(-1).intHigh(1));
        assertEquals(165, solver.allInstances().length);
    }

    /**
     * <pre>
     * abstract Cost
     * Debt : Cost 3
     * Credit : Cost
     * Payment -> Cost
     * [Payment.ref in Debt]
     * </pre>
     */
    @Test(timeout = 60000)
    public void testIntegerInSetConstant() {
        AstModel model = newModel();

        AstAbstractClafer cost = model.addAbstractClafer("Cost");
        AstConcreteClafer debt = model.addChild("Debt").withCard(3, 3).extending(cost);
        AstConcreteClafer credit = model.addChild("Credit").withCard(Mandatory).extending(cost);
        AstConcreteClafer payment = model.addChild("Payment").refTo(cost).withCard(Mandatory);
        model.addConstraint(in(joinRef(global(payment)), global(debt)));

        ClaferSolver solver = ClaferCompiler.compile(model, Scope.defaultScope(4).intLow(-1).intHigh(1));
        // Should be 1 with stronger symmetry breaking.
        assertEquals(3, solver.allInstances().length);
    }

    /**
     * <pre>
     * abstract Cost
     * Debt : Cost 3
     * Credit : Cost
     * Payment -> Cost
     * [Payment.ref not in Debt]
     * </pre>
     */
    @Test(timeout = 60000)
    public void testIntegerNotInSetConstant() {
        AstModel model = newModel();

        AstAbstractClafer cost = model.addAbstractClafer("Cost");
        AstConcreteClafer debt = model.addChild("Debt").withCard(3, 3).extending(cost);
        AstConcreteClafer credit = model.addChild("Credit").withCard(Mandatory).extending(cost);
        AstConcreteClafer payment = model.addChild("Payment").refTo(cost).withCard(Mandatory);
        model.addConstraint(notIn(joinRef(global(payment)), global(debt)));

        ClaferSolver solver = ClaferCompiler.compile(model, Scope.defaultScope(4).intLow(-1).intHigh(1));
        assertEquals(1, solver.allInstances().length);
    }
}
