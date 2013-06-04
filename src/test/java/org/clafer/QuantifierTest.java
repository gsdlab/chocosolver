package org.clafer;

import org.clafer.ast.AstAbstractClafer;
import org.clafer.ast.AstConcreteClafer;
import org.clafer.ast.AstLocal;
import org.clafer.ast.AstModel;
import static org.clafer.ast.Asts.*;
import org.clafer.scope.Scope;
import org.clafer.compiler.ClaferCompiler;
import org.clafer.compiler.ClaferSolver;
import static org.junit.Assert.assertEquals;
import org.junit.Test;

/**
 *
 * @author jimmy
 */
public class QuantifierTest {

    /**
     * <pre>
     * abstract Feature
     *     Cost ->> integer
     * Backup : Feature 2..3
     * Firewall : Feature ?
     * Guard : Feature ?
     * [all a : Backup ++ Firewall | a.Cost.ref = 2]
     * </pre>
     */
    @Test
    public void testAll() {
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
         *     backupCard <- [2..3]
         *     backup <- choose backupCard [-1..1]
         *    
         *     firewallCard <- [0, 1]
         *     firewall <- choose firewallCard [-1..1]
         *    
         *     guarddCard <- [0, 1]
         *     guardd <- choose guarddCard [-1..1]
         *    
         *     guard $ all (== 1) (backup ++ firewall)
         *    
         *     return (backup, firewall)
         */
        AstModel model = newModel();

        AstAbstractClafer feature = model.addAbstractClafer("Feature");
        AstConcreteClafer cost = feature.addChild("Cost").withCard(1, 1).refTo(IntType);
        AstConcreteClafer backup = model.addChild("Backup").withCard(2, 3).extending(feature);
        AstConcreteClafer firewall = model.addChild("Firewall").withCard(0, 1).extending(feature);
        AstConcreteClafer guard = model.addChild("Guard").withCard(0, 1).extending(feature);
        AstLocal x = local("x");
        model.addConstraint(all(decl(x, union(global(backup), global(firewall))),
                equal(joinRef(join(x, cost)), constant(1))));

        ClaferSolver solver = ClaferCompiler.compile(model, Scope.defaultScope(5).intLow(-1).intHigh(1));
        assertEquals(16, solver.allInstances().length);
    }

    /**
     * <pre>
     * abstract Feature
     *     Cost ->> integer
     * Backup : Feature 2..3
     * Firewall : Feature ?
     * Guard : Feature ?
     * [lone a : Backup ++ Firewall | a.Cost.ref = 2]
     * </pre>
     */
    @Test
    public void testLone() {
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
         *     backupCard <- [2..3]
         *     backup <- choose backupCard [-1..1]
         *    
         *     firewallCard <- [0, 1]
         *     firewall <- choose firewallCard [-1..1]
         *    
         *     guarddCard <- [0, 1]
         *     guardd <- choose guarddCard [-1..1]
         *     
         *     guard $ length (filter (== 1) (backup ++ firewall)) <= 1
         *     
         *     return (backup, firewall)
         */
        AstModel model = newModel();

        AstAbstractClafer feature = model.addAbstractClafer("Feature");
        AstConcreteClafer cost = feature.addChild("Cost").withCard(1, 1).refTo(IntType);
        AstConcreteClafer backup = model.addChild("Backup").withCard(2, 3).extending(feature);
        AstConcreteClafer firewall = model.addChild("Firewall").withCard(0, 1).extending(feature);
        AstConcreteClafer guard = model.addChild("Guard").withCard(0, 1).extending(feature);
        AstLocal x = local("x");
        model.addConstraint(lone(decl(x, union(global(backup), global(firewall))),
                equal(joinRef(join(x, cost)), constant(1))));

        ClaferSolver solver = ClaferCompiler.compile(model, Scope.defaultScope(5).intLow(-1).intHigh(1));
        assertEquals(172, solver.allInstances().length);
    }

    /**
     * <pre>
     * abstract Feature
     *     Cost ->> integer
     * Backup : Feature 2..3
     * Firewall : Feature ?
     * Guard : Feature ?
     * [no a : Backup ++ Firewall | a.Cost.ref = 2]
     * </pre>
     */
    @Test
    public void testNone() {
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
         *     backupCard <- [2..3]
         *     backup <- choose backupCard [-1..1]
         *    
         *     firewallCard <- [0, 1]
         *     firewall <- choose firewallCard [-1..1]
         *    
         *     guarddCard <- [0, 1]
         *     guardd <- choose guarddCard [-1..1]
         *    
         *     guard $ all (/= 1) (backup ++ firewall)
         *    
         *     return (backup, firewall)
         */
        AstModel model = newModel();

        AstAbstractClafer feature = model.addAbstractClafer("Feature");
        AstConcreteClafer cost = feature.addChild("Cost").withCard(1, 1).refTo(IntType);
        AstConcreteClafer backup = model.addChild("Backup").withCard(2, 3).extending(feature);
        AstConcreteClafer firewall = model.addChild("Firewall").withCard(0, 1).extending(feature);
        AstConcreteClafer guard = model.addChild("Guard").withCard(0, 1).extending(feature);
        AstLocal x = local("x");
        model.addConstraint(none(decl(x, union(global(backup), global(firewall))),
                equal(joinRef(join(x, cost)), constant(1))));

        ClaferSolver solver = ClaferCompiler.compile(model, Scope.defaultScope(5).intLow(-1).intHigh(1));
        assertEquals(84, solver.allInstances().length);
    }

    /**
     * <pre>
     * abstract Feature
     *     Cost ->> integer
     * Backup : Feature 2..3
     * Firewall : Feature ?
     * Guard : Feature ?
     * [one a : Backup ++ Firewall | a.Cost.ref = 2]
     * </pre>
     */
    @Test
    public void testOne() {
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
         *     backupCard <- [2..3]
         *     backup <- choose backupCard [-1..1]
         *    
         *     firewallCard <- [0, 1]
         *     firewall <- choose firewallCard [-1..1]
         *    
         *     guarddCard <- [0, 1]
         *     guardd <- choose guarddCard [-1..1]
         *    
         *     guard $ length (filter (== 1) (backup ++ firewall)) == 1
         *    
         *     return (backup, firewall)
         */
        AstModel model = newModel();

        AstAbstractClafer feature = model.addAbstractClafer("Feature");
        AstConcreteClafer cost = feature.addChild("Cost").withCard(1, 1).refTo(IntType);
        AstConcreteClafer backup = model.addChild("Backup").withCard(2, 3).extending(feature);
        AstConcreteClafer firewall = model.addChild("Firewall").withCard(0, 1).extending(feature);
        AstConcreteClafer guard = model.addChild("Guard").withCard(0, 1).extending(feature);
        AstLocal x = local("x");
        model.addConstraint(one(decl(x, union(global(backup), global(firewall))),
                equal(joinRef(join(x, cost)), constant(1))));

        ClaferSolver solver = ClaferCompiler.compile(model, Scope.defaultScope(5).intLow(-1).intHigh(1));
        assertEquals(88, solver.allInstances().length);
    }

    /**
     * <pre>
     * abstract Feature
     *     Cost ->> integer
     * Backup : Feature 2..3
     * Firewall : Feature ?
     * Guard : Feature ?
     * [some a : Backup ++ Firewall | a.Cost.ref = 2]
     * </pre>
     */
    @Test
    public void testSome() {
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
         *     backupCard <- [2..3]
         *     backup <- choose backupCard [-1..1]
         *    
         *     firewallCard <- [0, 1]
         *     firewall <- choose firewallCard [-1..1]
         *    
         *     guarddCard <- [0, 1]
         *     guardd <- choose guarddCard [-1..1]
         *    
         *     guard $ any (== 1) (backup ++ firewall)
         *    
         *     return (backup, firewall)
         */
        AstModel model = newModel();

        AstAbstractClafer feature = model.addAbstractClafer("Feature");
        AstConcreteClafer cost = feature.addChild("Cost").withCard(1, 1).refTo(IntType);
        AstConcreteClafer backup = model.addChild("Backup").withCard(2, 3).extending(feature);
        AstConcreteClafer firewall = model.addChild("Firewall").withCard(0, 1).extending(feature);
        AstConcreteClafer guard = model.addChild("Guard").withCard(0, 1).extending(feature);
        AstLocal x = local("x");
        model.addConstraint(some(decl(x, union(global(backup), global(firewall))),
                equal(joinRef(join(x, cost)), constant(1))));

        ClaferSolver solver = ClaferCompiler.compile(model, Scope.defaultScope(5).intLow(-1).intHigh(1));
        assertEquals(172, solver.allInstances().length);
    }
}
