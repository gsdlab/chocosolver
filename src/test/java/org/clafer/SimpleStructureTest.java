package org.clafer;

import java.util.HashSet;
import java.util.Set;
import org.clafer.ast.AstAbstractClafer;
import org.clafer.ast.AstConcreteClafer;
import org.clafer.ast.AstModel;
import static org.clafer.ast.Asts.*;
import org.clafer.ast.analysis.InsufficientScopeException;
import org.clafer.collection.Pair;
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
public class SimpleStructureTest {

    /**
     * <pre>
     * abstract Object
     *     Name ?
     * abstract Animal : Object
     *     Tail ?
     * abstract Primate : Animal
     *     Bipedal ?
     * Human : Primate
     * Beaver : Animal
     * </pre>
     */
    @Test(timeout = 60000)
    public void testMultiLevelAbstract() {
        AstModel model = newModel();

        AstAbstractClafer object = model.addAbstract("Object");
        object.addChild("Name").withCard(0, 1);

        AstAbstractClafer animal = model.addAbstract("Animal").extending(object);
        animal.addChild("Tail").withCard(0, 1);

        AstAbstractClafer primate = model.addAbstract("Primate").extending(animal);
        primate.addChild("Bipedal").withCard(0, 1);

        model.addChild("Human").withCard(1, 1).extending(primate);
        model.addChild("Beaver").withCard(1, 1).extending(animal);

        ClaferSolver solver = ClaferCompiler.compile(model, Scope.defaultScope(2));
        assertEquals(32, solver.allInstances().length);
    }

    /**
     * <pre>
     * xor Type
     *     Car ?
     *     Truck ?
     *     Van ?
     * </pre>
     */
    @Test(timeout = 60000)
    public void testXorGroupCardinality() {
        AstModel model = newModel();

        AstConcreteClafer type = model.addChild("Type").withCard(1, 1).withGroupCard(1, 1);
        type.addChild("Car").withCard(0, 1);
        type.addChild("Truck").withCard(0, 1);
        type.addChild("Van").withCard(0, 1);

        ClaferSolver solver = ClaferCompiler.compile(model, Scope.defaultScope(2));
        assertEquals(3, solver.allInstances().length);
    }

    /**
     * <pre>
     * xor Type
     *     Car ?
     *     Truck ?
     *     Van ?
     * </pre>
     */
    @Test(timeout = 60000)
    public void testCustomGroupCardinality() {
        AstModel model = newModel();

        AstConcreteClafer type = model.addChild("Type").withCard(1, 1).withGroupCard(2, 3);
        type.addChild("Car").withCard(0, 1);
        type.addChild("Truck").withCard(0, 1);
        type.addChild("Van").withCard(0, 1);

        ClaferSolver solver = ClaferCompiler.compile(model, Scope.defaultScope(2));
        assertEquals(4, solver.allInstances().length);
    }

    /**
     * <pre>
     * xor Type ?
     *     Car ?
     *     Truck ?
     *     Van ?
     * </pre>
     */
    @Test(timeout = 60000)
    public void testMaybeGroupCardinality() {
        AstModel model = newModel();

        AstConcreteClafer type = model.addChild("Type").withCard(0, 1).withGroupCard(1, 1);
        type.addChild("Car").withCard(0, 1);
        type.addChild("Truck").withCard(0, 1);
        type.addChild("Van").withCard(0, 1);

        ClaferSolver solver = ClaferCompiler.compile(model, Scope.defaultScope(2));
        assertEquals(4, solver.allInstances().length);
    }

    /**
     * <pre>
     * Person
     *     Age ->> integer 3
     * </pre>
     */
    @Test(timeout = 60000)
    public void testFixedRefs() {
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
         * solutions = choose 3 [-2..2] :: [[Int]]
         */
        AstModel model = newModel();

        AstConcreteClafer person = model.addChild("Person").withCard(1, 1);
        person.addChild("Age").withCard(3, 3).refTo(IntType);

        ClaferSolver solver = ClaferCompiler.compile(model, Scope.defaultScope(3).intLow(-2).intHigh(2));
        assertEquals(35, solver.allInstances().length);
    }

    /**
     * <pre>
     * Person
     *     Age ->> integer 2..3
     * </pre>
     */
    @Test(timeout = 60000)
    public void testVariableRefs() {
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
         *     numAge <- [2,3]
         *     choose numAge [-2..2]
         */
        AstModel model = newModel();

        AstConcreteClafer person = model.addChild("Person").withCard(1, 1);
        person.addChild("Age").withCard(2, 3).refTo(IntType);

        ClaferSolver solver = ClaferCompiler.compile(model, Scope.defaultScope(3).intLow(-2).intHigh(2));
        assertEquals(50, solver.allInstances().length);
    }

    /**
     * <pre>
     * Person
     *     Age -> integer 3
     * </pre>
     */
    @Test(timeout = 60000)
    public void testFixedUniqueRefs() {
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
         *     age <- choose 3 [-2..2]
         *     guard $ isUnique age
         *     (return age) :: [[Int]]
         */
        AstModel model = newModel();

        AstConcreteClafer person = model.addChild("Person").withCard(Mandatory);
        person.addChild("Age").withCard(3, 3).refToUnique(IntType);

        ClaferSolver solver = ClaferCompiler.compile(model, Scope.defaultScope(3).intLow(-2).intHigh(2));
        assertEquals(10, solver.allInstances().length);
    }

    /**
     * <pre>
     * Person 2
     *     Age -> integer 2
     * </pre>
     */
    @Test(timeout = 60000)
    public void testGlobalFixedUniqueRefs() {
        /*
         * int solutions = 0;
         * for (int i = -2; i <= 2; i++) {
         *     for (int j = i + 1; j <= 2; j++) {
         *         for (int k = i; k <= 2; k++) {
         *             for (int l = k + 1; l <= 2; l++) {
         *                 if (i != k || j != l) {
         *                     solutions++;
         *                 }
         *             }
         *         }
         *     }
         * }
         */
        AstModel model = newModel();

        AstConcreteClafer person = model.addChild("Person").withCard(2, 2);
        person.addChild("Age").withCard(2, 2).refToUnique(IntType);

        ClaferSolver solver = ClaferCompiler.compile(model, Scope.defaultScope(4).intLow(-2).intHigh(2));
        assertEquals(55, solver.allInstances().length);
    }

    /**
     * <pre>
     * Person
     *     Age -> integer 2..3
     * </pre>
     */
    @Test(timeout = 60000)
    public void testVariableUniqueRefs() {
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
         *     numAge <- [2,3]
         *     age <- choose numAge [-2..2]
         *     guard $ isUnique age
         *     return age
         */
        AstModel model = newModel();

        AstConcreteClafer person = model.addChild("Person").withCard(1, 1);
        person.addChild("Age").withCard(2, 3).refToUnique(IntType);

        ClaferSolver solver = ClaferCompiler.compile(model, Scope.defaultScope(3).intLow(-2).intHigh(2));
        assertEquals(20, solver.allInstances().length);
    }

    /**
     * <pre>
     * Age ->> integer 2..3
     * </pre>
     */
    @Test(timeout = 60000)
    public void testTopLevelRefs() {
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
         *     numAge <- [2,3]
         *     choose numAge [-2..2]
         */
        AstModel model = newModel();

        model.addChild("Age").withCard(2, 3).refTo(IntType);

        ClaferSolver solver = ClaferCompiler.compile(model, Scope.defaultScope(3).intLow(-2).intHigh(2));
        assertEquals(50, solver.allInstances().length);
    }

    /**
     * <pre>
     * Age -> integer 2..3
     * </pre>
     */
    @Test(timeout = 60000)
    public void testTopLevelUniqueRefs() {
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
         *     numAge <- [2,3]
         *     choose numAge [-2..2]
         */
        AstModel model = newModel();

        model.addChild("Age").withCard(2, 3).refToUnique(IntType);

        ClaferSolver solver = ClaferCompiler.compile(model, Scope.defaultScope(3).intLow(-2).intHigh(2));
        assertEquals(20, solver.allInstances().length);
    }

    /**
     * <pre>
     * abstract Feature ->> integer
     * Backup : Feature
     * Firewall : Feature 1..2
     * </pre>
     */
    @Test(timeout = 60000)
    public void testAbstractRefs() {
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
         *     backup <- [-2..2]
         *     numFirewall <- [1,2]
         *     firewall <- choose numFirewall [-2..2]
         *     return (backup, firewall)
         */
        AstModel model = newModel();

        AstAbstractClafer feature = model.addAbstract("Feature").refTo(IntType);
        model.addChild("Backup").withCard(1, 1).extending(feature);
        model.addChild("Firewall").withCard(1, 2).extending(feature);

        ClaferSolver solver = ClaferCompiler.compile(model, Scope.defaultScope(2).intLow(-2).intHigh(2));
        assertEquals(100, solver.allInstances().length);
    }

    /**
     * <pre>
     * abstract Feature -> integer
     * Backup : Feature
     * Firewall : Feature 1..2
     * </pre>
     */
    @Test(timeout = 60000)
    public void testAbstractUniqueRefs() {
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
         *     backup <- [-2..2]
         *     numFirewall <- [1,2]
         *     firewall <- choose numFirewall [-2..2]
         *     guard $ isUnique firewall
         *     return (backup, firewall)
         */
        AstModel model = newModel();

        AstAbstractClafer feature = model.addAbstract("Feature").refToUnique(IntType);
        model.addChild("Backup").withCard(1, 1).extending(feature);
        model.addChild("Firewall").withCard(1, 2).extending(feature);

        ClaferSolver solver = ClaferCompiler.compile(model, Scope.defaultScope(2).intLow(-2).intHigh(2));
        assertEquals(75, solver.allInstances().length);
    }

    /**
     * <pre>
     * A 3..4
     * B ->> A 2..3
     * </pre>
     */
    @Test(timeout = 60000)
    public void testRefToVariable() {
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
         *     a <- [3, 4]
         *     numB <- [2, 3]
         *     b <- choose numB [1..a]
         *     guard $ all (\x -> length (filter (== x) b)>= length (filter (== x + 1) b)) [1..a - 1]
         *     return (a, b)
         */
        AstModel model = newModel();

        AstConcreteClafer a = model.addChild("A").withCard(3, 4);
        AstConcreteClafer b = model.addChild("B").refTo(a).withCard(2, 3);

        ClaferSolver solver = ClaferCompiler.compile(model, Scope.defaultScope(4));
        assertEquals(10, solver.allInstances().length);
    }

    /**
     * <pre>
     * A 3..4
     * B -> A 2..3
     * </pre>
     */
    @Test(timeout = 60000)
    public void testUniqueRefToVariable() {
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
         *     a <- [3, 4]
         *     numB <- [2, 3]
         *     b <- choose numB [1..a]
         *     guard $ isUnique b
         *     guard $ all (\x -> length (filter (== x) b) >= length (filter (== x + 1) b)) [1..a - 1]
         *     return (a, b)
         */
        AstModel model = newModel();

        AstConcreteClafer a = model.addChild("A").withCard(3, 4);
        AstConcreteClafer b = model.addChild("B").refToUnique(a).withCard(2, 3);

        ClaferSolver solver = ClaferCompiler.compile(model, Scope.defaultScope(4));
        assertEquals(4, solver.allInstances().length);
    }

    /**
     * <pre>
     * A ?
     * B ->> A 2..3
     * </pre>
     */
    public void testNotEnoughRef() {
        AstModel model = newModel();

        AstConcreteClafer a = model.addChild("A").withCard(1, 1);
        AstConcreteClafer b = model.addChild("B").refTo(a).withCard(2, 3);

        try {
            ClaferCompiler.compile(model, Scope.setScope(a, 1).setScope(b, 3));
            fail();
        } catch (InsufficientScopeException e) {
            assertArrayEquals(
                    new Pair<?, ?>[]{new Pair<>(a, 1)},
                    e.getInsufficientScopes());
        }
    }

    /**
     * <pre>
     * A
     * B -> A 2..3
     * </pre>
     */
    @Test(timeout = 60000)
    public void testNotEnoughUniqueRef() {
        AstModel model = newModel();

        AstConcreteClafer a = model.addChild("A").withCard(1, 1);
        AstConcreteClafer b = model.addChild("B").refToUnique(a).withCard(2, 3);

        try {
            ClaferCompiler.compile(model, Scope.defaultScope(4));
            fail();
        } catch (InsufficientScopeException e) {
            assertArrayEquals(
                    new Pair<?, ?>[]{new Pair<>(a, 2)},
                    e.getInsufficientScopes());
        }
    }

    /**
     * <pre>
     * A
     * B -> A 2..3
     * </pre>
     */
    @Test(timeout = 60000)
    public void testNotEnoughAbstract() {
        AstModel model = newModel();

        AstAbstractClafer a = model.addAbstract("A");
        AstConcreteClafer b = model.addChild("B").refToUnique(a).withCard(2, 3);

        try {
            ClaferCompiler.compile(model, Scope.defaultScope(4));
            fail();
        } catch (InsufficientScopeException e) {
            assertArrayEquals(
                    new Pair<?, ?>[]{new Pair<>(a, 2)},
                    e.getInsufficientScopes());
        }
    }

    /**
     * <pre>
     * abstract A -> D
     * B : A
     * C : A 2
     * D
     * </pre>
     */
    @Test(timeout = 60000)
    public void testNotEnoughFromAbstract() {
        AstModel model = newModel();

        AstAbstractClafer a = model.addAbstract("A");
        AstConcreteClafer b = model.addChild("B").extending(a).withCard(Mandatory);
        AstConcreteClafer c = model.addChild("C").extending(a).withCard(2, 2);
        AstConcreteClafer d = model.addChild("D").withCard(Mandatory);
        a.refToUnique(d);

        try {
            ClaferCompiler.compile(model, Scope.defaultScope(2));
            fail();
        } catch (InsufficientScopeException e) {
            assertArrayEquals(
                    new Pair<?, ?>[]{new Pair<>(d, 2)},
                    e.getInsufficientScopes());
        }
    }

    /**
     * <pre>
     * Feature 1..2
     * Free -> Feature
     * </pre>
     */
    @Test(timeout = 60000)
    public void testRefToVariableClafer() {
        AstModel model = newModel();

        AstConcreteClafer feature = model.addChild("Feature").withCard(1, 2);
        model.addChild("Free").withCard(1, 1).refTo(feature);

        ClaferSolver solver = ClaferCompiler.compile(model, Scope.defaultScope(2));
        // Can be reduced to 2 with better symmetry breaking
        assertEquals(3, solver.allInstances().length);
    }

    /**
     * <pre>
     * Age -> integer ?
     * </pre>
     */
    @Test(timeout = 60000)
    public void testRefZeroOutsideIntRange() {
        AstModel model = newModel();

        model.addChild("Age").withCard(0, 1).refTo(IntType);

        ClaferSolver solver = ClaferCompiler.compile(model, Scope.defaultScope(1).intLow(1).intHigh(2));
        assertEquals(3, solver.allInstances().length);
    }

    /**
     * <pre>
     * abstract A
     * B : A 2
     * C : A 2
     * D -> A
     * </pre>
     */
    @Test(timeout = 60000)
    public void testRefToAbstract() {
        AstModel model = newModel();

        AstAbstractClafer a = model.addAbstract("A");
        AstConcreteClafer b = model.addChild("B").extending(a).withCard(2, 2);
        AstConcreteClafer c = model.addChild("C").extending(a).withCard(2, 2);
        AstConcreteClafer d = model.addChild("D").refTo(a).withCard(Mandatory);

        ClaferSolver solver = ClaferCompiler.compile(model, Scope.defaultScope(2));

        Set<InstanceModel> actual = new HashSet<>();
        for (InstanceModel instance : solver.allInstances()) {
            actual.add(instance);
        }
        InstanceClafer b0 = new InstanceClafer(b, 0, null);
        InstanceClafer b1 = new InstanceClafer(b, 1, null);
        InstanceClafer c0 = new InstanceClafer(c, 0, null);
        InstanceClafer c1 = new InstanceClafer(c, 1, null);
        {
            InstanceClafer d0 = new InstanceClafer(d, 0, b0);
            assertTrue(actual.contains(new InstanceModel(b0, b1, c0, c1, d0)));
        }
        {
            InstanceClafer d0 = new InstanceClafer(d, 0, b1);
            assertTrue(actual.contains(new InstanceModel(b0, b1, c0, c1, d0)));
        }
        {
            InstanceClafer d0 = new InstanceClafer(d, 0, c0);
            assertTrue(actual.contains(new InstanceModel(b0, b1, c0, c1, d0)));
        }
        {
            InstanceClafer d0 = new InstanceClafer(d, 0, c1);
            assertTrue(actual.contains(new InstanceModel(b0, b1, c0, c1, d0)));
        }
        assertEquals(4, actual.size());
    }

    /**
     * <pre>
     * abstract A
     * abstract B
     * C : B *
     * </pre>
     */
    @Test(timeout = 60000)
    public void testUnusedAbstract() {
        AstModel model = newModel();

        AstAbstractClafer a = model.addAbstract("A");
        AstAbstractClafer b = model.addAbstract("B");
        AstConcreteClafer c = model.addChild("C").extending(b);

        ClaferSolver solver = ClaferCompiler.compile(model, Scope.setScope(c, 1));
        assertTrue(solver.find());
    }

    /**
     * <pre>
     * A 1..2
     *     B 2+
     * </pre>
     */
    @Test(timeout = 60000)
    public void testForceEmptySecondChild() {
        AstModel model = newModel();

        AstConcreteClafer a = model.addChild("A").withCard(1, 2);
        AstConcreteClafer b = a.addChild("B").withCard(2);

        ClaferSolver solver = ClaferCompiler.compile(model, Scope.setScope(a, 2).setScope(b, 3));
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

        AstAbstractClafer a = model.addAbstract("A");
        AstAbstractClafer b = model.addAbstract("B");
        AstConcreteClafer c = model.addChild("C").extending(b);
        AstConcreteClafer d = model.addChild("D").refTo(a).withCard(Optional);

        ClaferSolver solver = ClaferCompiler.compile(model, Scope.setScope(c, 1).setScope(d, 1));
        assertTrue(solver.find());
    }

    /**
     * <pre>
     * abstract Path
     *     p : Path ?
     *
     * pth : Path
     * </pre>
     */
    @Test(timeout = 60000)
    public void testCircular() {
        AstModel model = newModel();

        AstAbstractClafer path = model.addAbstract("Path");
        AstConcreteClafer p = path.addChild("p").extending(path).withCard(Optional);
        AstConcreteClafer pth = model.addChild("pth").extending(path).withCard(Mandatory);

        ClaferSolver solver = ClaferCompiler.compile(model, Scope.defaultScope(5));
        assertEquals(6, solver.allInstances().length);
    }

    /**
     * <pre>
     * abstract Path
     *     p : Path ?
     *         q : Path ?
     *
     * pth : Path
     * </pre>
     */
    @Test(timeout = 60000)
    public void test2LevelCircular() {
        AstModel model = newModel();

        AstAbstractClafer path = model.addAbstract("Path");
        AstConcreteClafer p = path.addChild("p").extending(path).withCard(Optional);
        AstConcreteClafer q = p.addChild("q").extending(path).withCard(Optional);
        AstConcreteClafer pth = model.addChild("pth").extending(path).withCard(Mandatory);

        ClaferSolver solver = ClaferCompiler.compile(model, Scope.defaultScope(5));
        assertEquals(243, solver.allInstances().length);
    }
}
