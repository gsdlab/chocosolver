package org.clafer;

import java.util.HashSet;
import java.util.Set;
import org.clafer.scope.Scope;
import org.clafer.ast.AstAbstractClafer;
import org.clafer.ast.AstConcreteClafer;
import org.clafer.ast.AstModel;
import static org.clafer.ast.Asts.*;
import org.clafer.compiler.ClaferCompiler;
import org.clafer.compiler.ClaferSolver;
import org.clafer.instance.InstanceModel;
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
        
        AstAbstractClafer object = model.addAbstractClafer("Object");
        object.addChild("Name").withCard(0, 1);
        
        AstAbstractClafer animal = model.addAbstractClafer("Animal").extending(object);
        animal.addChild("Tail").withCard(0, 1);
        
        AstAbstractClafer primate = model.addAbstractClafer("Primate").extending(animal);
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
        AstModel model = newModel();
        
        AstConcreteClafer person = model.addChild("Person").withCard(1, 1);
        person.addChild("Age").withCard(3, 3).refTo(IntType);
        
        ClaferSolver solver = ClaferCompiler.compile(model, Scope.defaultScope(3).intLow(-2).intHigh(2));
        assertEquals(125, solver.allInstances().length);
    }

    /**
     * <pre>
     * Person
     *     Age ->> integer 2..3
     * </pre>
     */
    @Test(timeout = 60000)
    public void testVariableRefs() {
        AstModel model = newModel();
        
        AstConcreteClafer person = model.addChild("Person").withCard(1, 1);
        person.addChild("Age").withCard(2, 3).refTo(IntType);
        
        ClaferSolver solver = ClaferCompiler.compile(model, Scope.defaultScope(3).intLow(-2).intHigh(2));
        assertEquals(150, solver.allInstances().length);
    }

    /**
     * <pre>
     * Person
     *     Age -> integer 3
     * </pre>
     */
    @Test(timeout = 60000)
    public void testFixedUniqueRefs() {
        AstModel model = newModel();
        
        AstConcreteClafer person = model.addChild("Person").withCard(1, 1);
        person.addChild("Age").withCard(3, 3).refToUnique(IntType);
        
        ClaferSolver solver = ClaferCompiler.compile(model, Scope.defaultScope(3).intLow(-2).intHigh(2));
        assertEquals(60, solver.allInstances().length);
    }

    /**
     * <pre>
     * Person
     *     Age ->> integer 2..3
     * </pre>
     */
    @Test(timeout = 60000)
    public void testVariableUniqueRefs() {
        AstModel model = newModel();
        
        AstConcreteClafer person = model.addChild("Person").withCard(1, 1);
        person.addChild("Age").withCard(2, 3).refToUnique(IntType);
        
        ClaferSolver solver = ClaferCompiler.compile(model, Scope.defaultScope(3).intLow(-2).intHigh(2));
        assertEquals(80, solver.allInstances().length);
    }

    /**
     * <pre>
     * Age ->> integer 2..3
     * </pre>
     */
    @Test(timeout = 60000)
    public void testTopLevelRefs() {
        AstModel model = newModel();
        
        model.addChild("Age").withCard(2, 3).refTo(IntType);
        
        ClaferSolver solver = ClaferCompiler.compile(model, Scope.defaultScope(3).intLow(-2).intHigh(2));
        assertEquals(150, solver.allInstances().length);
    }

    /**
     * <pre>
     * Age -> integer 2..3
     * </pre>
     */
    @Test(timeout = 60000)
    public void testTopLevelUniqueRefs() {
        AstModel model = newModel();
        
        model.addChild("Age").withCard(2, 3).refToUnique(IntType);
        
        ClaferSolver solver = ClaferCompiler.compile(model, Scope.defaultScope(3).intLow(-2).intHigh(2));
        assertEquals(80, solver.allInstances().length);
    }

    /**
     * <pre>
     * abstract Feature -> integer
     * Backup : Feature
     * Firewall : Feature 1..2
     * </pre>
     */
    @Test(timeout = 60000)
    public void testAbstractRefs() {
        AstModel model = newModel();
        
        AstAbstractClafer feature = model.addAbstractClafer("Feature").refTo(IntType);
        model.addChild("Backup").withCard(1, 1).extending(feature);
        model.addChild("Firewall").withCard(1, 2).extending(feature);
        
        ClaferSolver solver = ClaferCompiler.compile(model, Scope.defaultScope(2).intLow(-2).intHigh(2));
        assertEquals(150, solver.allInstances().length);
    }

    /**
     * <pre>
     * abstract Feature ->> integer
     * Backup : Feature
     * Firewall : Feature 1..2
     * </pre>
     */
    @Test(timeout = 60000)
    public void testAbstractUniqueRefs() {
        AstModel model = newModel();
        
        AstAbstractClafer feature = model.addAbstractClafer("Feature").refToUnique(IntType);
        model.addChild("Backup").withCard(1, 1).extending(feature);
        model.addChild("Firewall").withCard(1, 2).extending(feature);
        
        ClaferSolver solver = ClaferCompiler.compile(model, Scope.defaultScope(2).intLow(-2).intHigh(2));
        assertEquals(125, solver.allInstances().length);
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
         * 
         * solutions = do
         *     a <- [3, 4]
         *     numB <- [2, 3]
         *     b <- sequence $ replicate numB [1..a]
         *     return (a, b)
         */
        AstModel model = newModel();
        
        AstConcreteClafer a = model.addChild("A").withCard(3, 4);
        AstConcreteClafer b = model.addChild("B").refTo(a).withCard(2, 3);
        
        ClaferSolver solver = ClaferCompiler.compile(model, Scope.defaultScope(4));
        
        assertEquals(116, solver.allInstances().length);
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
         * solutions = do
         * a <- [3, 4]
         * numB <- [2, 3]
         * b <- sequence $ replicate numB [1..a]
         * guard $ isUnique b
         * return (a, b)
         */
        AstModel model = newModel();
        
        AstConcreteClafer a = model.addChild("A").withCard(3, 4);
        AstConcreteClafer b = model.addChild("B").refToUnique(a).withCard(2, 3);
        
        ClaferSolver solver = ClaferCompiler.compile(model, Scope.defaultScope(4));
        
        assertEquals(48, solver.allInstances().length);
    }

    /**
     * A
     * B -> A 2..3
     */
    @Test(timeout = 60000)
    public void testNotEnoughUniqueRef() {
        AstModel model = newModel();
        
        AstConcreteClafer a = model.addChild("A").withCard(1, 1);
        AstConcreteClafer b = model.addChild("B").refToUnique(a).withCard(2, 3);
        
        ClaferSolver solver = ClaferCompiler.compile(model, Scope.defaultScope(4));
        
        assertEquals(0, solver.allInstances().length);
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
     * Age -> integer ?
     */
    @Test(timeout = 60000)
    public void testRefZeroOutsideIntRange() {
        AstModel model = newModel();
        
        model.addChild("Age").withCard(0, 1).refTo(IntType);
        
        ClaferSolver solver = ClaferCompiler.compile(model, Scope.defaultScope(1).intLow(1).intHigh(2));
        // Even though it should be "3", the current compiler will add "0" to the int range.
        assertEquals(4, solver.allInstances().length);
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
    public void testRefToAbstractToString() {
        AstModel model = newModel();
        
        AstAbstractClafer a = model.addAbstractClafer("A");
        AstConcreteClafer b = model.addChild("B").extending(a).withCard(2, 2);
        AstConcreteClafer c = model.addChild("C").extending(a).withCard(2, 2);
        AstConcreteClafer d = model.addChild("D").refTo(a).withCard(Mandatory);
        
        ClaferSolver solver = ClaferCompiler.compile(model, Scope.defaultScope(2));
        
        Set<String> actual = new HashSet<String>();
        for (InstanceModel instance : solver.allInstances()) {
            actual.add(instance.toString());
        }
        Set<String> expected = new HashSet<String>();
        expected.add("B#0\nB#1\nC#0\nC#1\nD#0 = B#0\n");
        expected.add("B#0\nB#1\nC#0\nC#1\nD#0 = B#1\n");
        expected.add("B#0\nB#1\nC#0\nC#1\nD#0 = C#0\n");
        expected.add("B#0\nB#1\nC#0\nC#1\nD#0 = C#1\n");
        assertEquals(expected, actual);
    }
}
