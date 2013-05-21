package org.clafer;

import org.clafer.ast.scope.Scope;
import org.clafer.ast.AstAbstractClafer;
import org.clafer.ast.AstConcreteClafer;
import org.clafer.ast.AstModel;
import static org.clafer.ast.Asts.*;
import org.clafer.compiler.ClaferCompiler;
import org.clafer.compiler.ClaferSolver;
import static org.junit.Assert.*;
import org.junit.Test;

/**
 *
 * @author jimmy
 */
public class SimpleStructureTest {

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

        ClaferSolver solver = ClaferCompiler.compile(model, Scope.defaultScope(2).toScope());
        assertEquals(32, solver.allInstances().length);
    }

    @Test(timeout = 60000)
    public void testGroupCardinality() {
        AstModel model = newModel();

        AstConcreteClafer type = model.addChild("Type").withCard(1, 1).withGroupCard(1, 1);
        type.addChild("Car").withCard(0, 1);
        type.addChild("Truck").withCard(0, 1);
        type.addChild("Van").withCard(0, 1);

        ClaferSolver solver = ClaferCompiler.compile(model, Scope.defaultScope(2).toScope());
        assertEquals(3, solver.allInstances().length);
    }

    @Test(timeout = 60000)
    public void testRefs() {
        AstModel model = newModel();

        AstConcreteClafer person = model.addChild("Person").withCard(1, 1);
        person.addChild("Age").withCard(2, 2).refTo(IntType);

        ClaferSolver solver = ClaferCompiler.compile(model, Scope.defaultScope(2).intLow(-2).intHigh(2).toScope());
        assertEquals(25, solver.allInstances().length);
    }

    @Test(timeout = 60000)
    public void testUniqueRefs() {
        AstModel model = newModel();

        AstConcreteClafer person = model.addChild("Person").withCard(1, 1);
        person.addChild("Age").withCard(2, 2).refToUnique(IntType);

        ClaferSolver solver = ClaferCompiler.compile(model, Scope.defaultScope(2).intLow(-2).intHigh(2).toScope());
        assertEquals(20, solver.allInstances().length);
    }

    @Test(timeout = 60000)
    public void testTopLevelRefs() {
        AstModel model = newModel();

        model.addChild("Age").withCard(2, 2).refTo(IntType);

        ClaferSolver solver = ClaferCompiler.compile(model, Scope.defaultScope(2).intLow(-2).intHigh(2).toScope());
        assertEquals(25, solver.allInstances().length);
    }

    @Test(timeout = 60000)
    public void testTopLevelUniqueRefs() {
        AstModel model = newModel();

        model.addChild("Age").withCard(2, 2).refToUnique(IntType);

        ClaferSolver solver = ClaferCompiler.compile(model, Scope.defaultScope(2).intLow(-2).intHigh(2).toScope());
        assertEquals(20, solver.allInstances().length);
    }
}
