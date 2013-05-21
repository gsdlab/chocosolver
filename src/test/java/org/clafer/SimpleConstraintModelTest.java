package org.clafer;

import org.clafer.ast.scope.Scope;
import org.clafer.ast.AstAbstractClafer;
import org.clafer.compiler.ClaferCompiler;
import org.clafer.ast.AstConcreteClafer;
import org.clafer.ast.AstModel;
import static org.clafer.ast.Asts.*;
import org.clafer.compiler.ClaferSolver;
import static org.junit.Assert.*;
import org.junit.Test;

/**
 *
 * @author jimmy
 */
public class SimpleConstraintModelTest {

    @Test
    public void testGlobal() {
        AstModel model = newModel();

        AstConcreteClafer age = model.addChild("Age").withCard(2, 2).refTo(IntType);
        age.addConstraint(equal(joinRef(global(age)), constant(3)));

        ClaferSolver solver = ClaferCompiler.compile(model, Scope.defaultScope(2).toScope());
        assertEquals(1, solver.allInstances().length);
    }

    @Test
    public void testVariableJoin() {
        AstModel model = newModel();

        AstConcreteClafer person = model.addChild("Person").withCard(1, 1);
        AstConcreteClafer hand = person.addChild("Hand");
        AstConcreteClafer finger = hand.addChild("Finger");
        person.addConstraint(equal(card(join(join($this(), hand), finger)), constant(3)));

        ClaferSolver solver = ClaferCompiler.compile(model, Scope.defaultScope(3).toScope());
        assertEquals(6, solver.allInstances().length);
    }

    @Test
    public void testFixedJoinRef() {
        AstModel model = newModel();

        AstConcreteClafer age = model.addChild("Age").withCard(2, 2).refTo(IntType);
        age.addConstraint(equal(joinRef($this()), constant(3)));

        ClaferSolver solver = ClaferCompiler.compile(model, Scope.defaultScope(2).toScope());
        assertEquals(1, solver.allInstances().length);
    }

    @Test
    public void testFixedJoinAndJoinRefOverAbstract() {
        AstModel model = newModel();

        AstAbstractClafer feature = model.addAbstractClafer("Feature");
        AstConcreteClafer cost = feature.addChild("Cost").withCard(1, 1).refToUnique(IntType);
        AstConcreteClafer backup = model.addChild("Backup").withCard(0, 1).extending(feature);
        AstConcreteClafer firewall = model.addChild("Firewall").withCard(0, 1).extending(feature);
        backup.addConstraint(equal(joinRef(join($this(), cost)), constant(3)));
        firewall.addConstraint(equal(joinRef(join($this(), cost)), constant(5)));

        ClaferSolver solver = ClaferCompiler.compile(model, Scope.defaultScope(2).toScope());
        assertEquals(4, solver.allInstances().length);
    }

    @Test
    public void testMaybeJoinRef() {
        AstModel model = newModel();

        AstConcreteClafer feature = model.addChild("Feature").withCard(0, 1);
        AstConcreteClafer cost = feature.addChild("Cost").withCard(0, 1).refToUnique(IntType);
        feature.addConstraint(equal(joinRef(join($this(), cost)), constant(3)));

        ClaferSolver solver = ClaferCompiler.compile(model, Scope.defaultScope(2).toScope());
        assertEquals(2, solver.allInstances().length);
    }

    @Test
    public void testJoinRefSingleValue() {
        AstModel model = newModel();

        AstConcreteClafer feature = model.addChild("Feature").withCard(1, 1);
        AstConcreteClafer cost = feature.addChild("Cost").withCard(2, 3).refTo(IntType);
        feature.addConstraint(equal(joinRef(join($this(), cost)), constant(5)));

        ClaferSolver solver = ClaferCompiler.compile(model, Scope.defaultScope(3).toScope());
        assertEquals(2, solver.allInstances().length);
    }
}
