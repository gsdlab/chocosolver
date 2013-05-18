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

        AstConcreteClafer age = model.addTopClafer("Age").withCard(2, 2).refTo(IntType);
        age.addConstraint(equal(joinRef(global(age)), constant(3)));

        ClaferSolver solver = ClaferCompiler.compile(model, new Scope(2));
        assertEquals(1, solver.allInstances().length);
    }

    @Test
    public void testVariableJoin() {
        AstModel model = newModel();

        AstConcreteClafer person = model.addTopClafer("Person").withCard(1, 1);
        AstConcreteClafer hand = person.addChild("Hand");
        AstConcreteClafer finger = hand.addChild("Finger");
        person.addConstraint(equal(card(join(join($this(), hand), finger)), constant(3)));

        ClaferSolver solver = ClaferCompiler.compile(model, new Scope(3));
        assertEquals(6, solver.allInstances().length);
    }

    @Test
    public void testFixedJoinRef() {
        AstModel model = newModel();

        AstConcreteClafer age = model.addTopClafer("Age").withCard(2, 2).refTo(IntType);
        age.addConstraint(equal(joinRef($this()), constant(3)));

        ClaferSolver solver = ClaferCompiler.compile(model, new Scope(2));
        assertEquals(1, solver.allInstances().length);
    }

    @Test
    public void testFixedJoinAndJoinRefOverAbstract() {
        AstModel model = newModel();

        AstAbstractClafer feature = model.addAbstractClafer("Feature");
        AstConcreteClafer cost = feature.addChild("Cost").withCard(1, 1).refToUnique(IntType);
        AstConcreteClafer backup = model.addTopClafer("Backup").withCard(0, 1).extending(feature);
        AstConcreteClafer firewall = model.addTopClafer("Firewall").withCard(0, 1).extending(feature);
        backup.addConstraint(equal(joinRef(join($this(), cost)), constant(3)));
        firewall.addConstraint(equal(joinRef(join($this(), cost)), constant(5)));

        ClaferSolver solver = ClaferCompiler.compile(model, new Scope(2));
        assertEquals(4, solver.allInstances().length);
    }
}
