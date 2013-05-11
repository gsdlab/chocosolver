package org.clafer;

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
    public void testVariableJoin() {
        AstModel model = newModel();

        AstConcreteClafer person = model.addTopClafer("Person").withCard(1, 1);
        AstConcreteClafer hand = person.addChild("Hand");
        AstConcreteClafer finger = hand.addChild("Finger");
        person.addConstraint(equal(card(join(join($this(), hand), finger)), constant(3)));

        ClaferSolver solver = ClaferCompiler.compile(model, new Scope(3));
        assertEquals(6, solver.allInstances().length);
    }
}
