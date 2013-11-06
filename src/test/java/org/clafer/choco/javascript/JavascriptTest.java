package org.clafer.choco.javascript;

import java.io.IOException;
import org.clafer.ast.AstAbstractClafer;
import org.clafer.ast.AstConcreteClafer;
import org.clafer.ast.AstModel;
import org.clafer.ast.Asts;
import org.clafer.ast.Card;
import org.clafer.collection.Triple;
import org.clafer.javascript.Javascript;
import org.clafer.objective.Objective;
import org.clafer.scope.Scope;
import static org.junit.Assert.*;
import org.junit.Test;

/**
 *
 * @author jimmy
 */
public class JavascriptTest {

    @Test
    public void testReadModel() throws IOException {
        Triple<AstModel, Scope, Objective[]> triple = Javascript.readModel(
                "scope({A:3, B:2, C:1})\n"
                + "defaultScope(2)\n"
                + "intRange(-10, 20);\n"
                + "A = Clafer('A').withCard(1,1).refTo(Int)\n"
                + "B = Abstract('B')\n"
                + "C = B.addChild('C').extending(B).withGroupCard(0, 1)\n"
                + "min(global(A));");
        AstModel model = triple.getFst();
        Scope scope = triple.getSnd();

        assertEquals(1, model.getChildren().size());
        // The first is the implicit "clafer" Clafer
        assertEquals(2, model.getAbstracts().size());

        AstConcreteClafer a = model.getChildren().get(0);
        assertEquals("A", a.getName());
        assertEquals(new Card(1, 1), a.getCard());
        assertEquals(Asts.IntType, a.getRef().getTargetType());
        assertFalse(a.getRef().isUnique());

        AstAbstractClafer b = model.getAbstracts().get(1);
        assertEquals("B", b.getName());
        assertEquals(1, b.getChildren().size());

        AstConcreteClafer c = b.getChildren().get(0);
        assertEquals("C", c.getName());
        assertEquals(b, c.getSuperClafer());
        assertEquals(new Card(0, 1), c.getGroupCard());

        assertEquals(3, scope.getScope(a));
        assertEquals(2, scope.getScope(b));
        assertEquals(1, scope.getScope(c));
        assertEquals(2, scope.getDefaultScope());
        assertEquals(-10, scope.getIntLow());
        assertEquals(20, scope.getIntHigh());
        
        Objective[] objectives = triple.getThd();
        assertEquals(1, objectives.length);
        assertFalse(objectives[0].isMaximize());
        assertTrue(objectives[0].isMinimize());
        assertEquals(Asts.global(a), objectives[0].getExpr());
    }
}
