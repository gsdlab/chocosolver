package org.clafer.choco.javascript;

import javax.script.ScriptException;
import org.clafer.ast.AstAbstractClafer;
import org.clafer.ast.AstConcreteClafer;
import org.clafer.ast.AstModel;
import org.clafer.ast.Card;
import org.clafer.collection.Pair;
import org.clafer.javascript.Javascript;
import org.clafer.scope.Scope;
import static org.junit.Assert.*;
import org.junit.Test;

/**
 *
 * @author jimmy
 */
public class JavascriptTest {

    @Test
    public void testReadModel() throws ScriptException {
        Pair<AstModel, Scope> pair = Javascript.readModel(
                "scope({A:3, B:2, C:1})\n"
                + "defaultScope(2)\n"
                + "intRange(-10, 20);\n"
                + "A = clafer('A').withCard(1,1)\n"
                + "B = abstract('B')\n"
                + "C = B.addChild('C').extending(B).withGroupCard(0, 1)");
        AstModel model = pair.getFst();
        Scope scope = pair.getSnd();

        assertEquals(1, model.getChildren().size());
        // The first is the implicit "clafer" Clafer
        assertEquals(2, model.getAbstractClafers().size());

        AstConcreteClafer a = model.getChildren().get(0);
        assertEquals("A", a.getName());
        assertEquals(new Card(1, 1), a.getCard());

        AstAbstractClafer b = model.getAbstractClafers().get(1);
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
    }
}
