package org.clafer.choco.javascript;

import javax.script.ScriptException;
import org.clafer.ast.AstAbstractClafer;
import org.clafer.ast.AstConcreteClafer;
import org.clafer.ast.AstModel;
import org.clafer.ast.Card;
import org.clafer.javascript.Javascript;
import static org.junit.Assert.*;
import org.junit.Test;

/**
 *
 * @author jimmy
 */
public class JavascriptTest {

    @Test
    public void testReadModel() throws ScriptException {
        AstModel model = Javascript.readModel(
                "a = clafer('A').withCard(1,1)\n"
                + "b = abstract('B')\n"
                + "c = b.addChild('C').extending(b).withGroupCard(0, 1)");
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
    }
}
