package org.clafer.analysis;

import java.util.HashMap;
import java.util.Map;
import org.clafer.ast.Ast;
import org.clafer.ast.AstAbstractClafer;
import org.clafer.ast.AstClafer;
import org.clafer.ast.AstConcreteClafer;
import org.clafer.ast.AstModel;
import org.clafer.ast.Card;
import static org.junit.Assert.*;
import org.junit.Test;

/**
 *
 * @author jimmy
 */
public class CardAnalysisTest {

    @Test
    public void testAnalyze() {
        AstModel model = Ast.newModel();

        AstAbstractClafer animal = model.addAbstractClafer("animal");
        AstConcreteClafer limb = animal.addChild("limb").withCard(4);
        AstConcreteClafer digit = limb.addChild("digit").withCard(3);
        AstConcreteClafer parrot = model.addTopClafer("parrot").withCard(3).extending(animal);
        AstConcreteClafer beak = parrot.addChild("beak").withCard(1, 1);

        Map<AstClafer, Card> globalCards = new HashMap<AstClafer, Card>();
        globalCards.put(animal, new Card(6, 7));
        globalCards.put(limb, new Card(30, 40));
        globalCards.put(digit, new Card(100, 200));
        globalCards.put(parrot, new Card(3, 4));
        globalCards.put(beak, new Card(1, 15));

        CardAnalysis.analyze(model, globalCards);

        assertEquals(new Card(4, 20), limb.getCard());
        assertEquals(new Card(3, 113), digit.getCard());
        assertEquals(new Card(3, 4), parrot.getCard());
        assertEquals(new Card(1, 1), beak.getCard());
    }
}
