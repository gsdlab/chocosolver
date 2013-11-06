package org.clafer.ast.analysis;

import java.util.HashMap;
import java.util.Map;
import org.clafer.ast.AstAbstractClafer;
import org.clafer.ast.AstClafer;
import org.clafer.ast.AstConcreteClafer;
import org.clafer.ast.AstModel;
import org.clafer.ast.Asts;
import org.clafer.ast.Card;
import org.clafer.scope.Scope;
import static org.junit.Assert.*;
import org.junit.Test;

/**
 *
 * @author jimmy
 */
public class CardAnalyzerTest {

    @Test
    public void testAnalyze() {
        AstModel model = Asts.newModel();

        AstAbstractClafer animal = model.addAbstract("animal");
        AstConcreteClafer limb = animal.addChild("limb").withCard(4);
        AstConcreteClafer digit = limb.addChild("digit").withCard(3);
        AstConcreteClafer parrot = model.addChild("parrot").withCard(3).extending(animal);
        AstConcreteClafer beak = parrot.addChild("beak").withCard(1, 1);

        Map<AstClafer, Card> globalCards = new HashMap<>();
        globalCards.put(animal, new Card(6, 7));
        globalCards.put(limb, new Card(30, 40));
        globalCards.put(digit, new Card(100, 200));
        globalCards.put(parrot, new Card(3, 4));
        globalCards.put(beak, new Card(1, 15));
        globalCards.put(model.getTypeHierarchyRoot(), new Card(1, 1));

        Analysis analysis =
                new CardAnalyzer().analyze(
                new Analysis(model, Scope.defaultScope(1).toScope()).setGlobalCardMap(globalCards));

        assertEquals(new Card(4, 20), analysis.getCard(limb));
        assertEquals(new Card(3, 113), analysis.getCard(digit));
        assertEquals(new Card(3, 4), analysis.getCard(parrot));
        assertEquals(new Card(1, 1), analysis.getCard(beak));
    }
}
