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
public class ScopeAnalyzerTest {

    @Test
    public void testAnalyze() {
        AstModel model = Asts.newModel();

        AstAbstractClafer object = model.addAbstract("object");
        AstConcreteClafer id = object.addChild("id").withCard(new Card(1, 1));
        AstConcreteClafer hash = object.addChild("hash").withCard(new Card(1, 1));
        AstConcreteClafer lock = object.addChild("lock").withCard(new Card(1, 1));

        AstConcreteClafer parrot = model.addChild("parrot").extending(object);
        AstConcreteClafer robin = model.addChild("robin").extending(object);

        Map<AstClafer, Integer> scope = new HashMap<>();
        scope.put(id, 3);
        scope.put(hash, 5);
        scope.put(robin, 3);

        Map<AstClafer, Card> globalCards = new HashMap<>();
        globalCards.put(model, new Card(1, 1));
        globalCards.put(model.getTypeHierarchyRoot(), new Card(1, 1));
        globalCards.put(object, new Card(3));
        globalCards.put(id, new Card(0, 2));
        globalCards.put(hash, new Card(7));
        globalCards.put(lock, new Card(3));
        globalCards.put(parrot, new Card(1, 3));
        globalCards.put(robin, new Card(2, 4));

        Analysis analysis = new ScopeAnalyzer().analyze(
                new Analysis(model,
                        new Scope(scope, 2, -16, 16, -16, 16, 10, Character.MIN_VALUE, Character.MAX_VALUE)
                ).setGlobalCardMap(globalCards));

        assertEquals(5, analysis.getScope(object)); // scope(parrot) + scope(robin)
        assertEquals(5, analysis.getScope(id));
        assertEquals(5, analysis.getScope(hash));
        assertEquals(5, analysis.getScope(lock));

        assertEquals(2, analysis.getScope(parrot)); // default scope
        assertEquals(3, analysis.getScope(robin));
    }
}
