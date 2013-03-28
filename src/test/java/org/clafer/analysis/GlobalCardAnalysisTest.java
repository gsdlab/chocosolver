package org.clafer.analysis;

import java.util.Map;
import org.clafer.Scope;
import org.clafer.ast.Asts;
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
public class GlobalCardAnalysisTest {

    @Test
    public void testAnalyze() {
        AstModel model = Asts.newModel();

        AstAbstractClafer object = model.addAbstractClafer("object");
        AstConcreteClafer id = object.addChild("id").withCard(new Card(1, 1));
        
        AstAbstractClafer animal = model.addAbstractClafer("animal").extending(object);
        AstAbstractClafer mammal = model.addAbstractClafer("mammal").extending(animal);
        AstAbstractClafer primate = model.addAbstractClafer("primate").extending(mammal);
        AstConcreteClafer arm = primate.addChild("arm").withCard(2, 2).extending(object);
        AstAbstractClafer human = model.addAbstractClafer("human").extending(primate);
        AstConcreteClafer jimmy = model.addTopClafer("Jimmy").extending(human).withCard(0, 1);
        AstConcreteClafer degree = jimmy.addChild("Degree@UWaterloo").extending(object).withCard(1, 1);
        AstConcreteClafer mona = model.addTopClafer("Mona").extending(human).withCard(1, 1);
        AstConcreteClafer lisa = model.addTopClafer("Lisa").extending(human).withCard(1, 1);
        AstConcreteClafer knut = model.addTopClafer("Knut").extending(mammal).withCard(0, 1);

        AstAbstractClafer art = model.addAbstractClafer("art").extending(object);
        AstConcreteClafer monalisa = model.addTopClafer("Mona Lisa").extending(art).withCard(1, 3); // forgeries

        Scope scope = new Scope(10);

        Map<AstClafer, Card> analysis = GlobalCardAnalysis.analyze(model, scope);
        
        System.out.println(analysis);
        assertEquals(new Card(7, 14), analysis.get(object));
        assertEquals(new Card(7, 10), analysis.get(id));
        
        assertEquals(new Card(2, 4), analysis.get(animal));
        assertEquals(new Card(2, 4), analysis.get(mammal));
        assertEquals(new Card(2, 3), analysis.get(primate));
        assertEquals(new Card(4, 6), analysis.get(arm));
        assertEquals(new Card(2, 3), analysis.get(human));
        assertEquals(new Card(0, 1), analysis.get(jimmy));
        assertEquals(new Card(0, 1), analysis.get(degree));
        assertEquals(new Card(1, 1), analysis.get(mona));
        assertEquals(new Card(1, 1), analysis.get(lisa));
        assertEquals(new Card(0, 1), analysis.get(knut));

        assertEquals(new Card(1, 3), analysis.get(art));
        assertEquals(new Card(1, 3), analysis.get(monalisa));
    }
}
