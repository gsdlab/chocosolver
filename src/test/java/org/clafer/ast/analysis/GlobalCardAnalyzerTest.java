package org.clafer.ast.analysis;

import org.clafer.scope.Scope;
import org.clafer.ast.Asts;
import org.clafer.ast.AstAbstractClafer;
import org.clafer.ast.AstConcreteClafer;
import org.clafer.ast.AstModel;
import org.clafer.ast.Card;
import static org.junit.Assert.*;
import org.junit.Test;

/**
 *
 * @author jimmy
 */
public class GlobalCardAnalyzerTest {

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
        AstConcreteClafer jimmy = model.addChild("Jimmy").extending(human).withCard(0, 1);
        AstConcreteClafer degree = jimmy.addChild("Degree@UWaterloo").extending(object).withCard(1, 1);
        AstConcreteClafer mona = model.addChild("Mona").extending(human).withCard(1, 1);
        AstConcreteClafer lisa = model.addChild("Lisa").extending(human).withCard(1, 1);
        AstConcreteClafer knut = model.addChild("Knut").extending(mammal).withCard(0, 1);

        AstAbstractClafer art = model.addAbstractClafer("art").extending(object);
        AstConcreteClafer monalisa = model.addChild("Mona Lisa").extending(art).withCard(1, 3); // forgeries

        Scope scope = Scope.defaultScope(10).toScope();

        Analysis analysis = Analysis.analyze(model, scope,
                new TypeHierarchyDepthAnalyzer(),
                new GlobalCardAnalyzer());

        assertEquals(new Card(7, 14), analysis.getGlobalCard(object));
        assertEquals(new Card(7, 10), analysis.getGlobalCard(id));

        assertEquals(new Card(2, 4), analysis.getGlobalCard(animal));
        assertEquals(new Card(2, 4), analysis.getGlobalCard(mammal));
        assertEquals(new Card(2, 3), analysis.getGlobalCard(primate));
        assertEquals(new Card(4, 6), analysis.getGlobalCard(arm));
        assertEquals(new Card(2, 3), analysis.getGlobalCard(human));
        assertEquals(new Card(0, 1), analysis.getGlobalCard(jimmy));
        assertEquals(new Card(0, 1), analysis.getGlobalCard(degree));
        assertEquals(new Card(1, 1), analysis.getGlobalCard(mona));
        assertEquals(new Card(1, 1), analysis.getGlobalCard(lisa));
        assertEquals(new Card(0, 1), analysis.getGlobalCard(knut));

        assertEquals(new Card(1, 3), analysis.getGlobalCard(art));
        assertEquals(new Card(1, 3), analysis.getGlobalCard(monalisa));
    }
}
