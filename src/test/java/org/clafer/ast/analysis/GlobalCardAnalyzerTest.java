package org.clafer.ast.analysis;

import org.clafer.ast.AstAbstractClafer;
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
public class GlobalCardAnalyzerTest {

    @Test
    public void testAnalyze() {
        AstModel model = Asts.newModel();

        AstAbstractClafer object = model.addAbstract("object");
        AstConcreteClafer id = object.addChild("id").withCard(new Card(1, 1));

        AstAbstractClafer animal = model.addAbstract("animal").extending(object);
        AstAbstractClafer mammal = model.addAbstract("mammal").extending(animal);
        AstAbstractClafer primate = model.addAbstract("primate").extending(mammal);
        AstConcreteClafer arm = primate.addChild("arm").withCard(2, 2).extending(object);
        AstAbstractClafer human = model.addAbstract("human").extending(primate);
        AstConcreteClafer jimmy = model.addChild("Jimmy").extending(human).withCard(0, 1);
        AstConcreteClafer degree = jimmy.addChild("Degree@UWaterloo").extending(object).withCard(1, 1);
        AstConcreteClafer mona = model.addChild("Mona").extending(human).withCard(1, 1);
        AstConcreteClafer lisa = model.addChild("Lisa").extending(human).withCard(1, 1);
        AstConcreteClafer knut = model.addChild("Knut").extending(mammal).withCard(0, 1);

        AstAbstractClafer art = model.addAbstract("art").extending(object);
        AstConcreteClafer monalisa = model.addChild("Mona Lisa").extending(art).withCard(1, 3); // forgeries

        Scope scope = Scope.defaultScope(10).toScope();

        Analysis analysis = Analysis.analyze(model, scope, new GlobalCardAnalyzer());

        assertEquals(new Card(7, 10), analysis.getGlobalCard(object));
        assertEquals(new Card(7, 10), analysis.getGlobalCard(id));

        assertEquals(new Card(2, 3), analysis.getGlobalCard(animal));
        assertEquals(new Card(2, 3), analysis.getGlobalCard(mammal));
        assertEquals(new Card(2, 2), analysis.getGlobalCard(primate));
        assertEquals(new Card(4, 4), analysis.getGlobalCard(arm));
        assertEquals(new Card(2, 2), analysis.getGlobalCard(human));
        assertEquals(new Card(0, 0), analysis.getGlobalCard(jimmy));
        assertEquals(new Card(0, 0), analysis.getGlobalCard(degree));
        assertEquals(new Card(1, 1), analysis.getGlobalCard(mona));
        assertEquals(new Card(1, 1), analysis.getGlobalCard(lisa));
        assertEquals(new Card(0, 1), analysis.getGlobalCard(knut));

        assertEquals(new Card(1, 3), analysis.getGlobalCard(art));
        assertEquals(new Card(1, 3), analysis.getGlobalCard(monalisa));
    }

    @Test
    public void testOptimize() {
        AstModel model = Asts.newModel();

        AstConcreteClafer a = model.addChild("A").withCard(1);
        AstConcreteClafer b = a.addChild("B").withCard(2, 2);

        Scope scope = Scope.defaultScope(5).toScope();

        Analysis analysis = Analysis.analyze(model, scope, new GlobalCardAnalyzer());

        assertEquals(new Card(1, 2), analysis.getGlobalCard(a));
        assertEquals(new Card(2, 4), analysis.getGlobalCard(b));
    }

    @Test
    public void testTradeoff() {
        AstModel model = Asts.newModel();

        AstAbstractClafer a = model.addAbstract("A");
        AstConcreteClafer b = a.addChild("B").withCard(2, 2);

        // The scope of B is shared between C and D.
        AstConcreteClafer c = model.addChild("C").extending(a);
        AstConcreteClafer d = model.addChild("D").extending(a);

        Scope scope = Scope.defaultScope(5).toScope();

        Analysis analysis = Analysis.analyze(model, scope, new GlobalCardAnalyzer());

        assertEquals(new Card(0, 2), analysis.getGlobalCard(c));
        assertEquals(new Card(0, 2), analysis.getGlobalCard(d));
    }
}
