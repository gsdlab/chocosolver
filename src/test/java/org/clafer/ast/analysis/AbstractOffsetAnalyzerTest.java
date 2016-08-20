package org.clafer.ast.analysis;

import org.clafer.ast.AstAbstractClafer;
import org.clafer.ast.AstClafer;
import org.clafer.ast.AstConcreteClafer;
import org.clafer.ast.AstModel;
import org.clafer.ast.Asts;
import org.clafer.ast.Card;
import org.clafer.scope.Scope;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import org.junit.Test;

/**
 *
 * @author jimmy
 */
public class AbstractOffsetAnalyzerTest {

    /**
     * <pre>
     * abstract Object
     *     Id
     * abstract Animal : Object
     * abstract Mammal : Animal
     * abstract Primate : Mammal
     *     Arm 2
     * abstract Human : Primate
     * Jimmy : Human ?
     *     UWaterlooDegree
     * Mona : Human
     * Lisa : Human
     * Knut : Mammal ?
     *
     * abstract Art : Object
     * MonaLisa : Art 1..3
     * </pre>
     */
    @Test
    public void testAnalyze() {
        AstModel model = Asts.newModel();

        AstAbstractClafer object = model.addAbstract("Object");
        AstConcreteClafer id = object.addChild("Id").withCard(new Card(1, 1));

        AstAbstractClafer animal = model.addAbstract("Animal").extending(object);
        AstAbstractClafer mammal = model.addAbstract("Mammal").extending(animal);
        AstAbstractClafer primate = model.addAbstract("Primate").extending(mammal);
        AstConcreteClafer arm = primate.addChild("Arm").withCard(2, 2).extending(object);
        AstAbstractClafer human = model.addAbstract("Human").extending(primate);
        AstConcreteClafer jimmy = model.addChild("Jimmy").extending(human).withCard(0, 1);
        AstConcreteClafer degree = jimmy.addChild("UWaterlooDegree").extending(object).withCard(1, 1);
        AstConcreteClafer mona = model.addChild("Mona").extending(human).withCard(1, 1);
        AstConcreteClafer lisa = model.addChild("Lisa").extending(human).withCard(1, 1);
        AstConcreteClafer knut = model.addChild("Knut").extending(mammal).withCard(0, 1);

        AstAbstractClafer art = model.addAbstract("Art").extending(object);
        AstConcreteClafer monalisa = model.addChild("MonaLisa").extending(art).withCard(1, 3); // forgeries

        Scope scope = Scope.defaultScope(10).toScope();

        Analysis analysis = Analysis.analyze(model, scope,
                new GlobalCardAnalyzer(),
                new AbstractOffsetAnalyzer());

        Offsets offsets = analysis.getOffsets(object);
        for (AstClafer sub : object.getSubs()) {
            assertEquals(sub, offsets.getClafer(offsets.getOffset(sub)));
        }
        for (int i = 0; i < scope.getScope(object); i++) {
            assertTrue(i >= offsets.getOffset(offsets.getClafer(i)));
        }
    }
}
