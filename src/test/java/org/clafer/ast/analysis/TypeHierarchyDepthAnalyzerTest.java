package org.clafer.ast.analysis;

import java.util.Map;
import org.clafer.ast.Asts;
import org.clafer.ast.AstAbstractClafer;
import org.clafer.ast.AstModel;
import org.clafer.scope.Scope;
import static org.junit.Assert.*;
import org.junit.Test;

/**
 *
 * @author jimmy
 */
public class TypeHierarchyDepthAnalyzerTest {

    @Test
    public void testAnalyze() {
        AstModel model = Asts.newModel();

        AstAbstractClafer object = model.addAbstractClafer("object");
        
        AstAbstractClafer animal = model.addAbstractClafer("animal").extending(object);
        AstAbstractClafer mammal = model.addAbstractClafer("mammal").extending(animal);
        AstAbstractClafer primate = model.addAbstractClafer("primate").extending(mammal);
        AstAbstractClafer human = model.addAbstractClafer("human").extending(primate);
        model.addChild("Jimmy").extending(human);

        AstAbstractClafer art = model.addAbstractClafer("art").extending(object);

        Analysis analysis = new TypeHierarchyDepthAnalyzer().analyze(new Analysis(model, Scope.defaultScope(1).toScope()));

        assertEquals(1, analysis.getDepth(object));
        assertEquals(2,  analysis.getDepth(animal));
        assertEquals(3, analysis.getDepth(mammal));
        assertEquals(4,  analysis.getDepth(primate));
        assertEquals(2,  analysis.getDepth(art));
    }
}
