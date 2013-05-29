package org.clafer.ast.analysis;

import java.util.Map;
import org.clafer.ast.Asts;
import org.clafer.ast.AstAbstractClafer;
import org.clafer.ast.AstModel;
import static org.junit.Assert.*;
import org.junit.Test;

/**
 *
 * @author jimmy
 */
public class TypeHierarchyDepthAnalysisTest {

//    @Test
//    public void testAnalyze() {
//        AstModel model = Asts.newModel();
//
//        AstAbstractClafer object = model.addAbstractClafer("object");
//        
//        AstAbstractClafer animal = model.addAbstractClafer("animal").extending(object);
//        AstAbstractClafer mammal = model.addAbstractClafer("mammal").extending(animal);
//        AstAbstractClafer primate = model.addAbstractClafer("primate").extending(mammal);
//        AstAbstractClafer human = model.addAbstractClafer("human").extending(primate);
//        model.addChild("Jimmy").extending(human);
//
//        AstAbstractClafer art = model.addAbstractClafer("art").extending(object);
//
//        Map<AstAbstractClafer, Integer> analysis = TypeHierarchyDepthAnalysis.analyze(model);
//
//        assertEquals(1, analysis.get(object).intValue());
//        assertEquals(2, analysis.get(animal).intValue());
//        assertEquals(3, analysis.get(mammal).intValue());
//        assertEquals(4, analysis.get(primate).intValue());
//        assertEquals(2, analysis.get(art).intValue());
//    }
}
