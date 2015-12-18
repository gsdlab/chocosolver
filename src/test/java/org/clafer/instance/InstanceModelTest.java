package org.clafer.instance;

import org.clafer.ast.AstConcreteClafer;
import org.clafer.ast.AstModel;
import static org.clafer.ast.Asts.IntType;
import static org.clafer.ast.Asts.newModel;
import static org.junit.Assert.*;
import org.junit.Test;

/**
 *
 * @author jimmy
 */
public class InstanceModelTest {

    @Test
    public void testToString() {
        AstModel model = newModel();
        AstConcreteClafer feature = model.addChild("Feature").withCard(1, 1);
        AstConcreteClafer cost = feature.addChild("Cost").withCard(1, 1).refTo(IntType);

        InstanceClafer costInstance = new InstanceClafer(cost, 0, 3);
        InstanceClafer featureInstance = new InstanceClafer(feature, 0, null, costInstance);
        InstanceModel modelInstance = new InstanceModel(featureInstance);

        assertEquals("Feature\n  Cost -> 3\n", modelInstance.toString());
    }
}
