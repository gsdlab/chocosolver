package org.clafer.ast;

import java.util.List;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import org.junit.Test;

/**
 *
 * @author jimmy
 */
public class AstUtilTest {

    @Test
    public void testGetClafers() {
        AstModel model = Asts.newModel();
        AstAbstractClafer animal = model.addAbstract("Animal");
        AstAbstractClafer limb = animal.addAbstractChild("Limb");
        AstConcreteClafer head = animal.addChild("Head");
        AstConcreteClafer dog = model.addChild("Dog").extending(animal);
        AstConcreteClafer leg = dog.addChild("Leg").extending(limb);
        AstConcreteClafer tail = dog.addChild("Tail");

        List<AstClafer> clafers = AstUtil.getClafers(model);
        assertEquals(9, clafers.size());
        assertTrue(clafers.contains(model.getAbstractRoot()));
        assertTrue(clafers.contains(model.getRoot()));
        assertTrue(clafers.contains(model.getTypeRoot()));
        assertTrue(clafers.contains(animal));
        assertTrue(clafers.contains(limb));
        assertTrue(clafers.contains(head));
        assertTrue(clafers.contains(dog));
        assertTrue(clafers.contains(leg));
        assertTrue(clafers.contains(tail));
    }
}
