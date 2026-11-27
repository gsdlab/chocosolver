package org.clafer.ontology;

import static org.junit.Assert.assertNotNull;
import org.junit.Test;

/**
 * @author Michał Antkiewicz
 */
public class OracleTest {

    /**
     * A local equality whose right side has no ground paths in one of the
     * contexts of its left side: A.x is grounded in A1 and A2, but A1.x only in
     * A1. There is nothing to equate in A2.
     */
    @Test
    public void testLocalEqualityWithMissingContext() {
        KnowledgeDatabase database = new KnowledgeDatabase();
        Concept a = database.newConcept("A");
        Concept a1 = database.newConcept("A1");
        Concept a2 = database.newConcept("A2");
        Concept x = database.newConcept("x");
        database.newIsA(a1, a);
        database.newIsA(a2, a);
        database.newHasA(a, x);
        database.newLocalEquality(new Path(a, x), new Path(a1, x));

        assertNotNull(database.oracle());
    }
}
