package org.clafer.math;

import org.clafer.domain.Domains;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import org.junit.Test;

/**
 *
 * @author jimmy
 */
public class SetTheoryTest {

    @Test
    public void testEqualPropagate() {
        SetTheory theory = new SetTheory();

        theory.subset(1, Domains.boundDomain(-1, 5));
        theory.subset(2, Domains.boundDomain(-5, 1));
        theory.equal(1, 2);

        assertTrue(theory.propagate());
        assertEquals(theory.getEnv(1), Domains.boundDomain(-1, 1));
        assertEquals(theory.getEnv(2), Domains.boundDomain(-1, 1));
    }

    @Test
    public void testSubsetPropagate() {
        SetTheory theory = new SetTheory();

        theory.subset(1, Domains.boundDomain(-1, 5));
        theory.subset(2, 1);

        assertTrue(theory.propagate());
        assertEquals(theory.getEnv(2), Domains.boundDomain(-1, 5));
    }

    @Test
    public void testUnionPropagate() {
        SetTheory theory = new SetTheory();

        theory.subset(1, Domains.boundDomain(-1, 1));
        theory.subset(2, Domains.boundDomain(3, 4));
        theory.subset(3, Domains.boundDomain(4, 6));
        theory.subset(4, Domains.boundDomain(0, 5));
        theory.union(1, 2, 3).equalsTo(4);

        assertTrue(theory.propagate());
        assertEquals(theory.getEnv(1), Domains.boundDomain(0, 1));
        assertEquals(theory.getEnv(2), Domains.boundDomain(3, 4));
        assertEquals(theory.getEnv(3), Domains.boundDomain(4, 5));
        assertEquals(theory.getEnv(4), Domains.enumDomain(0, 1, 3, 4, 5));
    }
}
