package org.clafer.math;

import org.clafer.domain.Domains;
import static org.clafer.math.SetTheory.or;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
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
        assertEquals(Domains.boundDomain(-1, 1), theory.getEnv(1));
        assertEquals(Domains.boundDomain(-1, 1), theory.getEnv(2));
    }

    @Test
    public void testSubsetPropagate() {
        SetTheory theory = new SetTheory();

        theory.subset(1, Domains.boundDomain(-1, 5));
        theory.subset(2, 1);

        assertTrue(theory.propagate());
        assertEquals(Domains.boundDomain(-1, 5), theory.getEnv(2));
    }

    @Test
    public void testUnionPropagate() {
        SetTheory theory = new SetTheory();

        theory.subset(1, Domains.boundDomain(-1, 1));
        theory.subset(2, Domains.boundDomain(3, 4));
        theory.subset(3, Domains.boundDomain(4, 6));
        theory.subset(4, Domains.boundDomain(0, 5));
        theory.union(4, new int[]{1, 2, 3});

        assertTrue(theory.propagate());
        assertEquals(Domains.boundDomain(0, 1), theory.getEnv(1));
        assertEquals(Domains.boundDomain(3, 4), theory.getEnv(2));
        assertEquals(Domains.boundDomain(4, 5), theory.getEnv(3));
        assertEquals(Domains.enumDomain(0, 1, 3, 4, 5), theory.getEnv(4));
    }

    @Test
    public void testConstructiveDisjunctionPropagate() {
        SetTheory theory = new SetTheory();

        theory.constructiveDisjunction(
                or().subset(1, Domains.OneDomain),
                or().subset(1, Domains.ZeroDomain));

        assertTrue(theory.propagate());
        assertEquals(Domains.boundDomain(0, 1), theory.getEnv(1));
    }

    @Test
    public void testTwoConstructiveDisjunctionPropagate() {
        SetTheory theory = new SetTheory();

        theory.constructiveDisjunction(
                or().subset(1, Domains.OneDomain),
                or().subset(1, Domains.ZeroDomain));
        theory.constructiveDisjunction(
                or().subset(2, Domains.NegativeOneDomain),
                or().subset(2, 1));

        assertTrue(theory.propagate());
        assertEquals(Domains.boundDomain(0, 1), theory.getEnv(1));
        assertEquals(Domains.boundDomain(-1, 1), theory.getEnv(2));
    }

    @Test
    public void testThreeConstructiveDisjunctionPropagate() {
        SetTheory theory = new SetTheory();

        theory.constructiveDisjunction(
                or().subset(1, Domains.OneDomain),
                or().subset(1, Domains.ZeroDomain));
        theory.constructiveDisjunction(
                or().subset(2, Domains.NegativeOneDomain),
                or().subset(2, 1));
        theory.constructiveDisjunction(
                or().subset(3, 2).subset(4, 3).subset(5, Domains.OneDomain),
                or().subset(3, Domains.constantDomain(2)).subset(4, 2));

        assertTrue(theory.propagate());
        assertEquals(Domains.boundDomain(0, 1), theory.getEnv(1));
        assertEquals(Domains.boundDomain(-1, 1), theory.getEnv(2));
        assertEquals(Domains.boundDomain(-1, 2), theory.getEnv(3));
        assertEquals(Domains.boundDomain(-1, 1), theory.getEnv(4));
        assertNull(theory.getEnv(5));
    }

    @Test
    public void testConstructiveDisjunctionWithHardConstraintsPropagate() {
        SetTheory theory = new SetTheory();

        theory.subset(3, 4);
        theory.constructiveDisjunction(
                or().subset(1, Domains.OneDomain),
                or().subset(1, Domains.ZeroDomain));
        theory.constructiveDisjunction(
                or().subset(2, Domains.NegativeOneDomain),
                or().subset(2, 3).subset(4, 1));

        assertTrue(theory.propagate());
        assertEquals(Domains.boundDomain(0, 1), theory.getEnv(1));
        assertEquals(Domains.boundDomain(-1, 1), theory.getEnv(2));
        assertNull(theory.getEnv(3));
        assertNull(theory.getEnv(4));
    }
}
