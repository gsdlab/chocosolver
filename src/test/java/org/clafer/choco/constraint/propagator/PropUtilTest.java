package org.clafer.choco.constraint.propagator;

import gnu.trove.set.hash.TIntHashSet;
import java.util.Arrays;
import java.util.Random;
import static org.junit.Assert.*;
import org.junit.Test;
import solver.Solver;
import solver.variables.IntVar;
import solver.variables.SetVar;
import solver.variables.VF;

/**
 *
 * @author jimmy
 */
public class PropUtilTest {

    private final Random rand = new Random();
    private static final int problemSize = 10;

    private boolean canIntersectBruteForce(IntVar i1, IntVar i2) {
        for (int i = i1.getLB(); i <= i1.getUB(); i = i1.nextValue(i)) {
            if (i2.contains(i)) {
                return true;
            }
        }
        return false;
    }

    private boolean canIntersectBruteForce(IntVar i1, SetVar s2) {
        for (int i = s2.getEnvelopeFirst(); i != SetVar.END; i = s2.getEnvelopeNext()) {
            if (i1.contains(i)) {
                return true;
            }
        }
        return false;
    }

    private int randInt() {
        return rand.nextInt(problemSize * 2 + 1) - problemSize;
    }

    private IntVar randIntVar(String name, Solver solver) {
        int size = rand.nextInt(problemSize) + 1;
        TIntHashSet domain = new TIntHashSet(size);
        for (int i = 0; i < size; i++) {
            domain.add(randInt());
        }
        int[] domainArray = domain.toArray();
        Arrays.sort(domainArray);
        return VF.enumerated(name, domainArray, solver);
    }

    private SetVar randSetVar(String name, Solver solver) {
        int size = rand.nextInt(problemSize) + 1;
        TIntHashSet domain = new TIntHashSet(size);
        for (int i = 0; i < size; i++) {
            domain.add(randInt());
        }
        int[] domainArray = domain.toArray();
        Arrays.sort(domainArray);
        return VF.set(name, domainArray, solver);
    }

    @Test
    public void testDomainIntersectDomain() {
        Solver solver = new Solver();
        for (int i = 0; i < 100; i++) {
            IntVar i1 = randIntVar("i1" + i, solver);
            IntVar i2 = randIntVar("i2" + i, solver);

            assertEquals(
                    i1 + " intersect " + i2,
                    canIntersectBruteForce(i1, i2), PropUtil.domainIntersectDomain(i1, i2));
        }
    }

    @Test
    public void testDomainIntersectDomainOnUB() {
        Solver solver = new Solver();
        IntVar i1 = VF.enumerated("i1", new int[]{-10, -8, 9}, solver);
        IntVar i2 = VF.enumerated("i2", new int[]{-4, 0, 3, 9}, solver);

        assertTrue(PropUtil.domainIntersectDomain(i1, i2));
    }

    @Test
    public void testDomainIntersectEnv() {
        Solver solver = new Solver();
        for (int i = 0; i < 100; i++) {
            IntVar i1 = randIntVar("i1" + i, solver);
            SetVar s2 = randSetVar("s2" + i, solver);

            assertEquals(
                    i1 + " intersect " + s2,
                    canIntersectBruteForce(i1, s2), PropUtil.domainIntersectEnv(i1, s2));
        }
    }
}
