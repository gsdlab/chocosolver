package org.clafer.constraint.propagator;

import gnu.trove.set.hash.TIntHashSet;
import java.util.Arrays;
import java.util.Random;
import org.clafer.collection.Pair;
import static org.junit.Assert.*;
import org.junit.Test;
import solver.Solver;
import solver.variables.IntVar;
import solver.variables.SetVar;
import solver.variables.VariableFactory;

/**
 *
 * @author jimmy
 */
public class PropUtilTest {

    private final Random rand = new Random();
    private static final int problemSize = 100000;

    private boolean canIntersectBruteForce(IntVar e1, SetVar e2) {
        for (int i = e2.getEnvelopeFirst(); i != SetVar.END; i = e2.getEnvelopeNext()) {
            if (e1.contains(i)) {
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
        return VariableFactory.enumerated(name, domainArray, solver);
    }

    private SetVar randSetVar(String name, Solver solver) {
        int size = rand.nextInt(problemSize) + 1;
        TIntHashSet domain = new TIntHashSet(size);
        for (int i = 0; i < size; i++) {
            domain.add(randInt());
        }
        int[] domainArray = domain.toArray();
        Arrays.sort(domainArray);
        return VariableFactory.set(name, domainArray, solver);
    }

    @Test
    public void testCanIntersect() {
        Solver solver = new Solver();
        for (int i = 0; i < 100; i++) {
            IntVar iv = randIntVar("iv" + i, solver);
            SetVar sv = randSetVar("sv" + i, solver);

            assertEquals(canIntersectBruteForce(iv, sv), PropUtil.canIntersect(iv, sv));
        }
    }

    @Test
    public void testMinMaxEnv() {
        Solver solver = new Solver();
        SetVar sv = randSetVar("sv", solver);
        int[] dom = PropUtil.iterateEnv(sv);
        Pair<int[], int[]> minMax = PropUtil.minMaxEnv(sv, 20000);
        if (dom.length <= 20000) {
            assertArrayEquals(dom, minMax.getFst());
            assertArrayEquals(dom, minMax.getSnd());
        } else {
            assertArrayEquals(Arrays.copyOf(dom, 20000), minMax.getFst());
            assertArrayEquals(Arrays.copyOfRange(dom, dom.length - 20000, dom.length), minMax.getSnd());
        }
    }
}
