package org.clafer.choco.constraint;

import org.clafer.collection.Pair;
import org.clafer.common.Util;
import static org.junit.Assert.*;
import org.junit.Test;
import solver.Solver;
import solver.constraints.Constraint;
import solver.variables.IntVar;
import solver.variables.SetVar;
import solver.variables.VF;

/**
 *
 * @author jimmy
 */
public class IntChannelTest extends ConstraintTest<Pair<SetVar[], IntVar[]>> {

    @Override
    protected void check(Pair<SetVar[], IntVar[]> s) {
        SetVar[] sets = s.getFst();
        IntVar[] ints = s.getSnd();
        for (int i = 0; i < sets.length; i++) {
            for (int j : sets[i].getValue()) {
                assertTrue(j >= 0 && j < ints.length);
                assertEquals(i, ints[j].getValue());
            }
        }
        for (int i = 0; i < ints.length; i++) {
            int value = ints[i].getValue();
            assertTrue(value >= 0 && value < sets.length);
            assertTrue(Util.in(i, sets[ints[i].getValue()].getValue()));
        }
    }

    @Test(timeout = 60000)
    public void quickTest() {
        randomizedTest(new TestCase<Pair<SetVar[], IntVar[]>>() {
            @Override
            public Pair<Constraint, Pair<SetVar[], IntVar[]>> setup(Solver solver) {
                SetVar[] sets = toSetVars(randPositiveSets(nextInt(3) + 1), solver);
                IntVar[] ints = toVars(randPositiveInts(nextInt(3) + 1), solver);
                return pair(Constraints.intChannel(sets, ints), pair(sets, ints));
            }
        });
    }

    @Test(timeout = 60000)
    public void testIntChannel() {
        randomizedTest(new TestCase<Pair<SetVar[], IntVar[]>>() {
            /**
             * positive = 3^3 negative = ((2^3)^3)*(3^3) - positive
             */
            @PositiveSolutions(27)
            @NegativeSolutions(13797)
            @Override
            public Pair<Constraint, Pair<SetVar[], IntVar[]>> setup(Solver solver) {
                SetVar[] sets = new SetVar[3];
                for (int i = 0; i < sets.length; i++) {
                    sets[i] = VF.set("set_" + i, Util.fromTo(0, 3), solver);
                }
                IntVar[] ints = VF.enumeratedArray("int", 3, Util.fromTo(0, 3), solver);
                return pair(Constraints.intChannel(sets, ints), pair(sets, ints));
            }
        });
    }
}
