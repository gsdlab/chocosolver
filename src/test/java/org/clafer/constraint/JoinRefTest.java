package org.clafer.constraint;

import static org.junit.Assert.*;
import static org.clafer.constraint.JoinRefManager.*;
import choco.Choco;
import choco.Options;
import choco.cp.model.CPModel;
import choco.kernel.model.Model;
import choco.kernel.model.variables.integer.IntegerVariable;
import choco.kernel.model.variables.set.SetVariable;
import choco.kernel.solver.Solver;
import gnu.trove.TIntHashSet;
import java.util.Random;
import org.clafer.Util;
import org.junit.Test;

/**
 *
 * @author jimmy
 */
public class JoinRefTest extends ConstraintTest {

    private void checkCorrectness(Solver solver, SetVariable take, IntegerVariable[] refs, SetVariable to) {
        int[] $from = solver.getVar(take).getValue();
        int[] $refs = Util.getVals(solver.getVar(refs));
        int[] $to = solver.getVar(to).getValue();

        TIntHashSet set = new TIntHashSet();

        for (int f : $from) {
            assertTrue(Util.in($refs[f], $to));
            set.add($refs[f]);
        }
        assertEquals(set.size(), $to.length);
    }

    @Test
    public void testJoinRef() {
        Random rand = new Random();
        for (int rr = 0; rr < 10; rr++) {
            Model m = new CPModel();

            SetVariable take = Choco.makeSetVar("take", 0, rand.nextInt(50));
            IntegerVariable[] refs = Choco.makeIntVarArray("ref", 1 + rand.nextInt(50), 0, rand.nextInt(50));
            SetVariable to = Choco.makeSetVar("to", 0, rand.nextInt(50));

            m.addConstraint(joinRef(take, refs, to));

            for (int repeat = 0; repeat < 10; repeat++) {
                Solver solver = solveRandomly(m, false);
                checkCorrectness(solver, take, refs, to);
            }
        }
    }

    @Test
    public void testJoinLargeDomain() {
        Random rand = new Random();
        Model m = new CPModel();

        SetVariable take = Choco.makeSetVar("take", 0, rand.nextInt(10));
        IntegerVariable[] refs = Choco.makeIntVarArray("ref", 1 + rand.nextInt(10), 0, 1000000, Options.V_ENUM);
        SetVariable to = Choco.makeSetVar("to", 0, 1000000, Options.V_NO_DECISION);

        m.addConstraint(joinRef(take, refs, to));

        Solver solver = solveOnce(m);
        checkCorrectness(solver, take, refs, to);
    }

    @Test
    public void quickTest() {
        Model m = new CPModel();

        SetVariable take = Choco.makeSetVar("take", 0, 4);
        IntegerVariable[] refs = Choco.makeIntVarArray("ref", 3, 0, 4);
        SetVariable to = Choco.makeSetVar("to", 0, 4);

        m.addConstraint(joinRef(take, refs, to));

        assertEquals(1000, quickCheckModel(m, 10));
    }
}
