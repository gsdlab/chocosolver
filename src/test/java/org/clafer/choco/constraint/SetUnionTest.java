package org.clafer.choco.constraint;

import gnu.trove.set.TIntSet;
import gnu.trove.set.hash.TIntHashSet;
import org.clafer.common.Util;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import org.junit.Test;
import solver.Solver;
import solver.constraints.set.SCF;
import solver.variables.IntVar;
import solver.variables.SetVar;
import solver.variables.VF;

/**
 *
 * @author jimmy
 */
public class SetUnionTest extends ConstraintTest {

    public void checkCorrectness(SetVar[] sets, SetVar union) {
        TIntSet answer = new TIntHashSet();
        for (SetVar set : sets) {
            answer.addAll(set.getValue());
        }
        assertEquals(new TIntHashSet(union.getValue()), answer);
    }

    @Test(timeout = 60000)
    public void quickTest() {
        for (int repeat = 0; repeat < 10; repeat++) {
            Solver solver = new Solver();

            SetVar[] sets = new SetVar[nextInt(3) + 1];
            IntVar[] setCards = new IntVar[sets.length];
            for (int i = 0; i < sets.length; i++) {
                sets[i] = VF.set("s" + i, -nextInt(10), nextInt(10), solver);
                setCards[i] = VF.enumerated("|s" + i + "|", 0, sets[i].getEnvelopeSize(), solver);
                solver.post(SCF.cardinality(sets[i], setCards[i]));
            }
            SetVar union = VF.set("union", -nextInt(10), nextInt(10), solver);
            IntVar unionCard = VF.enumerated("|union|", 0, union.getEnvelopeSize(), solver);

            solver.post(Constraints.union(sets, setCards, union, unionCard));

            assertTrue(randomizeStrategy(solver).findSolution());
            checkCorrectness(sets, union);
            for (int solutions = 1; solutions < 10 && solver.nextSolution(); solutions++) {
                checkCorrectness(sets, union);
            }
        }
    }

    @Test(timeout = 60000)
    public void testSetUnion() {
        /*
         * import Control.Monad
         * import Data.List
         *
         * powerset = filterM (const [True, False])
         *
         * solutions = do
         *     s1 <- powerset [-4..2]
         *     s2 <- powerset [-2..4]
         *     s3 <- powerset [-2..2]
         *     guard $ sort (nub $ s1 ++ s2) == sort s3
         *     return (s1, s2, s3)
         */
        Solver solver = new Solver();

        SetVar s1 = VF.set("s1", -4, 2, solver);
        IntVar s1Card = VF.enumerated("|s1|", 0, s1.getEnvelopeSize(), solver);
        SetVar s2 = VF.set("s2", -2, 4, solver);
        IntVar s2Card = VF.enumerated("|s2|", 0, s2.getEnvelopeSize(), solver);
        SetVar s3 = VF.set("s3", -2, 2, solver);
        IntVar s3Card = VF.enumerated("|s3|", 0, s3.getEnvelopeSize(), solver);

        solver.post(SCF.cardinality(s1, s1Card));
        solver.post(SCF.cardinality(s2, s2Card));
        solver.post(Constraints.union(new SetVar[]{s1, s2}, new IntVar[]{s1Card, s2Card},
                s3, s3Card));

        int count = 0;
        if (randomizeStrategy(solver).findSolution()) {
            do {
                checkCorrectness(new SetVar[]{s1, s2}, s3);
                count++;
            } while (solver.nextSolution());
        }
        assertEquals(1024, count);
    }
}
