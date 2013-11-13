package org.clafer.choco.constraint;

import org.clafer.collection.Pair;
import org.clafer.collection.Triple;
import static org.junit.Assert.*;
import org.junit.Test;
import solver.Solver;
import solver.constraints.Constraint;
import solver.variables.BoolVar;
import solver.variables.VF;

/**
 *
 * @author jimmy
 */
public class IfThenElseTest extends ConstraintTest<Triple<BoolVar, BoolVar, BoolVar>> {

    @Override
    protected void check(Triple<BoolVar, BoolVar, BoolVar> s) {
        if (s.getFst().instantiatedTo(1)) {
            assertTrue(s.getSnd().instantiatedTo(1));
        } else {
            assertTrue(s.getThd().instantiatedTo(1));
        }
    }

    @Override
    protected void checkNot(Triple<BoolVar, BoolVar, BoolVar> s) {
        if (s.getFst().instantiatedTo(1)) {
            assertFalse(s.getSnd().instantiatedTo(1));
        } else {
            assertFalse(s.getThd().instantiatedTo(1));
        }
    }

    @Test(timeout = 60000)
    public void quickTest() {
        randomizedTest(new TestCase<Triple<BoolVar, BoolVar, BoolVar>>() {
            @Override
            public Pair<Constraint, Triple<BoolVar, BoolVar, BoolVar>> setup(Solver solver) {
                BoolVar antecedent = toBoolVar(randBool(), solver);
                BoolVar consequent = toBoolVar(randBool(), solver);
                BoolVar alternative = toBoolVar(randBool(), solver);
                return pair(Constraints.ifThenElse(antecedent, consequent, alternative),
                        triple(antecedent, consequent, alternative));
            }
        });
    }

    @Test(timeout = 60000)
    public void testIfThenElse() {
        randomizedTest(new TestCase<Triple<BoolVar, BoolVar, BoolVar>>() {
            @PositiveSolutions(4)
            @NegativeSolutions(4)
            @Override
            public Pair<Constraint, Triple<BoolVar, BoolVar, BoolVar>> setup(Solver solver) {
                BoolVar antecedent = VF.bool("antecedent", solver);
                BoolVar consequent = VF.bool("consequent", solver);
                BoolVar alternative = VF.bool("alternative", solver);
                return pair(Constraints.ifThenElse(antecedent, consequent, alternative),
                        triple(antecedent, consequent, alternative));
            }
        });
    }
}
