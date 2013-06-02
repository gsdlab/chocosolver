package org.clafer.choco.constraint;

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
public class PropFilterStringTest extends ConstraintTest {

    @Test(timeout = 60000)
    public void testFilterString() {
        /*
         * import Control.Monad
         * 
         * powerset = filterM (const [True, False])
         * 
         * solutions = do
         *     set <- powerset [0..2]
         *     string <- sequence $ replicate 3 [0..2]
         *     let result = [string !! i | i <- set]
         * 
         *     return (set, string, result)
         */
        Solver solver = new Solver();

        SetVar set = VF.set("set", new int[]{0, 1, 2}, solver);
        IntVar[] string = VF.enumeratedArray("string", 3, 0, 2, solver);
        IntVar[] result = VF.enumeratedArray("result", 3, -1, 2, solver);

        solver.post(Constraints.filterString(set,0, string, result));

        assertEquals(216, randomizeStrategy(solver).findAllSolutions());
    }
}
