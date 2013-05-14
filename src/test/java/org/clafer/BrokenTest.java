package org.clafer;

import java.util.Arrays;
import org.junit.Test;
import solver.Solver;
import solver.constraints.set.SetConstraintsFactory;
import solver.search.strategy.strategy.set.SetSearchStrategy;
import solver.variables.IntVar;
import solver.variables.SetVar;
import solver.variables.VariableFactory;

/**
 *
 * @author jimmy
 */
public class BrokenTest {

    public static void main(String[] args) {
        Solver solver = new Solver();
        SetVar[] svs = new SetVar[4];
        for (int i = 0; i < svs.length; i++) {
            svs[i] = VariableFactory.set("sv" + i, new int[]{0, 1, 2, 3, 4, 5, 6, 7, 8, 9}, solver);
        }
        IntVar[] ivs = VariableFactory.enumeratedArray("iv", 10, 0, 3, solver);

        solver.post(SetConstraintsFactory.cardinality(svs[0], VariableFactory.fixed(3, solver)));
        solver.post(SetConstraintsFactory.cardinality(svs[1], VariableFactory.fixed(3, solver)));
        solver.set(new SetSearchStrategy(Arrays.copyOf(svs, 2)));

        solver.post(SetConstraintsFactory.int_channel(svs, ivs, 0, 0));
        if (solver.findSolution()) {
            System.out.println(solver);
        }
    }

    @Test
    public void testIntChannel() {
        // Set Configuration.IDEMPOTENCY = error;
        Solver solver = new Solver();
        SetVar[] svs = new SetVar[4];
        for (int i = 0; i < svs.length; i++) {
            svs[i] = VariableFactory.set("sv" + i, new int[]{0, 1, 2, 3, 4, 5, 6, 7, 8, 9}, solver);
        }
        IntVar[] ivs = VariableFactory.enumeratedArray("iv", 10, 0, 3, solver);

        solver.set(new SetSearchStrategy(Arrays.copyOf(svs, 4)));

        solver.post(SetConstraintsFactory.int_channel(svs, ivs, 0, 0));
        if (solver.findSolution()) {
            System.out.println(solver);
        }
    }
}
