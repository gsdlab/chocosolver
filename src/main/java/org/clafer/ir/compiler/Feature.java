package org.clafer.ir.compiler;

import java.util.Arrays;
import java.util.Random;
import solver.ResolutionPolicy;
import solver.Solver;
import solver.constraints.IntConstraintFactory;
import solver.search.loop.monitors.SearchMonitorFactory;
import solver.search.strategy.selectors.values.InDomainMin;
import solver.search.strategy.selectors.variables.FirstFail;
import solver.search.strategy.strategy.Assignment;
import solver.variables.BoolVar;
import solver.variables.IntVar;
import solver.variables.VariableFactory;
import util.tools.ArrayUtils;

/**
 * 1. optimize union if detect operands are disjoint.
 * 2. remove useless members and set unions
 * 
 * 
 * @author jimmy
 */
public class Feature {

    public static void main(String[] args) {
        Solver solver = new Solver();
        SearchMonitorFactory.log(solver, false, true);
        IntVar objective = VariableFactory.bounded("objective", -10000, 10000, solver);
        BoolVar[] features = VariableFactory.boolArray("feature", 100, solver);
        Random rand = new Random();
        IntVar[] footprint = new IntVar[features.length];
        for (int i = 0; i < footprint.length; i++) {
            int val = rand.nextBoolean() ? rand.nextInt(100) : -rand.nextInt(100);
            int[] e = new int[]{0, val};
            Arrays.sort(e);
            footprint[i] = VariableFactory.enumerated("footprint#" + i, e, solver);
            solver.post(IntConstraintFactory.implies(features[i],
                    IntConstraintFactory.arithm(footprint[i], "=", VariableFactory.fixed(val, solver))));
            solver.post(IntConstraintFactory.implies(VariableFactory.not(features[i]),
                    IntConstraintFactory.arithm(footprint[i], "=", VariableFactory.fixed(0, solver))));
        }
        solver.post(IntConstraintFactory.sum(footprint, objective));
        solver.set(new Assignment(new FirstFail(ArrayUtils.append(features, footprint)), new InDomainMin()));
        solver.findOptimalSolution(ResolutionPolicy.MINIMIZE, objective);
        System.out.println(solver);
    }
}
