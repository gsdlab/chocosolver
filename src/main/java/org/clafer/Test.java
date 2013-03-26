package org.clafer;

import java.util.ArrayList;
import java.util.List;
import solver.Solver;
import solver.constraints.set.SetConstraintsFactory;
import solver.search.strategy.IntStrategyFactory;
import solver.search.strategy.SetStrategyFactory;
import solver.search.strategy.strategy.StrategiesSequencer;
import solver.search.strategy.strategy.set.SetSearchStrategy;
import solver.variables.IntVar;
import solver.variables.SetVar;
import solver.variables.Variable;
import solver.variables.VariableFactory;

/**
 *
 * @author jimmy
 */
public class Test {

public static void main(String[] args) {
    Solver solver = new Solver("my first problem");
    IntVar[] iv = VariableFactory.enumeratedArray("iv", 5, 0, 3, solver);
    SetVar[] sv = new SetVar[4];
    for (int i = 0; i < sv.length; i++) {
        sv[i] = VariableFactory.set("sv" + i, new int[]{0, 1, 2, 3, 4}, solver);
    }

    solver.post(SetConstraintsFactory.int_channel(sv, iv, 0, 0));

    solver.set(new StrategiesSequencer(solver.getEnvironment(),
            IntStrategyFactory.firstFail_InDomainMin(iv),
            SetStrategyFactory.setLex(sv)));

    if (solver.findSolution()) {
        do {
            System.out.println(solver);
        } while (solver.nextSolution());
    }
}
//    public static void main(String[] args) {
//        Solver solver = new Solver("my first problem");
//
//        SetVar take = VariableFactory.set("take", new int[]{0, 1, 2}, solver);
//        SetVar[] c = new SetVar[]{
//            VariableFactory.set("c0", new int[]{0, 1, 2, 3, 4}, solver),
//            VariableFactory.set("c1", new int[]{0, 1, 2, 3, 4}, solver),
//            VariableFactory.set("c2", new int[]{0, 1, 2, 3, 4}, solver),};
//        SetVar to = VariableFactory.set("to", new int[]{0, 1, 2, 3, 4}, solver);
//
//        Constraint con = new Constraint(new SetVar[]{take, c[0], c[1], c[2], to}, solver);
//        con.setPropagators(new PropAllDisjoint(c), new PropJoin(take, c, to));
//        solver.post(con);
//
//        solver.set(new SetSearchStrategy(svs(solver)));
//        if (solver.findSolution()) {
//            do {
//                System.out.println(solver.toString());
//                System.out.println(solver.getMeasures().getSolutionCount());
//            } while (solver.nextSolution());
//        }
//        System.out.println(solver.getMeasures());
//    }

    

    private static SetVar[] svs(Solver solver) {
        Variable[] vars = solver.getVars();
        List<SetVar> svs = new ArrayList<SetVar>(vars.length);
        for (Variable var : vars) {
            if (var instanceof SetVar) {
                svs.add((SetVar) var);
            }
        }
        return svs.toArray(new SetVar[svs.size()]);
    }
}
