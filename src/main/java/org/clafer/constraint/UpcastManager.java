package org.clafer.constraint;

import choco.Choco;
import choco.cp.model.CPModel;
import choco.cp.model.managers.MixedConstraintManager;
import choco.cp.solver.CPSolver;
import choco.cp.solver.search.integer.branching.AssignVar;
import choco.cp.solver.search.integer.valiterator.IncreasingDomain;
import choco.cp.solver.search.integer.varselector.MinDomain;
import choco.cp.solver.search.set.AssignSetVar;
import choco.cp.solver.search.set.MinDomSet;
import choco.cp.solver.search.set.MinEnv;
import choco.kernel.common.logging.ChocoLogging;
import choco.kernel.model.Model;
import choco.kernel.model.ModelException;
import choco.kernel.model.constraints.ComponentConstraint;
import choco.kernel.model.constraints.Constraint;
import choco.kernel.model.variables.Variable;
import choco.kernel.model.variables.integer.IntegerVariable;
import choco.kernel.model.variables.set.SetVariable;
import choco.kernel.solver.Solver;
import choco.kernel.solver.constraints.SConstraint;
import java.util.List;
import org.clafer.Util;

/**
 *
 * @author jimmy
 */
public class UpcastManager extends MixedConstraintManager {

    @Override
    public SConstraint makeConstraint(Solver solver, Variable[] vars, Object params, List<String> options) {
        if (solver instanceof CPSolver) {
            if (params instanceof Integer) {
                return new Upcast(
                        solver.getVar((SetVariable) vars[0]),
                        solver.getVar((SetVariable) vars[1]),
                        (Integer) params);
            }
        }
        throw new ModelException("Could not found a constraint manager in " + this.getClass() + " !");
    }

    public static Constraint upcast(SetVariable from, SetVariable to, int offset) {
        return new ComponentConstraint(UpcastManager.class, offset, new Variable[]{from, to});
    }

    public static void main(String[] args) {
        Model m = new CPModel();

        SetVariable from = Choco.makeSetVar("from", 0, 10);
        SetVariable to = Choco.makeSetVar("to", 0, 10);

        m.addConstraint(upcast(from, to, 3));

        System.out.println(Util.allSolutions(m).getStatistics());

        Solver solver = Util.newSolver(m);
        solver.addGoal(new AssignVar(new MinDomain(solver), new IncreasingDomain()));
        solver.addGoal(new AssignSetVar(new MinDomSet(solver), new MinEnv()));
        System.out.println(Util.allSolutions(solver).getStatistics());

        // 2^[0..7|
        // #256 solutions 8 Time (ms), 511 Nodes, 510 Backtracks, 0 Restarts - 
        // #256 solutions 28 Time (ms), 504 Nodes, 503 Backtracks, 0 Restarts - 
    }
}
