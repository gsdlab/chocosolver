package org.clafer.constraint;

import choco.Choco;
import choco.cp.model.CPModel;
import choco.cp.model.managers.IntConstraintManager;
import choco.cp.solver.CPSolver;
import choco.cp.solver.search.integer.branching.AssignVar;
import choco.cp.solver.search.integer.valiterator.IncreasingDomain;
import choco.cp.solver.search.integer.varselector.MinDomain;
import choco.cp.solver.search.set.AssignSetVar;
import choco.cp.solver.search.set.MinDomSet;
import choco.cp.solver.search.set.MinEnv;
import choco.kernel.model.Model;
import choco.kernel.model.ModelException;
import choco.kernel.model.constraints.ComponentConstraint;
import choco.kernel.model.constraints.Constraint;
import choco.kernel.model.variables.integer.IntegerVariable;
import choco.kernel.solver.Solver;
import choco.kernel.solver.constraints.SConstraint;
import java.util.List;
import org.clafer.Util;

/**
 *
 * @author jimmy
 */
public class BoolSlideManager extends IntConstraintManager {

    @Override
    public SConstraint makeConstraint(Solver solver, IntegerVariable[] variables, Object parameters, List<String> options) {
        if (solver instanceof CPSolver) {
            if (parameters instanceof Integer) {
                int baseLength = (Integer) parameters;
                IntegerVariable offset = variables[0];
                IntegerVariable[] base = new IntegerVariable[baseLength];
                IntegerVariable[] slide = new IntegerVariable[variables.length - baseLength - 1];
                System.arraycopy(variables, 1, base, 0, baseLength);
                System.arraycopy(variables, baseLength + 1, slide, 0, slide.length);
                return new BoolSlide(solver.getVar(base), solver.getVar(slide), solver.getVar(offset));
            }
        }
        throw new ModelException("Could not found a constraint manager in " + this.getClass() + " !");
    }

    public static Constraint boolSlide(IntegerVariable[] base, IntegerVariable[] slide, IntegerVariable offset) {
        if (base.length > slide.length) {
            throw new IllegalArgumentException();
        }

        return new ComponentConstraint(
                BoolSlideManager.class,
                base.length,
                Util.cons(offset, Util.combine(base, slide)));
    }

    public static void main(String[] args) {
        Model m = new CPModel();
        IntegerVariable[] base = Choco.makeBooleanVarArray("base", 4);
        IntegerVariable[] slide = Choco.makeBooleanVarArray("base", 6);
        IntegerVariable offset = Choco.makeIntVar("base", 0, 3);

        m.addConstraint(boolSlide(base, slide, offset));

        System.out.println(Util.allSolutions(m).getStatistics());

        Solver solver = Util.newSolver(m);
        solver.addGoal(new AssignVar(new MinDomain(solver), new IncreasingDomain()));
        solver.addGoal(new AssignSetVar(new MinDomSet(solver), new MinEnv()));
        System.out.println(Util.allSolutions(solver).getStatistics());
        
        // #192 solutions 5 Time (ms), 387 Nodes, 392 Backtracks, 0 Restarts - 
        // #192 solutions 1 Time (ms), 387 Nodes, 389 Backtracks, 0 Restarts - 
    }
}