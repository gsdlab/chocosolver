package org.clafer.constraint;

import choco.cp.model.managers.SetConstraintManager;
import choco.cp.solver.CPSolver;
import choco.kernel.model.ModelException;
import choco.kernel.model.constraints.ComponentConstraint;
import choco.kernel.model.constraints.Constraint;
import choco.kernel.model.variables.set.SetVariable;
import choco.kernel.solver.Solver;
import choco.kernel.solver.constraints.SConstraint;
import java.util.List;
import org.clafer.ast.Card;

/**
 *
 * @author jimmy
 */
public class SetLexManager extends SetConstraintManager {

    @Override
    public SConstraint makeConstraint(Solver solver, SetVariable[] vars, Object params, List<String> options) {
        if (solver instanceof CPSolver) {
            if (params instanceof Card) {
                return new SetLex(solver.getVar(vars), (Card) params);
            }
        }
        throw new ModelException("Could not found a constraint manager in " + this.getClass() + " !");
    }

    public static Constraint setLex(SetVariable[] sets, Card card) {
        return new ComponentConstraint(SetLexManager.class, card, sets);
    }
}