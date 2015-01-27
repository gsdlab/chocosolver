package org.clafer.choco.constraint;

import java.util.Arrays;
import java.util.HashSet;
import java.util.Set;
import org.chocosolver.solver.constraints.Constraint;
import org.chocosolver.solver.constraints.Propagator;
import org.chocosolver.solver.constraints.PropagatorPriority;
import org.chocosolver.solver.exception.ContradictionException;
import org.chocosolver.solver.variables.Variable;
import org.chocosolver.util.ESat;
import static org.chocosolver.util.ESat.TRUE;
import static org.chocosolver.util.ESat.UNDEFINED;

/**
 * Boolean or. The difference between this constraint and the one provided by
 * the library is that this one does not reify to boolean variables, thus it can
 * be added after the search has started. The one provided in the library
 * requires dynamic variable addition which is not supported.
 *
 * @author jimmy
 */
public class OrConstraint extends Constraint {

    public OrConstraint(Constraint... constraints) {
        super("or", new PropOr(buildArray(constraints), constraints));
    }

    private static Variable[] buildArray(Constraint... constraints) {
        Set<Variable> vars = new HashSet<>();
        for (Constraint constraint : constraints) {
            for (Propagator propagator : constraint.getPropagators()) {
                vars.addAll(Arrays.asList(propagator.getVars()));
                propagator.setReifiedSilent();
            }
        }
        return vars.toArray(new Variable[vars.size()]);
    }

    private static class PropOr extends Propagator<Variable> {

        private final Constraint[] constraints;

        protected PropOr(Variable[] vars, Constraint[] constraints) {
            super(vars, PropagatorPriority.LINEAR, false);
            this.constraints = constraints;
        }

        @Override
        public void propagate(int evtmask) throws ContradictionException {
            boolean allFalse = true;
            for (Constraint constraint : constraints) {
                switch (constraint.isSatisfied()) {
                    case TRUE:
                        setPassive();
                        return;
                    case UNDEFINED:
                        allFalse = false;
                        break;
                }
            }
            if (allFalse) {
                contradiction(vars[0], "All unsat.");
            }
        }

        @Override
        public ESat isEntailed() {
            boolean allFalse = true;
            for (Constraint constraint : constraints) {
                switch (constraint.isSatisfied()) {
                    case TRUE:
                        return ESat.TRUE;
                    case UNDEFINED:
                        allFalse = false;
                        break;
                }
            }
            return allFalse ? ESat.FALSE : ESat.UNDEFINED;
        }
    }
}
