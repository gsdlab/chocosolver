package org.clafer.choco.constraint;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import solver.constraints.Constraint;
import solver.constraints.Propagator;
import solver.constraints.PropagatorPriority;
import solver.exception.ContradictionException;
import solver.variables.EventType;
import solver.variables.Variable;
import util.ESat;
import static util.ESat.TRUE;
import static util.ESat.UNDEFINED;

/**
 * Boolean or. The difference between this constraint and the one provided by
 * the library is that this one does not reify to boolean variables, thus it can
 * be added after the search has started. The one provided in the library
 * requires dynamic variable addition which is not supported.
 *
 * @author jimmy
 */
public class OrConstraint extends Constraint<Variable, Propagator<Variable>> {

    public OrConstraint(Constraint... constraints) {
        super(buildArray(constraints), constraints[0].getSolver());
        setPropagators(new PropOr(vars, constraints));
    }

    private static Variable[] buildArray(Constraint... constraints) {
        List<Variable> vars = new ArrayList<Variable>();
        for (Constraint constraint : constraints) {
            vars.addAll(Arrays.asList(constraint.getVariables()));
            for(Propagator propagator : constraint.getPropagators()) {
                propagator.setReifiedSilent();
            }
        }
        return vars.toArray(new Variable[vars.size()]);
    }

    private class PropOr extends Propagator<Variable> {

        private final Constraint[] constraints;

        protected PropOr(Variable[] vars, Constraint[] constraints) {
            super(vars, PropagatorPriority.LINEAR, true);
            this.constraints = constraints;
        }

        @Override
        public int getPropagationConditions(int vIdx) {
            return EventType.ALL_FINE_EVENTS.mask;
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
        public void propagate(int idxVarInProp, int mask) throws ContradictionException {
            forcePropagate(EventType.CUSTOM_PROPAGATION);
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
