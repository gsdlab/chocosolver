package org.clafer.constraint.propagator;

import org.clafer.constraint.Constraints;
import solver.Solver;
import solver.constraints.propagators.Propagator;
import solver.constraints.propagators.PropagatorPriority;
import solver.exception.ContradictionException;
import solver.search.strategy.IntStrategyFactory;
import solver.variables.EventType;
import solver.variables.IntVar;
import solver.variables.SetVar;
import solver.variables.Variable;
import solver.variables.VariableFactory;
import util.ESat;

/**
 *
 * @author jimmy
 */
public class PropSingleton extends Propagator<Variable> {

    private final IntVar ivar;
    private final SetVar svar;

    public PropSingleton(IntVar ivar, SetVar svar) {
        super(new Variable[]{ivar, svar}, PropagatorPriority.UNARY);
        this.ivar = ivar;
        this.svar = svar;
    }

    private final boolean isIVar(int idxVarInProp) {
        return idxVarInProp == 0;
    }

    private final boolean isSVar(int idxVarInProp) {
        return idxVarInProp == 1;
    }

    @Override
    public int getPropagationConditions(int vIdx) {
        if (isIVar(vIdx)) {
            return EventType.REMOVE.mask + EventType.INSTANTIATE.mask;
        }
        assert isSVar(vIdx);
        return EventType.ADD_TO_KER.mask + EventType.REMOVE_FROM_ENVELOPE.mask;
    }

    @Override
    public void propagate(int evtmask) throws ContradictionException {
        if (svar.getKernelSize() > 1) {
            contradiction(svar, "Singleton cannot have more than 1 element");
        }
        if (svar.getEnvelopeSize() < 1) {
            contradiction(svar, "Singleton cannot have less than 1 element");
        }
        PropUtil.subsetEnv(ivar, svar, aCause);
        PropUtil.subsetEnv(svar, ivar, aCause);
        if (ivar.instantiated()) {
            svar.instantiateTo(new int[]{ivar.getValue()}, aCause);
        } else if (svar.getEnvelopeSize() == 1) {
            int val = svar.getEnvelopeFirst();
            ivar.instantiateTo(val, aCause);
            svar.instantiateTo(new int[]{val}, aCause);
        }
    }

    @Override
    public void propagate(int idxVarInProp, int mask) throws ContradictionException {
        // TODO
        propagate(mask);
    }

    @Override
    public ESat isEntailed() {
        if (svar.getKernelSize() > 1) {
            return ESat.FALSE;
        }
        if (svar.getEnvelopeSize() < 1) {
            return ESat.FALSE;
        }
        int ub = ivar.getUB();
        for (int i = ivar.getLB(); i <= ub; i = ivar.nextValue(i)) {
            for (int j = svar.getEnvelopeFirst(); j != SetVar.END; j = svar.getEnvelopeNext()) {
                if (i == j) {
                    return ivar.instantiated() && svar.instantiated() ? ESat.TRUE : ESat.UNDEFINED;
                }
            }
        }
        return ESat.FALSE;
    }

    @Override
    public String toString() {
        return "{" + ivar + "} = " + svar;
    }

    public static void main(String[] args) {
        Solver s = new Solver();

        IntVar ivar = VariableFactory.enumerated("ivar", 0, 10, s);
        SetVar svar = VariableFactory.set("svar", new int[]{1, 2, 3, 4, 5, 6, 7, 8, 9}, s);
        s.post(Constraints.singleton(ivar, svar));

        s.set(IntStrategyFactory.firstFail_InDomainMin(new IntVar[]{ivar}));

        if (s.findSolution()) {
            do {
                System.out.println(s);
            } while (s.nextSolution());
        }
        System.out.println(s.getMeasures());
    }
}
