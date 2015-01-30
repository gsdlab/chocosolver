package org.clafer.choco.constraint.propagator;

import org.clafer.common.Check;
import org.chocosolver.solver.constraints.Propagator;
import org.chocosolver.solver.constraints.PropagatorPriority;
import org.chocosolver.solver.exception.ContradictionException;
import org.chocosolver.solver.variables.SetVar;
import org.chocosolver.solver.variables.delta.ISetDeltaMonitor;
import org.chocosolver.solver.variables.events.SetEventType;
import org.chocosolver.util.ESat;
import org.chocosolver.util.procedure.IntProcedure;

/**
 *
 * @author jimmy
 */
public class PropSetDifference extends Propagator<SetVar> {

    private final SetVar minuend, subtrahend, difference;
    private final ISetDeltaMonitor minuendD, subtrahendD, differenceD;

    public PropSetDifference(SetVar minuend, SetVar subtrahend, SetVar difference) {
        super(new SetVar[]{minuend, subtrahend, difference}, PropagatorPriority.LINEAR, true);
        this.minuend = Check.notNull(minuend);
        this.subtrahend = Check.notNull(subtrahend);
        this.difference = Check.notNull(difference);
        this.minuendD = minuend.monitorDelta(aCause);
        this.subtrahendD = subtrahend.monitorDelta(aCause);
        this.differenceD = difference.monitorDelta(aCause);
    }

    @Override
    public int getPropagationConditions(int vIdx) {
        return SetEventType.all();
    }

    @Override
    public void propagate(int evtmask) throws ContradictionException {
        for (int i = minuend.getEnvelopeFirst(); i != SetVar.END; i = minuend.getEnvelopeNext()) {
            if (!subtrahend.envelopeContains(i) && !minuend.envelopeContains(i)) {
                minuend.removeFromEnvelope(i, aCause);
            }
        }
        for (int i = difference.getKernelFirst(); i != SetVar.END; i = difference.getKernelNext()) {
            minuend.addToKernel(i, aCause);
            subtrahend.removeFromEnvelope(i, aCause);
        }

        PropUtil.envSubsetEnv(difference, minuend, aCause);
        for (int i = subtrahend.getKernelFirst(); i != SetVar.END; i = subtrahend.getKernelNext()) {
            difference.removeFromEnvelope(i, aCause);
        }
        for (int i = minuend.getKernelFirst(); i != SetVar.END; i = minuend.getKernelNext()) {
            if (!subtrahend.envelopeContains(i)) {
                difference.addToKernel(i, aCause);
            }
        }
    }

    @Override
    public void propagate(int idxVarInProp, int mask) throws ContradictionException {
        switch (idxVarInProp) {
            case 0:
                // minuend
                minuendD.freeze();
                minuendD.forEach(pruneDifferenceOnMinuendEnv, SetEventType.REMOVE_FROM_ENVELOPE);
                minuendD.forEach(pickDifferenceOnMinuendKer, SetEventType.ADD_TO_KER);
                minuendD.unfreeze();
                break;
            case 1:
                // subtrahend
                subtrahendD.freeze();
                subtrahendD.forEach(pickMinuendPickDiffrenceOnSubtrahendEnv, SetEventType.REMOVE_FROM_ENVELOPE);
                subtrahendD.forEach(pruneDifferenceOnSubtrahendKer, SetEventType.ADD_TO_KER);
                subtrahendD.unfreeze();
                break;
            case 2:
                // difference
                differenceD.freeze();
                differenceD.forEach(pruneMinuendOnDifferenceEnv, SetEventType.REMOVE_FROM_ENVELOPE);
                differenceD.forEach(pickMinuendPruneSubtrahendOnDifferenceKer, SetEventType.ADD_TO_KER);
                differenceD.unfreeze();
                break;
        }
    }
    private final IntProcedure pruneDifferenceOnMinuendEnv = new IntProcedure() {
        @Override
        public void execute(int minuendEnv) throws ContradictionException {
            difference.removeFromEnvelope(minuendEnv, aCause);
        }
    };
    private final IntProcedure pickDifferenceOnMinuendKer = new IntProcedure() {
        @Override
        public void execute(int minuendKer) throws ContradictionException {
            if (!subtrahend.envelopeContains(minuendKer)) {
                difference.addToKernel(minuendKer, aCause);
            }
        }
    };
    private final IntProcedure pickMinuendPickDiffrenceOnSubtrahendEnv = new IntProcedure() {
        @Override
        public void execute(int subtrahendEnv) throws ContradictionException {
            if (minuend.kernelContains(subtrahendEnv)) {
                difference.addToKernel(subtrahendEnv, aCause);
            } else if (difference.kernelContains(subtrahendEnv)) {
                minuend.addToKernel(subtrahendEnv, aCause);
            }
        }
    };
    private final IntProcedure pruneDifferenceOnSubtrahendKer = new IntProcedure() {
        @Override
        public void execute(int subtrahendKer) throws ContradictionException {
            difference.removeFromEnvelope(subtrahendKer, aCause);
        }
    };
    private final IntProcedure pruneMinuendOnDifferenceEnv = new IntProcedure() {
        @Override
        public void execute(int differenceEnv) throws ContradictionException {
            if (!subtrahend.envelopeContains(differenceEnv)) {
                minuend.removeFromEnvelope(differenceEnv, aCause);
            }
        }
    };
    private final IntProcedure pickMinuendPruneSubtrahendOnDifferenceKer = new IntProcedure() {
        @Override
        public void execute(int differenceKer) throws ContradictionException {
            minuend.addToKernel(differenceKer, aCause);
            subtrahend.removeFromEnvelope(differenceKer, aCause);
        }
    };

    @Override
    public ESat isEntailed() {
        for (int i = minuend.getKernelFirst(); i != SetVar.END; i = minuend.getKernelNext()) {
            if (!subtrahend.envelopeContains(i) && !difference.envelopeContains(i)) {
                return ESat.FALSE;
            }
        }
        for (int i = difference.getKernelFirst(); i != SetVar.END; i = difference.getKernelNext()) {
            if (!minuend.envelopeContains(i) || subtrahend.kernelContains(i)) {
                return ESat.FALSE;
            }
        }
        return isCompletelyInstantiated() ? ESat.TRUE : ESat.UNDEFINED;
    }

    @Override
    public String toString() {
        return minuend + " - " + subtrahend + " = " + difference;
    }
}
