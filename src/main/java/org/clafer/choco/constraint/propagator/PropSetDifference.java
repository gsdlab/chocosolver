package org.clafer.choco.constraint.propagator;

import org.chocosolver.solver.constraints.Propagator;
import org.chocosolver.solver.constraints.PropagatorPriority;
import org.chocosolver.solver.exception.ContradictionException;
import org.chocosolver.solver.variables.SetVar;
import org.chocosolver.solver.variables.delta.ISetDeltaMonitor;
import org.chocosolver.solver.variables.events.SetEventType;
import org.chocosolver.util.ESat;
import org.chocosolver.util.objects.setDataStructures.ISetIterator;
import org.clafer.common.Check;

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
        this.minuendD = minuend.monitorDelta(this);
        this.subtrahendD = subtrahend.monitorDelta(this);
        this.differenceD = difference.monitorDelta(this);
    }

    @Override
    public int getPropagationConditions(int vIdx) {
        return SetEventType.all();
    }

    @Override
    public void propagate(int evtmask) throws ContradictionException {
        ISetIterator minuendEnv = minuend.getUB().iterator();
        while (minuendEnv.hasNext()) {
            int i = minuendEnv.nextInt();
            if (!subtrahend.getUB().contains(i) && !minuend.getUB().contains(i)) {
                minuend.remove(i, this);
            }
        }
        ISetIterator differenceKer = difference.getLB().iterator();
        while (differenceKer.hasNext()) {
            int i = differenceKer.nextInt();
            minuend.force(i, this);
            subtrahend.remove(i, this);
        }

        PropUtil.envSubsetEnv(difference, minuend, this);
        ISetIterator subtrahendIter = subtrahend.getLB().iterator();
        while (subtrahendIter.hasNext()) {
            int i = subtrahendIter.nextInt();
            difference.remove(i, this);
        }
        ISetIterator minuendKer = minuend.getLB().iterator();
        while (minuendKer.hasNext()) {
            int i = minuendKer.nextInt();
            if (!subtrahend.getUB().contains(i)) {
                difference.force(i, this);
            }
        }
    }

    @Override
    public void propagate(int idxVarInProp, int mask) throws ContradictionException {
        switch (idxVarInProp) {
            case 0:
                // minuend
                minuendD.freeze();
                minuendD.forEach(this::pruneDifferenceOnMinuendEnv, SetEventType.REMOVE_FROM_ENVELOPE);
                minuendD.forEach(this::pickDifferenceOnMinuendKer, SetEventType.ADD_TO_KER);
                minuendD.unfreeze();
                break;
            case 1:
                // subtrahend
                subtrahendD.freeze();
                subtrahendD.forEach(this::pickMinuendPickDiffrenceOnSubtrahendEnv, SetEventType.REMOVE_FROM_ENVELOPE);
                subtrahendD.forEach(this::pruneDifferenceOnSubtrahendKer, SetEventType.ADD_TO_KER);
                subtrahendD.unfreeze();
                break;
            case 2:
                // difference
                differenceD.freeze();
                differenceD.forEach(this::pruneMinuendOnDifferenceEnv, SetEventType.REMOVE_FROM_ENVELOPE);
                differenceD.forEach(this::pickMinuendPruneSubtrahendOnDifferenceKer, SetEventType.ADD_TO_KER);
                differenceD.unfreeze();
                break;
        }
    }

    private void pruneDifferenceOnMinuendEnv(int minuendEnv) throws ContradictionException {
        difference.remove(minuendEnv, this);
    }

    private void pickDifferenceOnMinuendKer(int minuendKer) throws ContradictionException {
        if (!subtrahend.getUB().contains(minuendKer)) {
            difference.force(minuendKer, this);
        }
    }

    private void pickMinuendPickDiffrenceOnSubtrahendEnv(int subtrahendEnv) throws ContradictionException {
        if (minuend.getLB().contains(subtrahendEnv)) {
            difference.force(subtrahendEnv, this);
        } else if (difference.getLB().contains(subtrahendEnv)) {
            minuend.force(subtrahendEnv, this);
        }
    }

    private void pruneDifferenceOnSubtrahendKer(int subtrahendKer) throws ContradictionException {
        difference.remove(subtrahendKer, this);
    }

    private void pruneMinuendOnDifferenceEnv(int differenceEnv) throws ContradictionException {
        if (!subtrahend.getUB().contains(differenceEnv)) {
            minuend.remove(differenceEnv, this);
        }
    }

    private void pickMinuendPruneSubtrahendOnDifferenceKer(int differenceKer) throws ContradictionException {
        minuend.force(differenceKer, this);
        subtrahend.remove(differenceKer, this);
    }

    @Override
    public ESat isEntailed() {
        ISetIterator minuendKer = minuend.getLB().iterator();
        while (minuendKer.hasNext()) {
            int i = minuendKer.nextInt();
            if (!subtrahend.getUB().contains(i) && !difference.getUB().contains(i)) {
                return ESat.FALSE;
            }
        }
        ISetIterator differenceKer = difference.getLB().iterator();
        while (differenceKer.hasNext()) {
            int i = differenceKer.nextInt();
            if (!minuend.getUB().contains(i) || subtrahend.getLB().contains(i)) {
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
