package org.clafer.constraint.propagator;

import solver.constraints.propagators.Propagator;
import solver.constraints.propagators.PropagatorPriority;
import solver.exception.ContradictionException;
import solver.variables.BoolVar;
import solver.variables.EventType;
import solver.variables.IntVar;
import util.ESat;

/**
 * The first n booleans are the true, the rest are false.
 * 
 * @author jimmy
 */
public class PropSelectN extends Propagator<IntVar> {

    private final BoolVar[] bools;
    private final IntVar n;

    public PropSelectN(BoolVar[] bools, IntVar n) {
        super(init(bools, n), PropagatorPriority.LINEAR);
        this.bools = bools;
        this.n = n;
    }

    private static IntVar[] init(BoolVar[] bools, IntVar n) {
        IntVar[] init = new IntVar[bools.length + 1];
        System.arraycopy(bools, 0, init, 0, bools.length);
        init[bools.length] = n;
        return init;
    }

    private boolean isBoolsVar(int varIdx) {
        return varIdx < bools.length;
    }

    private boolean isNVar(int varIdx) {
        return varIdx == bools.length;
    }

    @Override
    public int getPropagationConditions(int vIdx) {
        if (isBoolsVar(vIdx)) {
            return EventType.DECUPP.mask + EventType.INCLOW.mask;
        }
        return EventType.INSTANTIATE.mask;
    }

    @Override
    public void propagate(int evtmask) throws ContradictionException {
        throw new UnsupportedOperationException("Not supported yet.");
    }

    @Override
    public void propagate(int idxVarInProp, int mask) throws ContradictionException {
        propagate(mask);
    }

    @Override
    public ESat isEntailed() {
        boolean allInstantiated = true;
        for(int i = 0; i < bools.length;i++) {
            if(bools[i].instantiated()) {
                if(bools[i].getValue() == 0 && i > n.getUB()) {
                    return ESat.FALSE;
                }
                if(bools[i].getValue() == 1 && i < n.getLB()) {
                    return ESat.FALSE;
                }
            } else {
                allInstantiated = false;
            }
        }
        return allInstantiated ? ESat.TRUE : ESat.UNDEFINED;
    }

//    @Override
//    public void awakeOnInf(int varIdx) throws ContradictionException {
//        if (isNVar(varIdx)) {
//            // Pick bools
//            for (int i = 0; i < n.getInf(); i++) {
//                bools[i].instantiate(1, this, false);
//            }
//        }
//    }
//
//    @Override
//    public void awakeOnSup(int varIdx) throws ContradictionException {
//        if (isNVar(varIdx)) {
//            // Pick bools
//            for (int i = n.getSup(); i < bools.length; i++) {
//                bools[i].instantiate(0, this, false);
//            }
//        }
//    }
//
//    @Override
//    public void awakeOnInst(int idx) throws ContradictionException {
//        if (isBoolsVar(idx)) {
//            // Prune N
//            int id = getBoosVarIndex(idx);
//            if (bools[id].getVal() == 0) {
//                n.updateSup(id, this, false);
//            } else {
//                n.updateInf(id + 1, this, false);
//            }
//        }
//    }
//
//    @Override
//    public void awake() throws ContradictionException {
//        n.updateInf(0, this, false);
//        n.updateSup(bools.length, this, false);
//        propagate();
//    }
//
//    @Override
//    public void propagate() throws ContradictionException {
//        pickBools();
//        pruneN();
//    }
//
//    private void pickBools() throws ContradictionException {
//        for (int i = 0; i < n.getInf(); i++) {
//            bools[i].instantiate(1, this, false);
//        }
//        for (int i = n.getSup(); i < bools.length; i++) {
//            bools[i].instantiate(0, this, false);
//        }
//    }
//
//    private void pruneN() throws ContradictionException {
//        for (int i = 0; i < bools.length; i++) {
//            if (bools[i].isInstantiated()) {
//                if (bools[i].getVal() == 0) {
//                    n.updateSup(i, this, false);
//                } else {
//                    n.updateInf(i + 1, this, false);
//                }
//            }
//        }
//    }
//
}
