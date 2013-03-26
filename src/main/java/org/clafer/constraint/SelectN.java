//package org.clafer.constraint;
//
//import choco.cp.solver.variables.integer.IntVarEvent;
//import choco.kernel.solver.ContradictionException;
//import choco.kernel.solver.constraints.integer.AbstractLargeIntSConstraint;
//import choco.kernel.solver.variables.integer.IntDomainVar;
//import org.clafer.Util;
//
///**
// * The first n booleans are the true, the rest are false.
// * 
// * @author jimmy
// */
//public class SelectN extends AbstractLargeIntSConstraint {
//
//    private final IntDomainVar n;
//    private final IntDomainVar[] bools;
//
//    public SelectN(IntDomainVar[] bools, IntDomainVar n) {
//        super(Util.cons(n, bools));
//        this.n = n;
//        this.bools = bools;
//    }
//
//    private boolean isNVar(int varIdx) {
//        return varIdx == 0;
//    }
//
//    private boolean isBoolsVar(int varIdx) {
//        return varIdx > 0;
//    }
//
//    private int getBoosVarIndex(int varIdx) {
//        return varIdx - 1;
//    }
//
//    @Override
//    public int getFilteredEventMask(int idx) {
//        if (isNVar(idx)) {
//            return IntVarEvent.DECSUP_MASK + IntVarEvent.INCINF_MASK;
//        }
//        return IntVarEvent.INSTINT_MASK;
//    }
//
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
//    @Override
//    public boolean isSatisfied(int[] tuple) {
//        int selectN = tuple[0];
//        if (selectN >= tuple.length) {
//            return false;
//        }
//        for (int i = 1; i < tuple.length; i++) {
//            if (tuple[i] == (i <= selectN ? 0 : 1)) {
//                return false;
//            }
//        }
//        return true;
//    }
//}
