//package org.clafer.constraint;
//
//import choco.cp.solver.variables.integer.IntVarEvent;
//import choco.kernel.solver.ContradictionException;
//import choco.kernel.solver.constraints.integer.AbstractLargeIntSConstraint;
//import choco.kernel.solver.variables.integer.IntDomainVar;
//import org.clafer.Util;
//
///**
// * Assumption: parents small domain and all positive
// * 
// * @author jimmy
// */
//public class UniqRef extends AbstractLargeIntSConstraint {
//
//    private final IntDomainVar[] parents, refs;
//    private final int unused;
//    private final int parentsDomainSize;
//    private final int refsDomainSize;
//
//    public UniqRef(IntDomainVar[] parents, IntDomainVar[] refs, int unused) {
//        super(Util.combine(parents, refs));
//        for (IntDomainVar parent : parents) {
//            if (!parent.canBeInstantiatedTo(unused)) {
//                throw new IllegalArgumentException();
//            }
//        }
//        this.parents = parents;
//        this.refs = refs;
//        this.unused = unused;
//        // One is reserved for unused.
//        this.parentsDomainSize = Util.maximum(Util.domainSizes(parents)) - 1;
//        this.refsDomainSize = Util.maximum(Util.domainSizes(refs));
//    }
//
//    @Override
//    public int getFilteredEventMask(int idx) {
//        return IntVarEvent.INSTINT_MASK;
//    }
//
//    public boolean isParentVar(int varIdx) {
//        return varIdx < parents.length;
//    }
//
//    public boolean isRefVar(int varIdx) {
//        return varIdx >= parents.length;
//    }
//
//    public int getParentVarIndex(int varIdx) {
//        return varIdx;
//    }
//
//    public int getRefVarIndex(int varIdx) {
//        return varIdx - parents.length;
//    }
//
//    private void neq(IntDomainVar[] vars, int i, int j) throws ContradictionException {
//        if (vars[i].isInstantiated()) {
//            vars[j].removeVal(vars[i].getVal(), this, false);
//        }
//        if (vars[j].isInstantiated()) {
//            vars[i].removeVal(vars[j].getVal(), this, false);
//        }
//    }
//
//    private void neqParent(IntDomainVar[] vars, int i, int j) throws ContradictionException {
//        if (vars[i].isInstantiated() && vars[i].getVal() != unused) {
//            vars[j].removeVal(vars[i].getVal(), this, false);
//        }
//        if (vars[j].isInstantiated() && vars[j].getVal() != unused) {
//            vars[i].removeVal(vars[j].getVal(), this, false);
//        }
//    }
//
////    @Override
////    public void awakeOnInst(int idx) throws ContradictionException {
////        ss = gg();
////        if (isParentVar(idx)) {
////            int i = getParentVarIndex(idx);
////
////            pruneRef(i);
////            pruneParents();
////        } else {
////            int i = getRefVarIndex(idx);
////
////            pruneRefs();
////            pruneParent(i);
////        }
////    }
//    @Override
//    public void propagate() throws ContradictionException {
//        pruneRefs();
//        pruneParents();
//    }
//
//    /**
//     * 
//     * @param i - A parent that is instantiated.
//     * @throws ContradictionException 
//     */
//    private void pruneRef(int i) throws ContradictionException {
//        int val = parents[i].getVal();
//        if (val != unused) {
//            int count = 1;
//            for (int j = 0; j < parents.length; j++) {
//                if (i != j
//                        && parents[j].isInstantiated()
//                        && parents[j].getVal() == val) {
//                    neq(refs, i, j);
//                    count++;
//                }
//            }
//            // Simple heuristic.
//            // See testManyParentSmallDomainRefs in the unit test.
//            // Prevents some cases of very bad performance.
//            if (count == refsDomainSize) {
//                for (IntDomainVar parent : parents) {
//                    if (!parent.isInstantiated()) {
//                        parent.removeVal(val, this, false);
//                    }
//                }
//            }
//            // This can happen because of pruning on parents by this or
//            // other constraints.
//            if (count > refsDomainSize) {
//                fail();
//            }
//        } else {
//            refs[i].instantiate(0, this, false);
//        }
//    }
//
//    private void pruneRefs() throws ContradictionException {
//        for (int i = 0; i < parents.length; i++) {
//            if (parents[i].isInstantiated()) {
//                pruneRef(i);
//            }
//        }
//    }
//
//    /**
//     * 
//     * @param i - A ref that is isntantiated
//     * @throws ContradictionException 
//     */
//    private void pruneParent(int i) throws ContradictionException {
//        int val = refs[i].getVal();
//        int count = 1;
//        for (int j = 0; j < refs.length; j++) {
//            if (i != j
//                    && refs[j].isInstantiated()
//                    && refs[j].getVal() == val) {
//                neqParent(parents, i, j);
//                count++;
//            }
//        }
//        if (val != 0) {
//            parents[i].removeVal(unused, this, false);
//
//            if (count == parentsDomainSize) {
//                for (IntDomainVar ref : refs) {
//                    if (!ref.isInstantiated()) {
//                        ref.removeVal(val, this, false);
//                    }
//                }
//            }
//            if (count > parentsDomainSize) {
//                fail();
//            }
//        }
//    }
//
//    private void pruneParents() throws ContradictionException {
//        for (int i = 0; i < refs.length; i++) {
//            if (refs[i].isInstantiated()) {
//                pruneParent(i);
//            }
//        }
//    }
//
//    @Override
//    public boolean isSatisfied() {
//        return true;
//    }
//
//    @Override
//    public boolean isSatisfied(int[] tuple) {
//        return true;
//    }
//}
