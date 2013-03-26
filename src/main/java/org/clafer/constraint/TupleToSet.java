//package org.clafer.constraint;
//
//import choco.cp.solver.variables.integer.IntVarEvent;
//import choco.cp.solver.variables.set.SetVarEvent;
//import choco.kernel.common.util.iterators.DisposableIntIterator;
//import choco.kernel.solver.ContradictionException;
//import choco.kernel.solver.constraints.set.AbstractMixedSetIntSConstraint;
//import choco.kernel.solver.variables.Var;
//import choco.kernel.solver.variables.integer.IntDomainVar;
//import choco.kernel.solver.variables.set.SetVar;
//import gnu.trove.TIntHashSet;
//import org.clafer.Check;
//import org.clafer.Util;
//
///**
// * Elements in the tuple are unique.
// * 
// * @author jimmy
// */
//public class TupleToSet extends AbstractMixedSetIntSConstraint {
//
//    private final IntDomainVar[] tuple;
//    private final SetVar set;
//
//    public TupleToSet(IntDomainVar[] tuple, SetVar set) {
//        super(init(Check.noNulls(tuple), Check.notNull(set)));
//        this.tuple = tuple;
//        this.set = set;
//    }
//
//    private static Var[] init(IntDomainVar[] tuple, SetVar set) {
//        Var[] vars = new Var[tuple.length + 1];
//        vars[0] = set;
//        System.arraycopy(tuple, 0, vars, 1, tuple.length);
//        return vars;
//    }
//
//    @Override
//    public int getFilteredEventMask(int idx) {
//        if (isSetVar(idx)) {
//            return SetVarEvent.ADDKER_MASK + SetVarEvent.REMENV_MASK + SetVarEvent.INSTSET_MASK;
//        }
//        return IntVarEvent.REMVAL_MASK + IntVarEvent.INSTINT_MASK;
//    }
//
//    boolean isSetVar(int varIdx) {
//        return varIdx == 0;
//    }
//
//    boolean isTupleVar(int varIdx) {
//        return varIdx > 0;
//    }
//
//    int getTupleVarIndex(int varIdx) {
//        return varIdx - 1;
//    }
//
//    @Override
//    public void awake() throws ContradictionException {
//        set.getCard().instantiate(tuple.length, this, false);
//        propagate();
//    }
//
//    @Override
//    public void awakeOnEnv(int varIdx, int x) throws ContradictionException {
//        for (IntDomainVar var : tuple) {
//            var.removeVal(x, this, false);
//        }
//    }
//
//    @Override
//    public void awakeOnKer(int varIdx, int x) throws ContradictionException {
//        pickTuple(x);
//    }
//
//    @Override
//    public void awakeOnRem(int varIdx, int val) throws ContradictionException {
//        if(isSetVar(varIdx)) {
//            awakeOnEnv(varIdx, val);
//        } else {
//            pruneSet(val);
//        }
//    }
//    
//    @Override
//    public void awakeOnInst(int varIdx) throws ContradictionException {
//        if (isTupleVar(varIdx)) {
//            int i = getTupleVarIndex(varIdx);
//            // pick set
//            set.addToKernel(tuple[i].getVal(), this, false);
//            uniqueTuple(i);
//        }
//    }
//
//    @Override
//    public void propagate() throws ContradictionException {
//        pickTuple();
//        pickSet();
//        pruneTuple();
//        uniqueTuple();
//        pruneSet();
//    }
//
//    private void pruneTuple() throws ContradictionException {
//        for (int i = 0; i < tuple.length; i++) {
//            Util.subsetOf(this, tuple[i], set.getDomain().getEnveloppeDomain());
//        }
//    }
//
//    private void pruneSet(int val) throws ContradictionException {
//        if (!isSupportedByTuple(val)) {
//            set.remFromEnveloppe(val, this, false);
//        }
//    }
//
//    private void pruneSet() throws ContradictionException {
//        DisposableIntIterator it = set.getDomain().getEnveloppeIterator();
//        try {
//            while (it.hasNext()) {
//                int val = it.next();
//                pruneSet(val);
//            }
//        } finally {
//            it.dispose();
//        }
//    }
//
//    private void uniqueTuple(int i) throws ContradictionException {
//        assert tuple[i].isInstantiated();
//        for (int j = 0; j < tuple.length; j++) {
//            if (i != j) {
//                tuple[j].removeVal(tuple[i].getVal(), this, false);
//            }
//        }
//    }
//
//    private void uniqueTuple() throws ContradictionException {
//        for (int i = 0; i < tuple.length; i++) {
//            if (tuple[i].isInstantiated()) {
//                uniqueTuple(i);
//            }
//        }
//    }
//
//    private void pickTuple(int val) throws ContradictionException {
//        IntDomainVar support = isSupportedByOneTupleElement(val);
//        if (support != null) {
//            support.instantiate(val, this, false);
//        }
//    }
//
//    private void pickTuple() throws ContradictionException {
//        DisposableIntIterator it = set.getDomain().getKernelIterator();
//        try {
//            while (it.hasNext()) {
//                int val = it.next();
//
//                pickTuple(val);
//            }
//        } finally {
//            it.dispose();
//        }
//    }
//
//    private void pickSet() throws ContradictionException {
//        for (IntDomainVar var : tuple) {
//            if (var.isInstantiated()) {
//                set.addToKernel(var.getVal(), this, false);
//            }
//        }
//    }
//
//    private IntDomainVar isSupportedByOneTupleElement(int i) {
//        IntDomainVar found = null;
//        for (IntDomainVar var : tuple) {
//            if (var.canBeInstantiatedTo(i)) {
//                if (found == null) {
//                    found = var;
//                } else {
//                    return null;
//                }
//            }
//        }
//        return found;
//    }
//
//    private boolean isSupportedByTuple(int i) {
//        for (IntDomainVar var : tuple) {
//            if (var.canBeInstantiatedTo(i)) {
//                return true;
//            }
//        }
//        return false;
//    }
//
//    @Override
//    public boolean isSatisfied() {
//        TIntHashSet ans = new TIntHashSet();
//
//        for (IntDomainVar var : tuple) {
//            int val = var.getVal();
//            ans.add(val);
//            if (!set.isInDomainKernel(val)) {
//                return false;
//            }
//        }
//
//        return ans.size() == tuple.length && ans.size() == set.getKernelDomainSize();
//    }
//}
