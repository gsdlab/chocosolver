package org.clafer.choco.constraint.propagator;

import gnu.trove.TIntCollection;
import gnu.trove.iterator.TIntIterator;
import gnu.trove.set.TIntSet;
import gnu.trove.set.hash.TIntHashSet;
import java.io.File;
import java.io.FileNotFoundException;
import javax.script.ScriptException;
import org.clafer.ast.AstModel;
import org.clafer.collection.Pair;
import org.clafer.compiler.ClaferCompiler;
import org.clafer.compiler.ClaferSolver;
import org.clafer.javascript.Javascript;
import org.clafer.scope.Scope;
import solver.Configuration;
import solver.ICause;
import solver.exception.ContradictionException;
import solver.variables.IntVar;
import solver.variables.SetVar;
import solver.variables.delta.IIntDeltaMonitor;
import solver.variables.delta.monitor.SetDeltaMonitor;

/**
 * Various static utility functions for writing Choco propagators.
 *
 * @author jimmy
 */
public class PropUtil {

    public static void main(String[] args) throws FileNotFoundException, ScriptException {
        Pair<AstModel, Scope> p = Javascript.readModel(new File("/home/jimmy/Programming/clafer/AADL_simplified.js"));
        System.out.println(Configuration.IDEMPOTENCY);
        ClaferSolver s = ClaferCompiler.compile(p.getFst(), p.getSnd());
        System.out.println(s.getInternalSolver());
//        SearchMonitorFactory.log(s.getInternalSolver(), false, true);
//        SearchMonitorFactory.logContradiction(s.getInternalSolver());
//        SearchMonitorFactory.limitNode(s.getInternalSolver(), 2000);
        System.out.println(s.getInternalSolver().getVars().length);
        System.out.println(s.getInternalSolver().getCstrs().length);
//        52596
//        34450
//        59188
//        41436
//        58558
//        40809
//        48310
//        30327
//        48056
//        30093
//        46158
//        28195
//        34206
//        19987
//        33150
//        19277
//        32278
//        18067
        if (s.find()) {
            System.out.println(s.instance());
        }
    }

    private PropUtil() {
    }

    /**
     * Monitor the deltas for all the variables.
     *
     * @param vars the variables
     * @param propagator the propagator
     * @return the variables delta monitors
     */
    public static IIntDeltaMonitor[] monitorDeltas(IntVar[] vars, ICause propagator) {
        IIntDeltaMonitor[] deltas = new IIntDeltaMonitor[vars.length];
        for (int i = 0; i < vars.length; i++) {
            deltas[i] = vars[i].monitorDelta(propagator);
        }
        return deltas;
    }

    /**
     * Monitor the deltas for all the variables.
     *
     * @param vars the variables
     * @param propagator the propagator
     * @return the variables delta monitors
     */
    public static SetDeltaMonitor[] monitorDeltas(SetVar[] vars, ICause propagator) {
        SetDeltaMonitor[] deltas = new SetDeltaMonitor[vars.length];
        for (int i = 0; i < vars.length; i++) {
            deltas[i] = vars[i].monitorDelta(propagator);
        }
        return deltas;
    }

    /**
     * Checks if it is possible for the integer variables to instantiate to the
     * same value
     *
     * @param i1 the integer variable
     * @param i2 the integer variable
     * @return {@code true} if and only if {@code (dom(i1) intersect dom(i2)) !=
     *         empty set}, {@code false} otherwise
     */
    public static boolean domainIntersectDomain(IntVar i1, IntVar i2) {
        for (int i = i1.getLB(); i <= i1.getUB(); i = i1.nextValue(i)) {
            if (i2.contains(i)) {
                return true;
            }
        }
        return false;
//        IntVar small;
//        IntVar large;
//        if (i1.getDomainSize() < i2.getDomainSize()) {
//            small = i1;
//            large = i2;
//        } else {
//            small = i2;
//            large = i1;
//        }
//        DisposableRangeIterator iter = small.getRangeIterator(true);
//        try {
//            while (iter.hasNext()) {
//                if (large.nextValue(iter.min() - 1) <= iter.max()) {
//                    return true;
//                }
//                iter.next();
//            }
//        } finally {
//            iter.dispose();
//        }
//        return false;
    }

    /**
     * Checks if it is possible for an integer variable to instantiate to a
     * value in the set variable. Assumes the set variables envelope is sorted.
     *
     * @param ivar the integer variable
     * @param svar the set variable whose envelope is sorted
     * @return {@code true} if and only if {@code (dom(ivar) intersect env(svar)) !=
     *         empty set}, {@code false} otherwise
     */
    public static boolean domainIntersectEnv(IntVar ivar, SetVar svar) {
        if (ivar.getDomainSize() < svar.getEnvelopeSize()) {
            int i = ivar.getLB();
            int envFirst = svar.getEnvelopeFirst();
            if (envFirst >= i) {
                if (envFirst == i || ivar.contains(envFirst)) {
                    return true;
                }
                i = ivar.nextValue(envFirst);
            }
            int ub = ivar.getUB();
            for (; i <= ub; i = ivar.nextValue(i)) {
                if (svar.envelopeContains(i)) {
                    return true;
                }
            }
        } else {
            for (int i = svar.getEnvelopeFirst(); i != SetVar.END;) {
                int next = ivar.nextValue(i - 1);
                while (i < next && i != SetVar.END) {
                    i = svar.getEnvelopeNext();
                }
                if (i == next) {
                    return true;
                }
            }
        }
        return false;
    }

    /**
     * Checks if it is guaranteed that an integer variable instantiates to a
     * value in a collection.
     *
     * @param ivar the integer variable
     * @param collection the collection
     * @return {@code true} if and only if {@code dom(ivar) subsetof collection},
     *         {@code false} otherwise
     */
    public static boolean isDomainSubsetOf(IntVar ivar, TIntCollection collection) {
        if (ivar.getDomainSize() > collection.size()) {
            return false;
        }
        int ub = ivar.getUB();
        for (int i = ivar.getLB(); i <= ub; i = ivar.nextValue(i)) {
            if (!collection.contains(i)) {
                return false;
            }
        }
        return true;
    }

    /**
     * Checks if it is guaranteed that an integer variable instantiates to a
     * value in the set variable.
     *
     * @param ivar the integer variable
     * @param svar the set variable
     * @return {@code true} if and only if {@code dom(ivar) subsetof env(svar)},
     *         {@code false} otherwise
     */
    public static boolean isDomainSubsetEnv(IntVar ivar, SetVar svar) {
        if (ivar.getDomainSize() < svar.getKernelSize()) {
            int ub = ivar.getUB();
            for (int i = Math.max(ivar.getLB(), svar.getKernelFirst()); i <= ub; i = ivar.nextValue(i)) {
                if (!svar.kernelContains(i)) {
                    return false;
                }
            }
        } else {
            for (int i = svar.getKernelFirst(); i != SetVar.END;) {
                int next = ivar.nextValue(i - 1);
                while (i < next && i != SetVar.END) {
                    i = svar.getKernelNext();
                }
                if (i == next) {
                    return false;
                }
            }
        }
        return true;
    }

    /**
     * Checks if a collection is contained entirely in a set variable's
     * envelope.
     *
     * @param sub the subset
     * @param sup the superset
     * @return {@code true} if and only if {@code sub subsetof env(sup)},
     *         {@code false} otherwise
     */
    public static boolean isSubsetOfEnv(TIntCollection sub, SetVar sup) {
        TIntIterator iter = sub.iterator();
        while (iter.hasNext()) {
            if (!sup.envelopeContains(iter.next())) {
                return false;
            }
        }
        return true;
    }

    /**
     * Checks if a set variable's envelope is contained entirely in another set
     * variable's envelope.
     *
     * @param sub the subset
     * @param sup the superset
     * @return {@code true} if and only if {@code env(sub) subsetof env(sup)},
     *         {@code false} otherwise
     */
    public static boolean isEnvSubsetEnv(SetVar sub, SetVar sup) {
        for (int i = sub.getEnvelopeFirst(); i != SetVar.END; i = sub.getEnvelopeNext()) {
            if (!sup.envelopeContains(i)) {
                return false;
            }
        }
        return true;
    }

    /**
     * Checks if a set variable's envelope is contained entirely in the union of
     * other set variables' envelope.
     *
     * @param sub the subset
     * @param sups the superset union
     * @return {@code true} if and only if {@code env(sub) subsetof env(union(sups))},
     *         {@code false} otherwise
     */
    public static boolean isEnvSubsetEnvs(SetVar sub, SetVar[] sups) {
        for (int i = sub.getEnvelopeFirst(); i != SetVar.END; i = sub.getEnvelopeNext()) {
            if (!envsContain(sups, i)) {
                return false;
            }
        }
        return true;
    }

    /**
     * Checks if a set variable's kernel is contained entirely in another set
     * variable's envelope.
     *
     * @param sub the subset
     * @param sup the superset
     * @return {@code true} if and only if {@code ker(sub) subsetof env(sup)},
     *         {@code false} otherwise
     */
    public static boolean isKerSubsetEnv(SetVar sub, SetVar sup) {
        for (int i = sub.getKernelFirst(); i != SetVar.END; i = sub.getKernelNext()) {
            if (!sup.envelopeContains(i)) {
                return false;
            }
        }
        return true;
    }

    /**
     * Checks if a set variable's envelope is contained entirely in a
     * collection.
     *
     * @param sub the subset
     * @param sup the superset
     * @return {@code true} if and only if {@code ker(sub) subsetof sup},
     *         {@code false} otherwise
     */
    public static boolean isKerSubsetOf(SetVar sub, TIntCollection sup) {
        for (int i = sub.getKernelFirst(); i != SetVar.END; i = sub.getKernelNext()) {
            if (!sup.contains(i)) {
                return false;
            }
        }
        return true;
    }

    /**
     * Removes every element in the subset's domain that is not in the
     * collection.
     *
     * @param sub the subset
     * @param sup the superset
     * @param propagator the propagator
     * @throws ContradictionException
     */
    public static boolean domainSubsetOf(IntVar sub, TIntSet sup, ICause propagator) throws ContradictionException {
        int left = Integer.MIN_VALUE;
        int right = left;
        int ub = sub.getUB();
        boolean changed = false;
        for (int val = sub.getLB(); val <= ub; val = sub.nextValue(val)) {
            if (!sup.contains(val)) {
                if (val == right + 1) {
                    right = val;
                } else {
                    changed |= sub.removeInterval(left, right, propagator);
                    left = val;
                    right = val;
                }
            }
        }
        changed |= sub.removeInterval(left, right, propagator);
        return changed;
    }

    /**
     * Adds every element in the subset's kernel to the superset's kernel.
     *
     * @param sub the subset
     * @param sup the superset
     * @param propagator the propagator
     * @throws ContradictionException
     */
    public static void kerSubsetKer(SetVar sub, SetVar sup, ICause propagator) throws ContradictionException {
        for (int i = sub.getKernelFirst(); i != SetVar.END; i = sub.getKernelNext()) {
            sup.addToKernel(i, propagator);
        }
    }

    /**
     * Removes every element in the subset's envelope that is not in the
     * collection.
     *
     * @param sub the subset
     * @param sup the superset
     * @param propagator the propagator
     * @throws ContradictionException
     */
    public static void envSubsetOf(SetVar sub, TIntHashSet sup, ICause propagator) throws ContradictionException {
        for (int i = sub.getEnvelopeFirst(); i != SetVar.END; i = sub.getEnvelopeNext()) {
            if (!sup.contains(i)) {
                sub.removeFromEnvelope(i, propagator);
            }
        }
    }

    /**
     * Removes every element in the subset's envelope that is not the superset's
     * envelope.
     *
     * @param sub the subset
     * @param sup the superset
     * @param propagator the propagator
     * @throws ContradictionException
     */
    public static void envSubsetEnv(SetVar sub, SetVar sup, ICause propagator) throws ContradictionException {
        for (int i = sub.getEnvelopeFirst(); i != SetVar.END; i = sub.getEnvelopeNext()) {
            if (!sup.envelopeContains(i)) {
                sub.removeFromEnvelope(i, propagator);
            }
        }
    }

    /**
     * Removes every element in the subset's envelope that is not in any of the
     * supersets' envelope.
     *
     * @param sub the subset
     * @param sups the superset union
     * @param propagator the propagator
     * @throws ContradictionException
     */
    public static void envSubsetEnvs(SetVar sub, SetVar[] sups, ICause propagator) throws ContradictionException {
        for (int i = sub.getEnvelopeFirst(); i != SetVar.END; i = sub.getEnvelopeNext()) {
            if (!envsContain(sups, i)) {
                sub.removeFromEnvelope(i, propagator);
            }
        }
    }

    /**
     * @param union
     * @param val
     * @return {@code true} if and only if one of the set variables' envelope
     * contain {@code val},
     *         {@code false} otherwise
     */
    public static boolean envsContain(SetVar[] union, int val) {
        for (SetVar var : union) {
            if (var.envelopeContains(val)) {
                return true;
            }
        }
        return false;
    }

    /**
     * Removes every element in the integer variable's domain that is not in the
     * envelope of the set variable.
     *
     * @param sub the subset
     * @param sup the superset
     * @param propagator the propagator
     * @throws ContradictionException
     */
    public static void intSubsetEnv(IntVar sub, SetVar sup, ICause propagator) throws ContradictionException {
        int left = Integer.MIN_VALUE;
        int right = left;
        int ub = sub.getUB();
        for (int val = sub.getLB(); val <= ub; val = sub.nextValue(val)) {
            if (!sup.envelopeContains(val)) {
                if (val == right + 1) {
                    right = val;
                } else {
                    sub.removeInterval(left, right, propagator);
                    left = val;
                    right = val;
                }
            }
        }
        sub.removeInterval(left, right, propagator);
    }

    /**
     * Removes every element in the integer variables' domain that is not in the
     * envelope of the set variable.
     *
     * @param subs the subsets
     * @param sup the superset
     * @param propagator the propagator
     * @throws ContradictionException
     */
    public static void intsSubsetEnv(IntVar[] subs, SetVar sup, ICause propagator) throws ContradictionException {
        for (IntVar sub : subs) {
            intSubsetEnv(sub, sup, propagator);
        }
    }

    /**
     * Removes every element in the set variable's envelope that is not in the
     * integer variable's domain.
     *
     * @param sub the subset
     * @param sup the superset
     * @param propagator the propagator
     * @throws ContradictionException
     */
    public static void envSubsetInt(SetVar sub, IntVar sup, ICause propagator) throws ContradictionException {
        for (int val = sub.getEnvelopeFirst(); val != SetVar.END; val = sub.getEnvelopeNext()) {
            if (!sup.contains(val)) {
                sub.removeFromEnvelope(val, propagator);
            }
        }
    }

    /**
     * Removes every element in the set variable's envelope that is not in any
     * of integer variables' domain.
     *
     * @param sub the subset
     * @param sups the superset union
     * @param propagator the propagator
     * @throws ContradictionException
     */
    public static void envSubsetInts(SetVar sub, IntVar[] sups, ICause propagator) throws ContradictionException {
        for (int val = sub.getEnvelopeFirst(); val != SetVar.END; val = sub.getEnvelopeNext()) {
            if (!domainsContain(sups, val)) {
                sub.removeFromEnvelope(val, propagator);
            }
        }
    }

    /**
     * @param union
     * @param val
     * @return {@code true} if and only if one of the integer variables contain {@code val},
     *         {@code false} otherwise
     */
    public static boolean domainsContain(IntVar[] union, int val) {
        for (IntVar var : union) {
            if (var.contains(val)) {
                return true;
            }
        }
        return false;
    }

    /**
     * Enumerate the domain of a integer variable.
     *
     * @param ivar the integer variable
     * @return {@code dom(int)}
     */
    public static int[] iterateDomain(IntVar ivar) {
        int[] iterate = new int[ivar.getDomainSize()];
        int count = 0;
        int ub = ivar.getUB();
        for (int i = ivar.getLB(); i <= ub; i = ivar.nextValue(i)) {
            iterate[count++] = i;
        }
        assert count == iterate.length;
        return iterate;
    }

    /**
     * Enumerate the domain of a integer variable.
     *
     * @param ivar the integer variable
     * @param collection add {@code env(set)} into here
     */
    public static void iterateDomain(IntVar ivar, TIntCollection collection) {
        int ub = ivar.getUB();
        for (int i = ivar.getLB(); i <= ub; i = ivar.nextValue(i)) {
            collection.add(i);
        }
    }

    /**
     * Enumerate the envelope of a set variable.
     *
     * @param set the set variable
     * @return {@code env(set)}
     */
    public static int[] iterateEnv(SetVar set) {
        int[] iterate = new int[set.getEnvelopeSize()];
        int count = 0;
        for (int i = set.getEnvelopeFirst(); i != SetVar.END; i = set.getEnvelopeNext()) {
            iterate[count++] = i;
        }
        assert count == iterate.length;
        return iterate;
    }

    /**
     * Enumerate the envelope of a set variable.
     *
     * @param set the set variable
     * @param collection add {@code env(set)} into here
     */
    public static void iterateEnv(SetVar set, TIntCollection collection) {
        for (int i = set.getEnvelopeFirst(); i != SetVar.END; i = set.getEnvelopeNext()) {
            collection.add(i);
        }
    }

    /**
     * Enumerate the kernel of a set variable.
     *
     * @param set the set variable
     * @return {@code ker(set)}
     */
    public static int[] iterateKer(SetVar set) {
        int[] iterate = new int[set.getKernelSize()];
        int count = 0;
        for (int i = set.getKernelFirst(); i != SetVar.END; i = set.getKernelNext()) {
            iterate[count++] = i;
        }
        assert count == iterate.length;
        return iterate;
    }

    /**
     * Enumerate the kernel of a set variable.
     *
     * @param set the set variable
     * @param collection add {@code ker(set)} into here
     */
    public static void iterateKer(SetVar set, TIntCollection collection) {
        for (int i = set.getKernelFirst(); i != SetVar.END; i = set.getKernelNext()) {
            collection.add(i);
        }
    }

    /**
     * Returns the integer variables' values. Assumes the variables are
     * instantiated.
     *
     * @param vars the variables
     * @return the variables' values
     */
    public static int[] getValues(IntVar[] vars) {
        int[] values = new int[vars.length];
        for (int i = 0; i < values.length; i++) {
            assert vars[i].instantiated();
            values[i] = vars[i].getValue();
        }
        return values;
    }

    /**
     * Returns the integer variables' values. Assumes the variables are
     * instantiated.
     *
     * @param vars the variables
     * @return the variables' values
     */
    public static int[][] getValues(SetVar[] vars) {
        int[][] values = new int[vars.length][];
        for (int i = 0; i < values.length; i++) {
            assert vars[i].instantiated();
            values[i] = vars[i].getValue();
        }
        return values;
    }
}
