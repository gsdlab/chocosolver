package org.clafer;

import choco.cp.solver.CPSolver;
import choco.cp.solver.search.integer.branching.AssignVar;
import choco.cp.solver.search.integer.valiterator.IncreasingDomain;
import choco.cp.solver.search.integer.valselector.RandomIntValSelector;
import choco.cp.solver.search.integer.varselector.MinDomain;
import choco.cp.solver.search.integer.varselector.RandomIntVarSelector;
import choco.cp.solver.search.set.AssignSetVar;
import choco.cp.solver.search.set.MaxDomSet;
import choco.cp.solver.search.set.MinEnv;
import choco.cp.solver.search.set.RandomSetValSelector;
import choco.cp.solver.search.set.RandomSetVarSelector;
import choco.kernel.common.util.iterators.DisposableIntIterator;
import choco.kernel.model.Model;
import choco.kernel.model.variables.VariableType;
import choco.kernel.model.variables.integer.IntegerConstantVariable;
import choco.kernel.model.variables.integer.IntegerExpressionVariable;
import choco.kernel.model.variables.integer.IntegerVariable;
import choco.kernel.model.variables.set.SetConstantVariable;
import choco.kernel.model.variables.set.SetVariable;
import choco.kernel.solver.Configuration;
import choco.kernel.solver.ContradictionException;
import choco.kernel.solver.Solver;
import choco.kernel.solver.constraints.SConstraint;
import choco.kernel.solver.variables.integer.IntDomainVar;
import choco.kernel.solver.variables.set.SetSubDomain;
import choco.kernel.solver.variables.set.SetVar;
import gnu.trove.TIntArrayList;
import gnu.trove.TIntHashSet;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import org.clafer.collection.IntIterator;
import org.clafer.tree.AtomicClafer;
import org.clafer.tree.ConcreteClafer;

/**
 *
 * @author jimmy
 */
public class Util {

    /**
     * 
     * @param variable
     * @return The integer value if variable is a constant, null otherwise.
     */
    public static Integer getConstant(IntegerVariable variable) {
        if (variable.getVariableType().equals(VariableType.CONSTANT_INTEGER)) {
            return ((IntegerConstantVariable) variable).getValue();
        }
        if (variable.isConstant()) {
            return variable.getLowB();
        }
        return null;
    }

    public static Integer getConstant(IntegerExpressionVariable variable) {
        if (variable instanceof IntegerVariable) {
            return getConstant((IntegerVariable) variable);
        }
        if (variable.getVariableType().equals(VariableType.CONSTANT_INTEGER)) {
            return ((IntegerConstantVariable) variable).getValue();
        }
        return null;
    }

    /**
     * 
     * @param variable
     * @return The integer values if variable is a constant, null otherwise.
     */
    public static int[] getConstant(SetVariable variable) {
        if (variable.getVariableType().equals(VariableType.CONSTANT_SET)) {
            return ((SetConstantVariable) variable).getValues();
        }
        return null;
    }

    // http://stackoverflow.com/questions/8095045/java-array-order-reversing
    public static void reverse(int[] a, int to) {
        for (int j = 0; j < to / 2; j++) {
            int temp = a[j];
            a[j] = a[to - j - 1];
            a[to - j - 1] = temp;
        }
    }

    public static <T> void reverse(T[] a) {
        reverse(a, a.length);
    }

    public static <T> void reverse(T[] a, int to) {
        for (int j = 0; j < to / 2; j++) {
            T temp = a[j];
            a[j] = a[to - j - 1];
            a[to - j - 1] = temp;
        }
    }

    public static IntegerVariable[][] transpose(IntegerVariable[][] a) {
        Check.noNulls(a);
        if (a.length == 0) {
            return new IntegerVariable[][]{};
        }
        int wide = a[0].length;
        for (IntegerVariable[] b : a) {
            Check.noNulls(b);
            if (b.length != wide) {
                throw new IllegalArgumentException();
            }
        }
        IntegerVariable[][] z = new IntegerVariable[wide][a.length];
        for (int i = 0; i < a.length; i++) {
            for (int j = 0; j < wide; j++) {
                z[j][i] = a[i][j];
            }
        }
        return z;
    }

    public static int[] domainSizes(IntDomainVar... vars) {
        int[] sizes = new int[vars.length];
        for (int i = 0; i < vars.length; i++) {
            sizes[i] = vars[i].getDomainSize();
        }
        return sizes;
    }

    public static int maximum(int[] is) {
        if (is.length == 0) {
            throw new IllegalArgumentException();
        }
        int max = is[0];
        for (int i = 1; i < is.length; i++) {
            max = Math.max(max, is[i]);
        }
        return max;
    }

    public static boolean in(int item, int[] array) {
        for (int a : array) {
            if (a == item) {
                return true;
            }
        }
        return false;
    }

    public static <T> List<T> cons(T head, List<? extends T> tail) {
        List<T> r = new ArrayList<T>(tail.size() + 1);
        r.add(head);
        r.addAll(tail);
        return r;
    }

    public static <T> List<T> cons(List<? extends T> head, T tail) {
        List<T> r = new ArrayList<T>(head.size() + 1);
        r.addAll(head);
        r.add(tail);
        return r;
    }

    public static <T> T[] cons(T item, T[] list) {
        T[] r = Arrays.copyOf(list, list.length + 1);
        for (int i = r.length - 1; i > 0; i--) {
            r[i] = r[i - 1];
        }
        r[0] = item;
        return r;
    }

    public static <T> T[] cons(T[] list, T item) {
        T[] r = Arrays.copyOf(list, list.length + 1);
        r[list.length] = item;
        return r;
    }

    public static <T> List<T> add(List<T> list, T... items) {
        list.addAll(Arrays.asList(items));
        return list;
    }

    public static <T> List<T> concat(List<T>... concats) {
        List<T> r = new ArrayList<T>();
        for (List<T> c : concats) {
            r.addAll(c);
        }
        return r;
    }

    public static int sumAndDispose(DisposableIntIterator it) {
        int sum = 0;
        try {
            while (it.hasNext()) {
                sum += it.next();
            }
        } finally {
            it.dispose();
        }
        return sum;
    }

    public static int sum(int[] is) {
        int sum = 0;
        for (int i : is) {
            sum += i;
        }
        return sum;
    }

    public static <T> T[] combine(T[] first, T[] second) {
        T[] result = Arrays.copyOf(first, first.length + second.length);
        System.arraycopy(second, 0, result, first.length, second.length);
        return result;
    }

    public static int[] iterateDomain(IntDomainVar var) {
        int[] a = new int[var.getDomainSize()];
        DisposableIntIterator it = var.getDomain().getIterator();
        try {
            for (int i = 0; i < a.length; i++) {
                assert it.hasNext();
                a[i] = it.next();
            }
            assert !it.hasNext();
        } finally {
            it.dispose();
        }
        return a;
    }

    public static int[] iterateKer(SetVar var) {
        int[] a = new int[var.getKernelDomainSize()];
        DisposableIntIterator it = var.getDomain().getKernelIterator();
        try {
            for (int i = 0; i < a.length; i++) {
                assert it.hasNext();
                a[i] = it.next();
            }
            assert !it.hasNext();
        } finally {
            it.dispose();
        }
        return a;
    }

    public static int[] iterateEnv(SetVar var) {
        int[] a = new int[var.getEnveloppeDomainSize()];
        DisposableIntIterator it = var.getDomain().getEnveloppeIterator();
        try {
            for (int i = 0; i < a.length; i++) {
                assert it.hasNext();
                a[i] = it.next();
            }
            assert !it.hasNext();
        } finally {
            it.dispose();
        }
        return a;
    }

    public static int[] iterate(IntIterator it) {
        TIntArrayList i = new TIntArrayList();
        while (it.hasNext()) {
            i.add(it.next());
        }
        return i.toNativeArray();
    }

    public static int[] iterate(DisposableIntIterator it) {
        TIntArrayList i = new TIntArrayList();
        while (it.hasNext()) {
            i.add(it.next());
        }
        return i.toNativeArray();
    }

    public static int[] iterateAndDispose(DisposableIntIterator it) {
        try {
            return iterate(it);
        } finally {
            it.dispose();
        }
    }

    public static int[] getVals(IntDomainVar... vars) {
        Check.noNulls(vars);
        int[] vals = new int[vars.length];
        for (int i = 0; i < vars.length; i++) {
            vals[i] = vars[i].getVal();
        }
        return vals;
    }

    public static int[][] getVals(SetVar... vars) {
        Check.noNulls(vars);
        int[][] vals = new int[vars.length][];
        for (int i = 0; i < vars.length; i++) {
            vals[i] = vars[i].getValue();
        }
        return vals;
    }

    public static void subsetOf(SConstraint cause, IntDomainVar sub, IntDomainVar sup) throws ContradictionException {
        DisposableIntIterator it = sub.getDomain().getIterator();
        try {
            while (it.hasNext()) {
                int x = it.next();

                if (!sup.canBeInstantiatedTo(x)) {
                    sub.removeVal(x, cause, false);
                }
            }
        } finally {
            it.dispose();
        }
    }

    public static void subsetOf(SConstraint cause, IntDomainVar sub, SetSubDomain sup) throws ContradictionException {
        int left = Integer.MIN_VALUE;
        int right = left;
        DisposableIntIterator it = sub.getDomain().getIterator();
        try {
            while (it.hasNext()) {
                int x = it.next();
                if (!sup.contains(x)) {
                    if (x == right + 1) {
                        right = x;
                    } else {
                        sub.removeInterval(left, right, cause, false);
                        left = x;
                        right = x;
                    }
                }
            }
            if (left != right) {
                sub.removeInterval(left, right, cause, false);
            }
        } finally {
            it.dispose();
        }
    }

    public static void envSubsetOf(SConstraint cause, SetVar sub, IntDomainVar sup) throws ContradictionException {
        DisposableIntIterator it = sub.getDomain().getEnveloppeIterator();
        try {
            while (it.hasNext()) {
                int x = it.next();

                if (!sup.canBeInstantiatedTo(x)) {
                    sub.remFromEnveloppe(x, cause, false);
                }
            }
        } finally {
            it.dispose();
        }
    }

    public static void envSubsetOf(SConstraint cause, SetVar sub, SetVar sup) throws ContradictionException {
        DisposableIntIterator it = sub.getDomain().getEnveloppeIterator();
        try {
            while (it.hasNext()) {
                int x = it.next();

                if (!sup.isInDomainEnveloppe(x)) {
                    sub.remFromEnveloppe(x, cause, false);
                }
            }
        } finally {
            it.dispose();
        }
    }

    public static void kerSubsetOf(SConstraint cause, SetVar sub, SetVar sup) throws ContradictionException {
        DisposableIntIterator it = sub.getDomain().getKernelIterator();
        try {
            while (it.hasNext()) {
                sup.addToKernel(it.next(), cause, false);
            }
        } finally {
            it.dispose();
        }
    }

    public static TIntHashSet enumerate(DisposableIntIterator it) {
        TIntHashSet i = new TIntHashSet();
        while (it.hasNext()) {
            i.add(it.next());
        }
        return i;
    }

    public static TIntHashSet enumerateAndDispose(DisposableIntIterator it) {
        try {
            return enumerate(it);
        } finally {
            it.dispose();
        }
    }

    public static void enumerateEnv(TIntHashSet set, SetVar var) {
        DisposableIntIterator it = var.getDomain().getEnveloppeIterator();
        try {
            while (it.hasNext()) {
                set.add(it.next());
            }
        } finally {
            it.dispose();
        }
    }

    public static void pruneEnv(SConstraint cause, TIntHashSet keep, SetVar var) throws ContradictionException {
        DisposableIntIterator it = var.getDomain().getEnveloppeIterator();
        try {
            while (it.hasNext()) {
                int x = it.next();
                if (!keep.contains(x)) {
                    var.remFromEnveloppe(x, cause, false);
                }
            }
        } finally {
            it.dispose();
        }
    }

    /**
     * @return Two variables has intersecting domains. Can return false positives.
     */
    public static boolean approxIntersects(IntDomainVar e1, SetVar e2) {
        if (e1.isInstantiated()) {
            return e2.isInDomainEnveloppe(e1.getVal());
        }
        if (Math.min(e1.getDomainSize(), e2.getEnveloppeDomainSize()) < 10) {
            return intersects(e1, e2);
        }
        return true;
    }

    public static boolean intersects(IntDomainVar e1, SetVar e2) {
        if (e1.getDomainSize() < e2.getEnveloppeDomainSize()) {
            DisposableIntIterator it = e1.getDomain().getIterator();
            try {
                while (it.hasNext()) {
                    if (e2.isInDomainEnveloppe(it.next())) {
                        return true;
                    }
                }
            } finally {
                it.dispose();
            }
        } else {
            DisposableIntIterator it = e2.getDomain().getEnveloppeIterator();
            try {
                while (it.hasNext()) {
                    if (e1.canBeInstantiatedTo(it.next())) {
                        return true;
                    }
                }
            } finally {
                it.dispose();
            }
        }
        return false;
    }

    public static List<ConcreteClafer> filterConcrete(List<AtomicClafer> clafers) {
        List<ConcreteClafer> concretes = new ArrayList<ConcreteClafer>();
        for (AtomicClafer clafer : clafers) {
            if (clafer instanceof ConcreteClafer) {
                concretes.add((ConcreteClafer) clafer);
            }
        }
        return concretes;
    }

    public static List<ConcreteClafer> filterInexactCard(List<ConcreteClafer> clafers) {
        List<ConcreteClafer> inexact = new ArrayList<ConcreteClafer>();
        for (ConcreteClafer clafer : clafers) {
            if (!clafer.getCard().isExact()) {
                inexact.add(clafer);
            }
        }
        return inexact;
    }

    public static Solution allSolutions(Model model) {
        return allSolutions(newSolver(model));
    }

    public static Solution allSolutions(Solver solver) {
        Set<String> solutions = new HashSet<String>();
        if (solver.solve()) {
            do {
                assert solver.checkSolution();
                if (!solutions.add(solver.solutionToString())) {
                    throw new IllegalStateException();
                }
            } while (solver.nextSolution());
        }
        return new Solution(solutions, "#" + solver.getSolutionCount() + " solutions " + solver.runtimeStatistics());
    }

    public static Solution oneSolution(Model model) {
        return oneSolution(newSolver(model));
    }

    public static Solution oneSolution(Solver solver) {
        if (solver.solve()) {
            assert solver.checkSolution();
            return new Solution(Collections.singleton(solver.solutionToString()), solver.runtimeStatistics());
        }
        throw new IllegalStateException();
    }

    public static Solver newSolver(Model m) {
        Solver solver = new CPSolver();
        solver.read(m);
        solver.getConfiguration().putInt(Configuration.LOGGING_MAX_DEPTH, 300000);
        return solver;
    }

    public static SolutionTest testSolutions(Model m, int repeat, boolean is) {
        Check.notNull(m);
        if (repeat < 1) {
            throw new IllegalArgumentException();
        }

        Solver defaultSolver = newSolver(m);
        Solution defaultSolution = allSolutions(defaultSolver);

        Solver customSolver = newSolver(m);
        customSolver.addGoal(new AssignSetVar(new MaxDomSet(customSolver), new MinEnv()));
        customSolver.addGoal(new AssignVar(new MinDomain(customSolver), new IncreasingDomain()));
        Solution customSolution = allSolutions(customSolver);

        List<Solution> randomSISolutions = new ArrayList<Solution>();
        for (int i = 0; i < repeat; i++) {
            Solver randomSISolver = newSolver(m);
            randomSISolver.addGoal(new AssignSetVar(new RandomSetVarSelector(randomSISolver), new RandomSetValSelector()));
            randomSISolver.addGoal(new AssignVar(new RandomIntVarSelector(randomSISolver), new RandomIntValSelector()));
            Solution randomSISolution = allSolutions(randomSISolver);
            randomSISolutions.add(randomSISolution);
        }

        List<Solution> randomISSolutions = new ArrayList<Solution>();
        for (int i = 0; i < repeat && is; i++) {
            Solver randomISSolver = newSolver(m);
            randomISSolver.addGoal(new AssignVar(new RandomIntVarSelector(randomISSolver), new RandomIntValSelector()));
            randomISSolver.addGoal(new AssignSetVar(new RandomSetVarSelector(randomISSolver), new RandomSetValSelector()));
            Solution randomISSolution = allSolutions(randomISSolver);
            randomISSolutions.add(randomISSolution);
        }

        return new SolutionTest(
                defaultSolution,
                customSolution,
                randomSISolutions.toArray(new Solution[randomSISolutions.size()]),
                randomISSolutions.toArray(new Solution[randomISSolutions.size()]));


    }

    public static class Solution {

        private final Set<String> solutions;
        private final String statistics;

        public Solution(Set<String> solutions, String statistics) {
            this.solutions = Collections.unmodifiableSet(new HashSet<String>(Check.notNull(solutions)));
            this.statistics = Check.notNull(statistics);
        }

        public Set<String> getSolutions() {
            return solutions;
        }

        public String getStatistics() {
            return statistics;
        }

        public int size() {
            return solutions.size();
        }
    }

    public static class SolutionTest {

        private final Solution defaultSolution;
        private final Solution customSolution;
        private final Solution[] randomSISolutions;
        private final Solution[] randomISSolutions;

        public SolutionTest(Solution defaultSolution, Solution customSolution, Solution[] randomSISolutions, Solution[] randomISSolutions) {
            this.defaultSolution = Check.notNull(defaultSolution);
            this.customSolution = Check.notNull(customSolution);
            this.randomSISolutions = Check.notNull(randomSISolutions);
            this.randomISSolutions = Check.notNull(randomISSolutions);
        }

        public Solution getDefaultSolution() {
            return defaultSolution;
        }

        public Solution getCustomSolution() {
            return customSolution;
        }

        public Solution[] getRandomSISolutions() {
            return randomSISolutions;
        }

        public Solution[] getRandomISSolutions() {
            return randomISSolutions;
        }
    }
}
