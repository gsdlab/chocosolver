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
import choco.kernel.solver.Configuration;
import choco.kernel.solver.ContradictionException;
import choco.kernel.solver.Solver;
import choco.kernel.solver.constraints.SConstraint;
import choco.kernel.solver.variables.integer.IntDomain;
import choco.kernel.solver.variables.integer.IntDomainVar;
import choco.kernel.solver.variables.set.SetDomain;
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

/**
 *
 * @author jimmy
 */
public class Util {

    public static boolean in(int item, int[] array) {
        for (int a : array) {
            if (a == item) {
                return true;
            }
        }
        return false;
    }

    public static <T> List<T> cons(T item, List<T> list) {
        List<T> r = new ArrayList<T>(list);
        r.add(item);
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

    public static <T> T[] combine(T[] first, T[] second) {
        T[] result = Arrays.copyOf(first, first.length + second.length);
        System.arraycopy(second, 0, result, first.length, second.length);
        return result;
    }

    // Optimize by taking size
    public static int[] iterateDomain(IntDomain dom) {
        DisposableIntIterator it = dom.getIterator();
        try {
            return iterate(it);
        } finally {
            it.dispose();
        }
    }

    // Optimize by taking size
    public static int[] iterateKer(SetDomain dom) {
        DisposableIntIterator it = dom.getKernelIterator();
        try {
            return iterate(it);
        } finally {
            it.dispose();
        }
    }

    // Optimize by taking size
    public static int[] iterateEnv(SetDomain dom) {
        DisposableIntIterator it = dom.getEnveloppeIterator();
        try {
            return iterate(it);
        } finally {
            it.dispose();
        }
    }

    public static int[] iterate(DisposableIntIterator it) {
        TIntArrayList i = new TIntArrayList();
        while (it.hasNext()) {
            i.add(it.next());
        }
        return i.toNativeArray();
    }

    public static int[] getVals(IntDomainVar... vars) {
        int[] vals = new int[vars.length];
        for (int i = 0; i < vars.length; i++) {
            vals[i] = vars[i].getVal();
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
        DisposableIntIterator it = sub.getDomain().getIterator();
        try {
            while (it.hasNext()) {
                int x = it.next();

                if (!sup.contains(x)) {
                    sub.removeVal(x, cause, false);
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

    public static Solver newSolver(Model m) {
        Solver solver = new CPSolver();
        solver.read(m);
        solver.getConfiguration().putInt(Configuration.LOGGING_MAX_DEPTH, 300000);
        return solver;
    }

    public static SolutionTest testSolutions(Model m) {
        return testSolutions(m, 1);
    }

    public static SolutionTest testSolutions(Model m, int repeat) {
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
        for (int i = 0; i < repeat; i++) {
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
    
    public static void main(String[] args) throws Exception {
        String[] s = new String[]{"abc", "def", "ghi"};
        System.out.println(Arrays.toString(cons("456", s)));
        System.out.println(Arrays.toString(cons("123", cons("456", s))));
    }
}
