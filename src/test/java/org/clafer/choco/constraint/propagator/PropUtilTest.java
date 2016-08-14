package org.clafer.choco.constraint.propagator;

import gnu.trove.set.TIntSet;
import gnu.trove.set.hash.TIntHashSet;
import java.util.Arrays;
import java.util.Random;
import static org.junit.Assert.*;
import org.junit.Test;
import org.chocosolver.solver.Cause;
import org.chocosolver.solver.ICause;
import org.chocosolver.solver.Model;
import org.chocosolver.solver.constraints.Propagator;
import org.chocosolver.solver.exception.ContradictionException;
import org.chocosolver.solver.propagation.IPropagationEngine;
import org.chocosolver.solver.variables.IntVar;
import org.chocosolver.solver.variables.SetVar;
import org.chocosolver.solver.variables.Variable;
import org.chocosolver.solver.variables.events.IEventType;
import org.chocosolver.solver.variables.events.PropagatorEventType;

/**
 *
 * @author jimmy
 */
public class PropUtilTest {

    @Test
    public void testGetEnv() {
        Model model = new Model();
        for (int i = 0; i < 100; i++) {
            SetVar s = randSetVar("s", model);
            int[] env = PropUtil.iterateEnv(s);
            for (int j = 0; j < env.length; j++) {
                assertEquals(j + " : " + Arrays.toString(env), env[j], PropUtil.getEnv(s, j));
            }
        }
    }

    @Test
    public void testIsDomIntersectDom() {
        Model model = new Model();
        for (int i = 0; i < 100; i++) {
            IntVar i1 = randIntVar("i1" + i, model);
            IntVar i2 = randIntVar("i2" + i, model);

            assertEquals(
                    isIntersectBruteForce(dom(i1), dom(i2)),
                    PropUtil.isDomIntersectDom(i1, i2));
        }
    }

    @Test
    public void testIsDomIntersectEnv() {
        Model model = new Model();
        for (int i = 0; i < 100; i++) {
            IntVar i1 = randIntVar("i1" + i, model);
            SetVar i2 = randSetVar("i2" + i, model);

            assertEquals(
                    isIntersectBruteForce(dom(i1), env(i2)),
                    PropUtil.isDomIntersectEnv(i1, i2));
        }
    }

    @Test
    public void testIsDomIntersectKer() {
        Model model = new Model();
        for (int i = 0; i < 100; i++) {
            IntVar i1 = randIntVar("i1" + i, model);
            SetVar i2 = randSetVar("i2" + i, model);

            assertEquals(
                    isIntersectBruteForce(dom(i1), ker(i2)),
                    PropUtil.isDomIntersectKer(i1, i2));
        }
    }

    @Test
    public void testIsEnvIntersectEnv() {
        Model model = new Model();
        for (int i = 0; i < 100; i++) {
            SetVar i1 = randSetVar("i1" + i, model);
            SetVar i2 = randSetVar("i2" + i, model);

            assertEquals(
                    isIntersectBruteForce(env(i1), env(i2)),
                    PropUtil.isEnvIntersectEnv(i1, i2));
        }
    }

    @Test
    public void testIsEnvIntersectKer() {
        Model model = new Model();
        for (int i = 0; i < 100; i++) {
            SetVar i1 = randSetVar("i1" + i, model);
            SetVar i2 = randSetVar("i2" + i, model);

            assertEquals(
                    isIntersectBruteForce(env(i1), ker(i2)),
                    PropUtil.isEnvIntersectKer(i1, i2));
        }
    }

    @Test
    public void testIsKerIntersectKer() {
        Model model = new Model();
        for (int i = 0; i < 100; i++) {
            SetVar i1 = randSetVar("i1" + i, model);
            SetVar i2 = randSetVar("i2" + i, model);

            assertEquals(
                    isIntersectBruteForce(ker(i1), ker(i2)),
                    PropUtil.isKerIntersectKer(i1, i2));
        }
    }

    @Test
    public void testIsDomSubsetDom() {
        Model model = new Model();
        for (int i = 0; i < 100; i++) {
            IntVar i1 = randIntVar("i1" + i, model);
            IntVar i2 = randIntVar("i2" + i, model);

            assertEquals(
                    isSubsetBruteForce(dom(i1), dom(i2)),
                    PropUtil.isDomSubsetDom(i1, i2));
        }
    }

    @Test
    public void testIsDomSubsetEnv() {
        Model model = new Model();
        for (int i = 0; i < 100; i++) {
            IntVar i1 = randIntVar("i1" + i, model);
            SetVar i2 = randSetVar("i2" + i, model);

            assertEquals(i1 + "," + i2,
                    isSubsetBruteForce(dom(i1), env(i2)),
                    PropUtil.isDomSubsetEnv(i1, i2));
        }
    }

    @Test
    public void testIsDomSubsetKer() {
        Model model = new Model();
        for (int i = 0; i < 100; i++) {
            IntVar i1 = randIntVar("i1" + i, model);
            SetVar i2 = randSetVar("i2" + i, model);

            assertEquals(
                    isSubsetBruteForce(dom(i1), ker(i2)),
                    PropUtil.isDomSubsetKer(i1, i2));
        }
    }

    @Test
    public void testIsEnvSubsetDom() {
        Model model = new Model();
        for (int i = 0; i < 100; i++) {
            SetVar i1 = randSetVar("i1" + i, model);
            IntVar i2 = randIntVar("i2" + i, model);

            assertEquals(
                    isSubsetBruteForce(env(i1), dom(i2)),
                    PropUtil.isEnvSubsetDom(i1, i2));
        }
    }

    @Test
    public void testIsEnvSubsetEnv() {
        Model model = new Model();
        for (int i = 0; i < 100; i++) {
            SetVar i1 = randSetVar("i1" + i, model);
            SetVar i2 = randSetVar("i2" + i, model);

            assertEquals(i1 + "," + i2,
                    isSubsetBruteForce(env(i1), env(i2)),
                    PropUtil.isEnvSubsetEnv(i1, i2));
        }
    }

    @Test
    public void testIsEnvSubsetKer() {
        Model model = new Model();
        for (int i = 0; i < 100; i++) {
            SetVar i1 = randSetVar("i1" + i, model);
            SetVar i2 = randSetVar("i2" + i, model);

            assertEquals(
                    isSubsetBruteForce(env(i1), ker(i2)),
                    PropUtil.isEnvSubsetKer(i1, i2));
        }
    }

    @Test
    public void testIsKerSubsetDom() {
        Model model = new Model();
        for (int i = 0; i < 100; i++) {
            SetVar i1 = randSetVar("i1" + i, model);
            IntVar i2 = randIntVar("i2" + i, model);

            assertEquals(
                    isSubsetBruteForce(ker(i1), dom(i2)),
                    PropUtil.isKerSubsetDom(i1, i2));
        }
    }

    @Test
    public void testIsKerSubsetEnv() {
        Model model = new Model();
        for (int i = 0; i < 100; i++) {
            SetVar i1 = randSetVar("i1" + i, model);
            SetVar i2 = randSetVar("i2" + i, model);

            assertEquals(i1 + "," + i2,
                    isSubsetBruteForce(ker(i1), env(i2)),
                    PropUtil.isKerSubsetEnv(i1, i2));
        }
    }

    @Test
    public void testIsKerSubsetKer() {
        Model model = new Model();
        for (int i = 0; i < 100; i++) {
            SetVar i1 = randSetVar("i1" + i, model);
            SetVar i2 = randSetVar("i2" + i, model);

            assertEquals(
                    isSubsetBruteForce(ker(i1), ker(i2)),
                    PropUtil.isKerSubsetKer(i1, i2));
        }
    }

    @Test
    public void testDomSubsetSet() throws ContradictionException {
        Model model = new Model();
        for (int i = 0; i < 100; i++) {
            IntVar i1 = randIntVar("i1" + i, model);
            TIntSet i2 = randSet();
            if (isIntersectBruteForce(dom(i1), set(i2))) {
                int s = i1.getDomainSize();
                boolean changed = PropUtil.domSubsetSet(i1, i2, Cause.Null);
                assertEquals(s == i1.getDomainSize(), !changed);
                assertTrue(isSubsetBruteForce(dom(i1), set(i2)));
            } else {
                try {
                    PropUtil.domSubsetSet(i1, i2, Cause.Null);
                } catch (ContradictionException e) {
                    continue;
                }
                fail();
            }
        }
    }

    @Test
    public void testDomSubsetDom() throws ContradictionException {
        Model model = new Model();
        for (int i = 0; i < 100; i++) {
            IntVar i1 = randIntVar("i1" + i, model);
            IntVar i2 = randIntVar("i2" + i, model);
            if (isIntersectBruteForce(dom(i1), dom(i2))) {
                int s = i1.getDomainSize();
                boolean changed = PropUtil.domSubsetDom(i1, i2, Cause.Null);
                assertEquals(s == i1.getDomainSize(), !changed);
                assertTrue(isSubsetBruteForce(dom(i1), dom(i2)));
            } else {
                try {
                    PropUtil.domSubsetDom(i1, i2, Cause.Null);
                } catch (ContradictionException e) {
                    continue;
                }
                fail();
            }
        }
    }

    @Test
    public void testDomSubsetEnv() throws ContradictionException {
        Model model = new Model();
        for (int i = 0; i < 100; i++) {
            IntVar i1 = randIntVar("i1" + i, model);
            SetVar i2 = randSetVar("i2" + i, model);
            if (isIntersectBruteForce(dom(i1), env(i2))) {
                int s = i1.getDomainSize();
                boolean changed = PropUtil.domSubsetEnv(i1, i2, Cause.Null);
                assertTrue(isSubsetBruteForce(dom(i1), env(i2)));
                assertEquals(s == i1.getDomainSize(), !changed);
            } else {
                try {
                    PropUtil.domSubsetEnv(i1, i2, Cause.Null);
                } catch (ContradictionException e) {
                    continue;
                }
                fail();
            }
        }
    }

    @Test
    public void testDomSubsetKer() throws ContradictionException {
        Model model = new Model();
        for (int i = 0; i < 100; i++) {
            IntVar i1 = randIntVar("i1" + i, model);
            SetVar i2 = randSetVar("i2" + i, model);
            if (isIntersectBruteForce(dom(i1), ker(i2))) {
                int s = i1.getDomainSize();
                boolean changed = PropUtil.domSubsetKer(i1, i2, Cause.Null);
                assertEquals(s == i1.getDomainSize(), !changed);
                assertTrue(isSubsetBruteForce(dom(i1), ker(i2)));
            } else {
                try {
                    PropUtil.domSubsetKer(i1, i2, Cause.Null);
                } catch (ContradictionException e) {
                    continue;
                }
                fail();
            }
        }
    }

    @Test
    public void testEnvSubsetSet() throws ContradictionException {
        Model model = new Model();
        for (int i = 0; i < 100; i++) {
            SetVar i1 = randSetVar("i1" + i, model);
            TIntSet i2 = randSet();
            if (isSubsetBruteForce(ker(i1), set(i2))) {
                int s = i1.getUB().size();
                boolean changed = PropUtil.envSubsetSet(i1, i2, Cause.Null);
                assertEquals(s == i1.getUB().size(), !changed);
                assertTrue(isSubsetBruteForce(env(i1), set(i2)));
            } else {
                try {
                    PropUtil.envSubsetSet(i1, i2, Cause.Null);
                } catch (ContradictionException e) {
                    continue;
                }
                fail();
            }
        }
    }

    @Test
    public void testEnvSubsetDom() throws ContradictionException {
        Model model = new Model();
        for (int i = 0; i < 100; i++) {
            SetVar i1 = randSetVar("i1" + i, model);
            IntVar i2 = randIntVar("i2" + i, model);
            if (isSubsetBruteForce(ker(i1), dom(i2))) {
                int s = i1.getUB().size();
                boolean changed = PropUtil.envSubsetDom(i1, i2, Cause.Null);
                assertEquals(s == i1.getUB().size(), !changed);
                assertTrue(isSubsetBruteForce(env(i1), dom(i2)));
            } else {
                try {
                    PropUtil.envSubsetDom(i1, i2, Cause.Null);
                } catch (ContradictionException e) {
                    continue;
                }
                fail();
            }
        }
    }

    @Test
    public void testEnvSubsetEnv() throws ContradictionException {
        Model model = new Model();
        for (int i = 0; i < 100; i++) {
            SetVar i1 = randSetVar("i1" + i, model);
            SetVar i2 = randSetVar("i2" + i, model);
            if (isSubsetBruteForce(ker(i1), env(i2))) {
                int s = i1.getUB().size();
                boolean changed = PropUtil.envSubsetEnv(i1, i2, Cause.Null);
                assertEquals(s == i1.getUB().size(), !changed);
                assertTrue(isSubsetBruteForce(env(i1), env(i2)));
            } else {
                try {
                    PropUtil.envSubsetEnv(i1, i2, Cause.Null);
                } catch (ContradictionException e) {
                    continue;
                }
                fail();
            }
        }
    }

    @Test
    public void testEnvSubsetKer() throws ContradictionException {
        Model model = new Model();
        for (int i = 0; i < 100; i++) {
            SetVar i1 = randSetVar("i1" + i, model);
            SetVar i2 = randSetVar("i2" + i, model);
            if (isSubsetBruteForce(ker(i1), ker(i2))) {
                int s = i1.getUB().size();
                boolean changed = PropUtil.envSubsetKer(i1, i2, Cause.Null);
                assertEquals(s == i1.getUB().size(), !changed);
                assertTrue(isSubsetBruteForce(env(i1), ker(i2)));
            } else {
                try {
                    PropUtil.envSubsetKer(i1, i2, Cause.Null);
                } catch (ContradictionException e) {
                    continue;
                }
                fail();
            }
        }
    }

    @Test
    public void testKerSubsetKer() throws ContradictionException {
        Model model = new Model();
        for (int i = 0; i < 100; i++) {
            SetVar i1 = randSetVar("i1" + i, model);
            SetVar i2 = randSetVar("i2" + i, model);
            if (isSubsetBruteForce(ker(i1), env(i2))) {
                int s = i2.getLB().size();
                boolean changed = PropUtil.kerSubsetKer(i1, i2, Cause.Null);
                assertEquals(s == i2.getLB().size(), !changed);
                assertTrue(isSubsetBruteForce(ker(i1), ker(i2)));
            } else {
                try {
                    PropUtil.kerSubsetKer(i1, i2, Cause.Null);
                } catch (ContradictionException e) {
                    continue;
                }
                fail();
            }
        }
    }

    private static Domain set(TIntSet set) {
        return new SetDomain(set);
    }

    private static Domain dom(IntVar var) {
        return new IntDomain(var);
    }

    private static Domain env(SetVar var) {
        return new EnvDomain(var);
    }

    private static Domain ker(SetVar var) {
        return new KerDomain(var);
    }

    private static interface Domain {

        boolean contains(int i);

        int[] values();
    }

    private static class SetDomain implements Domain {

        private final TIntSet set;

        SetDomain(TIntSet set) {
            this.set = set;
        }

        @Override
        public boolean contains(int i) {
            return set.contains(i);
        }

        @Override
        public int[] values() {
            return set.toArray();
        }
    }

    private static class IntDomain implements Domain {

        private final IntVar var;

        IntDomain(IntVar var) {
            this.var = var;
        }

        @Override
        public boolean contains(int i) {
            return var.contains(i);
        }

        @Override
        public int[] values() {
            return PropUtil.iterateDom(var);
        }
    }

    private static class EnvDomain implements Domain {

        private final SetVar var;

        EnvDomain(SetVar var) {
            this.var = var;
        }

        @Override
        public boolean contains(int i) {
            return var.getUB().contains(i);
        }

        @Override
        public int[] values() {
            return PropUtil.iterateEnv(var);
        }
    }

    private static class KerDomain implements Domain {

        private final SetVar var;

        KerDomain(SetVar var) {
            this.var = var;
        }

        @Override
        public boolean contains(int i) {
            return var.getLB().contains(i);
        }

        @Override
        public int[] values() {
            return PropUtil.iterateKer(var);
        }
    }
    private final Random rand = new Random();
    private static final int problemSize = 10;

    private static boolean isIntersectBruteForce(Domain i1, Domain i2) {
        for (int i : i1.values()) {
            if (i2.contains(i)) {
                return true;
            }
        }
        return false;
    }

    private static boolean isSubsetBruteForce(Domain i1, Domain i2) {
        for (int i : i1.values()) {
            if (!i2.contains(i)) {
                return false;
            }
        }
        return true;
    }

    private int randInt() {
        return rand.nextInt(problemSize * 2 + 1) - problemSize;
    }

    private TIntSet randSet() {
        int size = rand.nextInt(problemSize) + 1;
        TIntSet set = new TIntHashSet(size);
        for (int i = 0; i < size; i++) {
            set.add(randInt());
        }
        return set;
    }

    private IntVar randIntVar(String name, Model model) {
        int size = rand.nextInt(problemSize) + 1;
        TIntHashSet domain = new TIntHashSet(size);
        for (int i = 0; i < size; i++) {
            domain.add(randInt());
        }
        int[] domainArray = domain.toArray();
        Arrays.sort(domainArray);
        return model.intVar(name, domainArray);
    }

    private SetVar randSetVar(String name, Model model) {
        int size = rand.nextInt(problemSize) + 1;
        TIntHashSet env = new TIntHashSet(size);
        for (int i = 0; i < size; i++) {
            env.add(randInt());
        }
        TIntHashSet ker = new TIntHashSet(size);
        for (int i = 0; i < size; i++) {
            ker.add(randInt());
        }
        ker.retainAll(env);
        int[] envArray = env.toArray();
        Arrays.sort(envArray);
        int[] kerArray = ker.toArray();
        Arrays.sort(kerArray);
        return model.setVar(name, kerArray, envArray);
    }

    private static class DummyEngine implements IPropagationEngine {

        @Override
        public boolean isInitialized() {
            return false;
        }

        @Override
        public void propagate() throws ContradictionException {
        }

        @Override
        public void delayedPropagation(Propagator propagator, PropagatorEventType type) throws ContradictionException {
        }

        @Override
        public void flush() {
        }

        @Override
        public void fails(ICause cause, Variable variable, String message) throws ContradictionException {
            throw new ContradictionException().set(cause, variable, message);
        }

        @Override
        public ContradictionException getContradictionException() {
            return null;
        }

        @Override
        public void clear() {
        }

        @Override
        public void onVariableUpdate(Variable variable, IEventType type, ICause cause) {
        }

        @Override
        public void onPropagatorExecution(Propagator propagator) {
        }

        @Override
        public void desactivatePropagator(Propagator propagator) {
        }
    }
}
