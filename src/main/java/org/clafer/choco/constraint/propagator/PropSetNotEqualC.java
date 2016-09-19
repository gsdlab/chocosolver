package org.clafer.choco.constraint.propagator;

import java.util.Arrays;
import org.chocosolver.solver.constraints.Propagator;
import org.chocosolver.solver.constraints.PropagatorPriority;
import org.chocosolver.solver.exception.ContradictionException;
import org.chocosolver.solver.variables.SetVar;
import org.chocosolver.solver.variables.delta.ISetDeltaMonitor;
import org.chocosolver.solver.variables.events.SetEventType;
import org.chocosolver.util.ESat;
import org.chocosolver.util.objects.setDataStructures.ISetIterator;
import org.clafer.common.Util;

/**
 *
 * @author jimmy
 */
public class PropSetNotEqualC extends Propagator<SetVar> {

    private final SetVar s;
    private final ISetDeltaMonitor sD;
    private final int[] c;

    public PropSetNotEqualC(SetVar s, int[] c) {
        super(new SetVar[]{s}, PropagatorPriority.LINEAR, true);
        this.s = s;
        this.sD = s.monitorDelta(this);
        this.c = c;
    }

    @Override
    public int getPropagationConditions(int vIdx) {
        return SetEventType.all();
    }

    private void checkNotSame() throws ContradictionException {
        if (s.isInstantiated()) {
            if (s.getLB().size() == c.length) {
                ISetIterator iter = s.getLB().iterator();
                int i = 0;
                while (iter.hasNext()) {
                    if (c[i] != iter.nextInt()) {
                        return;
                    }
                    i++;
                }
                assert i == c.length;
                fails();
            }
        }
    }

    @Override
    public void propagate(int evtmask) throws ContradictionException {
        ISetIterator iter = s.getLB().iterator();
        while (iter.hasNext()) {
            int i = iter.nextInt();
            if (!Util.in(i, c)) {
                setPassive();
                return;
            }
        }
        for (int i : c) {
            if (!s.getUB().contains(i)) {
                setPassive();
                return;
            }
        }
        checkNotSame();
    }

    @Override
    public void propagate(int idxVarInProp, int mask) throws ContradictionException {
        sD.freeze();
        sD.forEach(this::onS1Env, SetEventType.REMOVE_FROM_ENVELOPE);
        sD.forEach(this::onS1Ker, SetEventType.ADD_TO_KER);
        sD.unfreeze();
        checkNotSame();
    }

    private void onS1Env(int s1Env) throws ContradictionException {
        if (isActive() && Util.in(s1Env, c)) {
            setPassive();
        }
    }

    private void onS1Ker(int s1Ker) throws ContradictionException {
        if (isActive() && !Util.in(s1Ker, c)) {
            setPassive();
        }
    }

    private static boolean isEnvSubsetOf(SetVar s, int[] c) {
        ISetIterator iter = s.getLB().iterator();
        while (iter.hasNext()) {
            int i = iter.nextInt();
            if (!Util.in(i, c)) {
                return false;
            }
        }
        return true;
    }

    private static boolean isSubsetEnv(int[] c, SetVar s) {
        for (int i : c) {
            if (!s.getUB().contains(i)) {
                return false;
            }
        }
        return true;
    }

    @Override
    public ESat isEntailed() {
        if (!isEnvSubsetOf(s, c) || !isSubsetEnv(c, s)) {
            return ESat.TRUE;
        }
        return s.isInstantiated() ? ESat.FALSE : ESat.UNDEFINED;
    }

    @Override
    public String toString() {
        return s + " != " + Arrays.toString(c);
    }
}
