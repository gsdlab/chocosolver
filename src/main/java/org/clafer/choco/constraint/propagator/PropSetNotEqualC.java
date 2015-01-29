package org.clafer.choco.constraint.propagator;

import java.util.Arrays;
import org.clafer.common.Util;
import org.chocosolver.solver.constraints.Propagator;
import org.chocosolver.solver.constraints.PropagatorPriority;
import org.chocosolver.solver.exception.ContradictionException;
import org.chocosolver.solver.variables.SetVar;
import org.chocosolver.solver.variables.delta.ISetDeltaMonitor;
import org.chocosolver.solver.variables.events.SetEventType;
import org.chocosolver.util.ESat;
import org.chocosolver.util.procedure.IntProcedure;

/**
 *
 * @author jimmy
 */
public class PropSetNotEqualC extends Propagator<SetVar> {

    private final SetVar s;
    private ISetDeltaMonitor sD;
    private final int[] c;

    public PropSetNotEqualC(SetVar s, int[] c) {
        super(new SetVar[]{s}, PropagatorPriority.LINEAR, true);
        this.s = s;
        this.sD = s.monitorDelta(aCause);
        this.c = c;
    }

    @Override
    public int getPropagationConditions(int vIdx) {
        return SetEventType.all();
    }

    private void checkNotSame() throws ContradictionException {
        if (s.isInstantiated()) {
            if (s.getKernelSize() == c.length) {
                int i = s.getKernelFirst();
                int j = 0;
                while (i != SetVar.END) {
                    if (i != c[j]) {
                        return;
                    }
                    i = s.getKernelNext();
                    j++;
                }
                assert j == c.length;
                contradiction(s, "Same");
            }
        }
    }

    @Override
    public void propagate(int evtmask) throws ContradictionException {
        for (int i = s.getKernelFirst(); i != SetVar.END; i = s.getKernelNext()) {
            if (!Util.in(i, c)) {
                setPassive();
                return;
            }
        }
        for (int i : c) {
            if (!s.envelopeContains(i)) {
                setPassive();
                return;
            }
        }
        checkNotSame();
    }

    @Override
    public void propagate(int idxVarInProp, int mask) throws ContradictionException {
        sD.freeze();
        sD.forEach(onS1Env, SetEventType.REMOVE_FROM_ENVELOPE);
        sD.forEach(onS1Ker, SetEventType.ADD_TO_KER);
        sD.unfreeze();
        checkNotSame();
    }
    private final IntProcedure onS1Env = new IntProcedure() {
        @Override
        public void execute(int s1Env) throws ContradictionException {
            if (isActive() && Util.in(s1Env, c)) {
                setPassive();
            }
        }
    };
    private final IntProcedure onS1Ker = new IntProcedure() {
        @Override
        public void execute(int s1Ker) throws ContradictionException {
            if (isActive() && !Util.in(s1Ker, c)) {
                setPassive();
            }
        }
    };

    private static boolean isEnvSubsetOf(SetVar s, int[] c) {
        for (int i = s.getKernelFirst(); i != SetVar.END; i = s.getKernelNext()) {
            if (!Util.in(i, c)) {
                return false;
            }
        }
        return true;
    }

    private static boolean isSubsetEnv(int[] c, SetVar s) {
        for (int i : c) {
            if (!s.envelopeContains(i)) {
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
