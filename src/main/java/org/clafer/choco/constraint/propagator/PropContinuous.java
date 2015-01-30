package org.clafer.choco.constraint.propagator;

import org.chocosolver.solver.constraints.Propagator;
import org.chocosolver.solver.constraints.PropagatorPriority;
import org.chocosolver.solver.exception.ContradictionException;
import org.chocosolver.solver.variables.IntVar;
import org.chocosolver.solver.variables.SetVar;
import org.chocosolver.solver.variables.Variable;
import org.chocosolver.solver.variables.events.IntEventType;
import org.chocosolver.solver.variables.events.SetEventType;
import org.chocosolver.util.ESat;

/**
 *
 * @author jimmy
 */
public class PropContinuous extends Propagator<Variable> {

    private static final long serialVersionUID = 1L;

    private final SetVar set;
    private final IntVar card;

    public PropContinuous(SetVar set, IntVar card) {
        super(new Variable[]{set, card}, PropagatorPriority.QUADRATIC, false);
        this.set = set;
        this.card = card;
    }

    private boolean isSetVar(int idx) {
        return idx == 0;
    }

    private boolean isCardVar(int idx) {
        return idx == 1;
    }

    @Override
    protected int getPropagationConditions(int vIdx) {
        if (isSetVar(vIdx)) {
            return SetEventType.all();
        }
        assert isCardVar(vIdx);
        return IntEventType.VOID.getMask();
    }

    public int maxKer(SetVar set) {
        int max = SetVar.END;
        for (int i = set.getKernelFirst(); i != SetVar.END; i = set.getKernelNext()) {
            max = i;
        }
        return max;
    }

    public int maxEnv(SetVar set) {
        int max = SetVar.END;
        for (int i = set.getEnvelopeFirst(); i != SetVar.END; i = set.getEnvelopeNext()) {
            max = i;
        }
        return max;
    }

    @Override
    public void propagate(int evtmask) throws ContradictionException {
        if (set.getKernelSize() > 0) {
            int cur = set.getKernelFirst();
            assert cur != SetVar.END;
            for (int next = set.getKernelNext(); next != SetVar.END; next = set.getKernelNext()) {
                for (int j = cur + 1; j < next; j++) {
                    set.addToKernel(j, aCause);
                }
                cur = next;
            }
            if (!set.isInstantiated()) {
                int min = set.getKernelFirst();

                int prev = set.getEnvelopeFirst();
                int[] queue = new int[set.getEnvelopeSize() - set.getKernelSize() + 1];
                queue[0] = prev;
                int size = 1;
                int i;
                for (i = set.getEnvelopeNext(); prev < min; i = set.getEnvelopeNext()) {
                    if (i > prev + 1) {
                        for (int j = 0; j < size; j++) {
                            set.removeFromEnvelope(queue[j], aCause);
                        }
                        size = 0;
                    }
                    prev = i;
                    queue[size++] = prev;
                }
                if (prev != SetVar.END) {
                    for (; i != SetVar.END && i == prev + 1; i = set.getEnvelopeNext()) {
                        prev = i;
                    }
                    if (i != SetVar.END) {
                        for (; i != SetVar.END; i = set.getEnvelopeNext()) {
                            set.removeFromEnvelope(i, aCause);
                        }
                    }
                }
            }
        } else if (set.getEnvelopeSize() > 0) {
            int prev = set.getEnvelopeFirst();
            int i;
            int max = 0;
            int[] region = card.getLB() >= 2 ? new int[card.getLB() - 1] : null;
            do {
                if (region != null) {
                    region[0] = prev;
                }
                int size = 1;
                for (i = set.getEnvelopeNext(); i != SetVar.END && prev + 1 == i; i = set.getEnvelopeNext()) {
                    prev = i;
                    if (region != null && size < region.length) {
                        region[size] = prev;
                    }
                    size++;
                }
                if (region != null && size <= region.length) {
                    for (int z = 0; z < size; z++) {
                        set.removeFromEnvelope(region[z], aCause);
                    }
                }
                prev = i;
                max = Math.max(max, size);
            } while (i != SetVar.END);

            card.updateUpperBound(max, aCause);
        }
    }

    @Override
    public ESat isEntailed() {
        int cur = set.getKernelFirst();
        if (cur != SetVar.END) {
            for (int next = set.getKernelNext(); next != SetVar.END; next = set.getKernelNext()) {
                for (int j = cur + 1; j < next; j++) {
                    if (!set.envelopeContains(j)) {
                        return ESat.FALSE;
                    }
                }
            }
        }
        return set.isInstantiated() ? ESat.TRUE : ESat.UNDEFINED;
    }

    @Override
    public String toString() {
        return "continuous(" + set + ")";
    }
}
