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
import org.chocosolver.util.objects.setDataStructures.ISetIterator;

/**
 *
 * @author jimmy
 */
public class PropContinuous extends Propagator<Variable> {

    private static final long serialVersionUID = 1L;

    private final SetVar set;
    private final IntVar card;

    public PropContinuous(SetVar set, IntVar card) {
        super(new Variable[]{set}, PropagatorPriority.QUADRATIC, false);
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
    public int getPropagationConditions(int vIdx) {
        if (isSetVar(vIdx)) {
            return SetEventType.all();
        }
        assert isCardVar(vIdx);
        return IntEventType.VOID.getMask();
    }

    @Override
    public void propagate(int evtmask) throws ContradictionException {
        if (set.getLB().size() > 0) {
            ISetIterator setKer = set.getLB().iterator();
            assert setKer.hasNext();
            int cur = setKer.nextInt();
            while (setKer.hasNext()) {
                int next = setKer.nextInt();
                for (int j = cur + 1; j < next; j++) {
                    set.force(j, this);
                }
                cur = next;
            }
        }
        if (set.getUB().size() > 0) {
            int cardLb = card.getLB();
            if (cardLb >= 2) {
                int ker = set.getLB().isEmpty() ? 0 : set.getLB().min();
                int maxSize = 0;

                ISetIterator iter = set.getUB().iterator();
                if (iter.hasNext()) {
                    int prev = iter.nextInt();
                    int size = 1;
                    while (iter.hasNext()) {
                        assert (!set.getUB().contains(prev - 1));

                        int next = iter.nextInt();

                        while (next == prev + 1) {
                            size++;
                            if (!iter.hasNext()) {
                                break;
                            }
                            prev = next;
                            next = iter.nextInt();
                        }

                        if (next != prev + 1
                                && (!set.getLB().isEmpty() && ker <= prev - size && ker > prev)
                                || size < cardLb) {
                            for (int i = prev; i > prev - size; i--) {
                                set.remove(i, this);
                            }
                        } else if (size > maxSize) {
                            maxSize = size;
                        }
                        prev = next;
                        size = 1;
                    }
                }
                card.updateUpperBound(maxSize, this);
            }
        }
    }

    @Override
    public ESat isEntailed() {
        if (set.getLB().size() > 0) {
            int min = set.getLB().min();
            int max = set.getLB().max();
            for (int i = min + 1; i < max; i++) {
                if (!set.getUB().contains(i)) {
                    return ESat.FALSE;
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
