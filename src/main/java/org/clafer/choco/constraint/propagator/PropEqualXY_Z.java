package org.clafer.choco.constraint.propagator;

import org.chocosolver.solver.constraints.Propagator;
import org.chocosolver.solver.constraints.PropagatorPriority;
import org.chocosolver.solver.exception.ContradictionException;
import org.chocosolver.solver.variables.IntVar;
import org.chocosolver.solver.variables.events.IntEventType;
import org.chocosolver.util.ESat;

/**
 * {@code x + y = z}
 *
 * @author jimmy
 */
public class PropEqualXY_Z extends Propagator<IntVar> {

    private final IntVar x, y, z;

    @SuppressWarnings({"unchecked"})
    public PropEqualXY_Z(IntVar x, IntVar y, IntVar z) {
        super(new IntVar[]{x, y, z}, PropagatorPriority.BINARY, false);
        this.x = x;
        this.y = y;
        this.z = z;
    }

    @Override
    public int getPropagationConditions(int vIdx) {
        return IntEventType.all();
    }

    private boolean supportForX(int x) {
        int yub = y.getUB();
        // todo iterate over whichever var has smaller domain
        for (int i = y.getLB(); i <= yub; i = y.nextValue(i)) {
            if (z.contains(x + i)) {
                return true;
            }
        }
        return false;
    }

    private boolean supportForY(int y) {
        int xub = x.getUB();
        for (int i = x.getLB(); i <= xub; i = x.nextValue(i)) {
            if (z.contains(y + i)) {
                return true;
            }
        }
        return false;
    }

    private boolean supportForZ(int z) {
        int xub = x.getUB();
        for (int i = x.getLB(); i <= xub; i = x.nextValue(i)) {
            if (y.contains(z - i)) {
                return true;
            }
        }
        return false;
    }

    @Override
    public void propagate(int evtmask) throws ContradictionException {
        boolean changed;
        do {
            changed = false;
            x.updateLowerBound(z.getLB() - y.getUB(), aCause);
            x.updateUpperBound(z.getUB() - y.getLB(), aCause);
            changed |= y.updateLowerBound(z.getLB() - x.getUB(), aCause);
            changed |= y.updateUpperBound(z.getUB() - x.getLB(), aCause);
            changed |= z.updateLowerBound(x.getLB() + y.getLB(), aCause);
            changed |= z.updateUpperBound(x.getUB() + y.getUB(), aCause);
        } while (changed);
        do {
            changed = false;
            int xub = x.getUB();
            for (int i = x.getLB(); i <= xub; i = x.nextValue(i)) {
                if (!supportForX(i)) {
                    x.removeValue(i, aCause);
                }
            }
            int yub = y.getUB();
            for (int i = y.getLB(); i <= yub; i = y.nextValue(i)) {
                if (!supportForY(i)) {
                    changed |= y.removeValue(i, aCause);
                }
            }
            int zub = z.getUB();
            for (int i = z.getLB(); i <= zub; i = z.nextValue(i)) {
                if (!supportForZ(i)) {
                    changed |= z.removeValue(i, aCause);
                }
            }
        } while (changed);
    }

    @Override
    public ESat isEntailed() {
        int xub = x.getUB();
        for (int i = x.getLB(); i <= xub; i = x.nextValue(i)) {
            if (supportForX(i)) {
                return x.isInstantiated() && y.isInstantiated() && z.isInstantiated() ? ESat.TRUE : ESat.UNDEFINED;
            }
        }
        return ESat.FALSE;
    }
}
