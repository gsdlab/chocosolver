package org.clafer.constraint;

import choco.cp.solver.variables.integer.IntVarEvent;
import choco.kernel.common.util.iterators.DisposableIntIterator;
import choco.kernel.solver.ContradictionException;
import choco.kernel.solver.constraints.integer.AbstractLargeIntSConstraint;
import choco.kernel.solver.variables.integer.IntDomainVar;
import org.clafer.Util;

/**
 * for all a, base[i] = a => slide[i + offset] = a
 * 
 * @author jimmy
 */
public class BoolSlide extends AbstractLargeIntSConstraint {

    private final IntDomainVar[] base;
    private final IntDomainVar[] slide;
    private final IntDomainVar offset;

    public BoolSlide(IntDomainVar[] base, IntDomainVar[] slide, IntDomainVar offset) {
        super(Util.cons(offset, Util.combine(base, slide)));
        if (base.length > slide.length) {
            throw new IllegalArgumentException();
        }
        this.base = base;
        this.slide = slide;
        this.offset = offset;
    }

    @Override
    public int getFilteredEventMask(int idx) {
        return IntVarEvent.INSTINT_MASK;
    }

    private boolean isOffsetVar(int varIdx) {
        return varIdx == 0;
    }

    private boolean isBaseVar(int varIdx) {
        return varIdx >= 1 && varIdx <= base.length;
    }

    private boolean isSlideVar(int varIdx) {
        return varIdx > base.length;
    }

    private int getBaseVarIndex(int varIdx) {
        return varIdx - 1;
    }

    private int getSlideVarIndex(int varIdx) {
        return varIdx - base.length - 1;
    }

    @Override
    public void awakeOnInst(int idx) throws ContradictionException {
        if (isOffsetVar(idx)) {
            pickBaseAndSlide();
        } else if (isBaseVar(idx)) {
            int id = getBaseVarIndex(idx);
            int val = base[id].getVal();
            if (offset.isInstantiated()) {
                // Pick slide
                slide[id + offset.getVal()].instantiate(val, this, false);
            } else {
                // Prune offset
                DisposableIntIterator it = offset.getDomain().getIterator();
                try {
                    while (it.hasNext()) {
                        int off = it.next();
                        if (slide[id + off].isInstantiated()) {
                            if (val != slide[id + off].getVal()) {
                                offset.removeVal(off, this, false);
                            }
                        }
                    }
                } finally {
                    it.dispose();
                }
            }
        } else {
            int id = getSlideVarIndex(idx);
            int val = slide[id].getVal();
            if (offset.isInstantiated()) {
                // Pick base
                int off = offset.getVal();
                int baseId = id - off;
                if (baseId >= 0 && baseId < base.length) {
                    base[baseId].instantiate(val, this, false);
                }
            } else {
                // Prune offset
                DisposableIntIterator it = offset.getDomain().getIterator();
                try {
                    while (it.hasNext()) {
                        int off = it.next();
                        int baseId = id - off;
                        if (baseId >= 0 && baseId < base.length && base[baseId].isInstantiated()) {
                            if (val != base[baseId].getVal()) {
                                offset.removeVal(off, this, false);
                            }
                        }
                    }
                } finally {
                    it.dispose();
                }
            }
        }
    }

    @Override
    public void awake() throws ContradictionException {
        offset.removeInterval(Integer.MIN_VALUE, -1, this, false);
        offset.removeInterval(slide.length - base.length + 1, Integer.MAX_VALUE, this, false);
        propagate();
    }

    @Override
    public void propagate() throws ContradictionException {
        if (offset.isInstantiated()) {
            pickBaseAndSlide();
        } else {
            pruneOffset();
        }
    }

    private void pickBaseAndSlide() throws ContradictionException {
        assert offset.isInstantiated();

        // Pick base and slide
        int off = offset.getVal();
        for (int i = 0; i < base.length; i++) {
            if (base[i].isInstantiated()) {
                slide[i + off].instantiate(base[i].getVal(), this, false);
            } else if (slide[i + off].isInstantiated()) {
                base[i].instantiate(slide[i + off].getVal(), this, false);
            }
        }
    }

    private void pruneOffset() throws ContradictionException {
        assert !offset.isInstantiated();

        DisposableIntIterator it = offset.getDomain().getIterator();
        try {
            while (it.hasNext()) {
                int off = it.next();
                for (int i = 0; i < base.length; i++) {
                    if (base[i].isInstantiated() && slide[i + off].isInstantiated()) {
                        if (base[i].getVal() != slide[i + off].getVal()) {
                            offset.removeVal(off, this, false);
                            break;
                        }
                    }
                }
            }
        } finally {
            it.dispose();
        }
    }

    @Override
    public boolean isSatisfied() {
        if (offset.getVal() > slide.length - base.length) {
            return false;
        }
        for (int i = 0; i < base.length; i++) {
            if (base[i].getVal() != slide[i + offset.getVal()].getVal()) {
                return false;
            }
        }
        return true;
    }

    @Override
    public boolean isSatisfied(int[] tuple) {
        int $offset = tuple[0];
        for (int i = 0; i < base.length; i++) {
            if (tuple[i + 1] != tuple[i + 1 + base.length + $offset]) {
                return false;
            }
        }
        return true;
    }
}
