package org.clafer.choco.constraint;

import java.util.Arrays;
import org.chocosolver.solver.Model;
import org.chocosolver.solver.constraints.Constraint;
import org.chocosolver.solver.constraints.Propagator;
import org.chocosolver.solver.constraints.binary.PropEqualXY_C;
import org.chocosolver.solver.constraints.binary.PropEqualX_Y;
import org.chocosolver.solver.constraints.binary.PropEqualX_YC;
import org.chocosolver.solver.constraints.binary.PropGreaterOrEqualX_Y;
import org.chocosolver.solver.constraints.binary.PropNotEqualX_Y;
import org.chocosolver.solver.constraints.set.PropSubsetEq;
import org.chocosolver.solver.constraints.unary.PropEqualXC;
import org.chocosolver.solver.constraints.unary.PropGreaterOrEqualXC;
import org.chocosolver.solver.constraints.unary.PropLessOrEqualXC;
import org.chocosolver.solver.constraints.unary.PropNotEqualXC;
import org.chocosolver.solver.variables.IntVar;
import org.chocosolver.solver.variables.SetVar;
import org.clafer.choco.constraint.propagator.PropContinuous;
import org.clafer.choco.constraint.propagator.PropElement;
import org.clafer.choco.constraint.propagator.PropElementArraySupport;
import org.clafer.choco.constraint.propagator.PropElementValueSupport;
import org.clafer.choco.constraint.propagator.PropEqualXY_Z;
import org.clafer.choco.constraint.propagator.PropIntMemberNonemptySet;
import org.clafer.choco.constraint.propagator.PropLength;
import org.clafer.choco.constraint.propagator.PropSetLowBound;
import org.clafer.choco.constraint.propagator.PropSetStrictHighBound;
import org.clafer.choco.constraint.propagator.PropUtil;

/**
 *
 * @author jimmy
 */
public class Propagators {

    final Propagator<?>[] propagators;
    int size = 0;

    public Propagators(int capacity) {
        this.propagators = new Propagator<?>[capacity];
    }

    public Propagators post(Propagator<?> propagator) {
        propagators[size++] = propagator;
        return this;
    }

    public Propagators eq(IntVar l, IntVar r) {
        if (l.isInstantiated()) {
            return eq(r, l.getValue());
        }
        if (r.isInstantiated()) {
            return eq(l, r.getValue());
        }
        return post(new PropEqualX_Y(l, r));
    }

    public Propagators eq(IntVar l, int r) {
        if (!l.isInstantiatedTo(r)) {
            return post(new PropEqualXC(l, r));
        }
        return this;
    }

    public Propagators neq(IntVar l, IntVar r) {
        if (l.isInstantiated()) {
            return neq(r, l.getValue());
        }
        if (r.isInstantiated()) {
            return neq(l, r.getValue());
        }
        return post(new PropNotEqualX_Y(l, r));
    }

    public Propagators neq(IntVar l, int r) {
        if (!l.isInstantiated() || l.getValue() == r) {
            return post(new PropNotEqualXC(l, r));
        }
        return this;
    }

    public Propagators leq(IntVar l, IntVar g) {
        if (l.getUB() <= g.getLB()) {
            return this;
        }
        if (l.isInstantiated()) {
            return post(new PropGreaterOrEqualXC(g, l.getValue()));
        }
        if (g.isInstantiated()) {
            return post(new PropLessOrEqualXC(l, g.getValue()));
        }
        return post(new PropGreaterOrEqualX_Y(new IntVar[]{g, l}));
    }

    public Propagators geq(IntVar g, IntVar l) {
        return leq(l, g);
    }

    public Propagators add(int x, IntVar y, IntVar z) {
        if (x == 0) {
            return eq(y, z);
        } else if (y.isInstantiated()) {
            return eq(z, x + y.getValue());
        } else if (z.isInstantiated()) {
            return eq(y, z.getValue() - x);
        } else {
            return post(new PropEqualX_YC(new IntVar[]{y, z}, -x));
        }
    }

    public Propagators add(IntVar x, IntVar y, int z) {
        if (x.isInstantiated()) {
            return eq(y, z - x.getValue());
        } else if (y.isInstantiated()) {
            return eq(x, z - y.getValue());
        } else {
            return post(new PropEqualXY_C(new IntVar[]{x, y}, z));
        }
    }

    public Propagators add(IntVar x, IntVar y, IntVar z) {
        if (x.isInstantiated()) {
            return add(x.getValue(), y, z);
        } else if (y.isInstantiated()) {
            return add(y.getValue(), x, z);
        } else if (z.isInstantiated()) {
            return add(x, y, z.getValue());
        } else {
            return post(new PropEqualXY_Z(x, y, z));
        }
    }

    public Propagators element(IntVar value, IntVar[] array, IntVar index, int offset) {
        if (value.isInstantiated()) {
            int v = value.getValue();
            int ub = index.getUB();
            for (int i = index.getLB(); i <= ub; i = index.nextValue(i)) {
                int j = i + offset;
                if (j < 0 || j >= array.length || !array[j].isInstantiatedTo(v)) {
                    return post(new PropElement(value, array, index, offset));
                }
            }
            return this;
        }
        return post(new PropElement(value, array, index, offset));
    }

    public Propagators elementArraySupport(IntVar value, IntVar[] array, IntVar index, int offset, int support) {
        if (index.getUB() < array.length) {
            int ub = index.getUB();
            for (int i = index.getLB(); i <= ub; i = index.nextValue(i)) {
                int j = i + offset;
                if (j >= 0 && j < array.length && (array[j].contains(support) || PropUtil.isDomIntersectDom(value, array[j]))) {
                    if (index.isInstantiated() && value.isInstantiated() && array[j].isInstantiated()) {
                        return this;
                    }
                }
            }
        }
        return post(new PropElementArraySupport(value, array, index, offset, support));
    }

    public Propagators elementValueSupport(IntVar value, IntVar[] array, IntVar index, int offset, int support) {
        if (!value.contains(support)) {
            return element(value, array, index, offset);
        } else if (value.isInstantiated() && index.getUB() < array.length) {
            return this;
        }
        return post(new PropElementValueSupport(value, array, index, offset, support));
    }

    public Propagators length(IntVar[] chars, IntVar length, int terminator) {
        if (length.isInstantiated()) {
            int l = length.getValue();
            if (l < chars.length) {
                for (int i = 0; i < l; i++) {
                    if (chars[i].contains(terminator)) {
                        return post(new PropLength(chars, length, terminator));
                    }
                }
                for (int i = l; i < chars.length; i++) {
                    if (!chars[i].isInstantiatedTo(terminator)) {
                        return post(new PropLength(chars, length, terminator));
                    }
                }
                return this;
            }
        }
        return post(new PropLength(chars, length, terminator));
    }

    public Propagators subsetEq(SetVar subset, SetVar superSet) {
        if (PropUtil.isEnvSubsetKer(subset, superSet)) {
            return this;
        }
        return post(new PropSubsetEq(subset, superSet));
    }

    public Propagators lowBound(SetVar set, IntVar bound) {
        if (set.getUB().isEmpty() || bound.getUB() <= set.getUB().min()) {
            return this;
        }
        return post(new PropSetLowBound(set, bound));
    }

    public Propagators strictHighBound(SetVar set, IntVar bound) {
        if (set.getUB().isEmpty() || bound.getLB() > set.getUB().max()) {
            return this;
        }
        return post(new PropSetStrictHighBound(set, bound));
    }

    public Propagators memberNonempty(IntVar element, SetVar set, IntVar setCard) {
        if (setCard.getUB() == 0 || PropUtil.isDomSubsetKer(element, set)
                || (set.getUB().size() == 1 && element.isInstantiatedTo(set.getUB().min()))) {
            return this;
        }
        return post(new PropIntMemberNonemptySet(element, set, setCard));
    }

    public Propagators continuous(SetVar set, IntVar card) {
        if (set.isInstantiated()
                && (set.getUB().isEmpty()
                || set.getLB().max() - set.getLB().min() + 1 == set.getUB().size())) {
            return this;
        }
        if (card.getUB() == 1) {
            return this;
        }
        return post(new PropContinuous(set, card));
    }

    public Constraint toConstraint(String name, Model model) {
        if (size == 0) {
            return model.trueConstraint();
        }
        if (size == propagators.length) {
            return new Constraint(name, propagators);
        }
        return new Constraint(name, Arrays.copyOf(propagators, size));
    }
}
