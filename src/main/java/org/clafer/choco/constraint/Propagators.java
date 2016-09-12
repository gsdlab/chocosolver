package org.clafer.choco.constraint;

import java.util.Arrays;
import java.util.Optional;
import org.chocosolver.solver.Model;
import org.chocosolver.solver.constraints.Constraint;
import org.chocosolver.solver.constraints.Propagator;

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

    public Propagators add(Propagator<?> propagator) {
        propagators[size++] = propagator;
        return this;
    }

    public Propagators add(Optional<? extends Propagator<?>> propagator) {
        propagator.ifPresent(this::add);
        return this;
    }

    public Constraint toConstraint(String name, Model model) {
        return size == 0
                ? model.trueConstraint()
                : new Constraint(name, toArray());
    }

    public Propagator<?>[] toArray() {
        if (size == propagators.length) {
            return propagators;
        }
        return Arrays.copyOf(propagators, size);
    }
}
