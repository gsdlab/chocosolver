package org.clafer.constraint;

import choco.Choco;
import choco.kernel.model.constraints.Constraint;
import choco.kernel.model.variables.integer.IntegerVariable;

/**
 *
 * @author jimmy
 */
public class BoolSlideManager {

    /**
     * base[i] <=> slide[i + offset]
     * 
     * @param base
     * @param slide
     * @param offset
     * @return 
     */
    public static Constraint boolSlide(IntegerVariable[] base, IntegerVariable[] slide, int offset) {
        if (base.length > slide.length) {
            throw new IllegalArgumentException();
        }

        if (Math.abs(offset) > slide.length - base.length) {
            return Choco.FALSE;
        }
        Constraint[] constraints = new Constraint[base.length];
        for (int i = 0; i < constraints.length; i++) {
            constraints[i] = Choco.eq(base[i], slide[i + offset]);
        }
        return Choco.and(constraints);
    }
}