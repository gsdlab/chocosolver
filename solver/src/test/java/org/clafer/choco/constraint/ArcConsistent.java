package org.clafer.choco.constraint;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * Denotes a constraint that is arc-consistent.
 *
 * @author jimmy
 */
@Retention(RetentionPolicy.RUNTIME)
@Target(ElementType.METHOD)
public @interface ArcConsistent {

    /**
     * @return {@code} true if isEntailed will return UNDEFINED if there exists
     * a satisfying and unsatisfying complete solution, {@code false} otherwise
     */
    boolean entailed() default false;

    /**
     * @return {@code true} if the opposite constraint is also arc-consistent,
     * {@code false} otherwise
     */
    boolean opposite() default false;
}
