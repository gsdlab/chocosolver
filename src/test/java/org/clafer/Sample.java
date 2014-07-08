package org.clafer;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 *
 * @author jimmy
 */
@Retention(RetentionPolicy.RUNTIME)
@Target(ElementType.METHOD)
public @interface Sample {

    public static final int Default = 10;

    /**
     * @return the number of times to repeat the experiment
     */
    int value() default Default;
}
