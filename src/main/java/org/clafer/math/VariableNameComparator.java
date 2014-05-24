package org.clafer.math;

import java.util.Comparator;

/**
 *
 * @author jimmy
 */
public class VariableNameComparator implements Comparator<Variable> {

    public static final VariableNameComparator Singleton = new VariableNameComparator();

    @Override
    public int compare(Variable o1, Variable o2) {
        return o1.getName().compareTo(o2.getName());
    }
}
