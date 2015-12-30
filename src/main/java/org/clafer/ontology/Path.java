package org.clafer.ontology;

import java.util.Arrays;
import org.clafer.common.Check;

/**
 *
 * @author jimmy
 */
public class Path {

    private final Concept[] steps;

    public Path(Concept... steps) {
        this.steps = Check.noNullsNotEmpty(steps);
    }

    public Concept getContext() {
        return steps[0];
    }

    public Concept[] getSteps() {
        return steps;
    }

    public int length() {
        return steps.length;
    }

    public Path prepend(Concept step) {
        Concept[] newSteps = new Concept[steps.length + 1];
        newSteps[0] = step;
        System.arraycopy(steps, 0, newSteps, 1, steps.length);
        return new Path(newSteps);
    }

    public Path append(Concept step) {
        Concept[] newSteps = Arrays.copyOf(steps, steps.length + 1);
        newSteps[steps.length] = step;
        return new Path(newSteps);
    }

    public Path dropPrefix(int index) {
        if (index == 0) {
            return this;
        }
        return new Path(Arrays.copyOfRange(steps, index, steps.length));
    }

    @Override
    public boolean equals(Object obj) {
        if (obj instanceof Path) {
            Path other = (Path) obj;
            return Arrays.equals(steps, other.steps);
        }
        return false;
    }

    @Override
    public int hashCode() {
        return Arrays.hashCode(steps);
    }

    @Override
    public String toString() {
        StringBuilder result = new StringBuilder();
        result.append(steps[0]);
        for (int i = 1; i < steps.length; i++) {
            result.append('.').append(steps[i]);
        }
        return result.toString();
    }
}
