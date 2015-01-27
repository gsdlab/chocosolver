package org.chocosolver.solver.variables;

import java.util.Arrays;
import org.clafer.choco.constraint.Constraints;
import org.chocosolver.solver.variables.IntVar;
import org.chocosolver.solver.variables.SetVar;

/**
 *
 * @author jimmy
 */
public class CStringVar {

    private final IntVar[] chars;
    private final IntVar length;

    public CStringVar(IntVar[] chars, IntVar length) {
        this.chars = chars;
        this.length = length;

        if (!isLengthEntailed(chars, length)) {
            length.getSolver().post(Constraints.length(chars, length));
        }
    }

    public IntVar[] getChars() {
        return chars;
    }

    public IntVar getLength() {
        return length;
    }

    @Override
    public String toString() {
        return "<" + Arrays.toString(chars) + ", " + length + ">";
    }

    private static boolean isLengthEntailed(IntVar[] chars, IntVar length) {
        if (!length.isInstantiated()) {
            return false;
        }
        int l = length.getValue();
        for (int i = 0; i < l; i++) {
            if (chars[i].contains(0)) {
                return false;
            }
        }
        for (int i = l; i < chars.length; i++) {
            if (!chars[i].isInstantiatedTo(0)) {
                return false;
            }
        }
        return l <= chars.length;
    }
}
