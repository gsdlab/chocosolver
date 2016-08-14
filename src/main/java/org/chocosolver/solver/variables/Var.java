package org.chocosolver.solver.variables;

import java.util.Arrays;
import org.chocosolver.solver.Model;
import org.clafer.choco.constraint.Constraints;
import org.clafer.common.Util;

/**
 *
 * @author jimmy
 */
public class Var {

    public static int[] dom(int... doms) {
        return doms;
    }

    public static int[] env(int... envs) {
        return envs;
    }

    public static int[] ker(int... kers) {
        return kers;
    }

    public static int[] card(int... cards) {
        return cards;
    }

    public static int[] chars(int... chars) {
        return chars;
    }

    public static int[] insertIntoSortedArray(int[] as, int a) {
        int index = Arrays.binarySearch(as, a);
        if (index < 0) {
            int[] a2 = new int[as.length + 1];
            index = -index - 1;
            System.arraycopy(as, 0, a2, 0, index);
            System.arraycopy(as, index, a2, index + 1, as.length - index);
            a2[index] = a;
            return a2;
        }
        return as;
    }

    public static SetVar[] setArray(String name, int length, int minEnv, int maxEnv, Model model) {
        SetVar[] setVars = new SetVar[length];
        for (int i = 0; i < setVars.length; i++) {
            setVars[i] = model.setVar(name + i, Util.range(minEnv, maxEnv));
        }
        return setVars;
    }

    public static CStringVar fixed(String string, Model model) {
        IntVar[] chars = new IntVar[string.length()];
        for (int i = 0; i < string.length(); i++) {
            chars[i] = model.intVar(string.charAt(i));
        }
        return new CStringVar(chars, model.intVar(string.length()));
    }

    public static CStringVar cstring(String name, int[] charDomain, int maxLength, Model model) {
        int[] charsWithZero = insertIntoSortedArray(charDomain, 0);
        IntVar[] chars = new IntVar[maxLength];
        for (int i = 0; i < chars.length; i++) {
            chars[i] = model.intVar(name + "[" + i + "]", charsWithZero);
        }
        IntVar length = model.intVar("|" + name + "|", 0, maxLength);
        model.post(Constraints.length(chars, length));
        return new CStringVar(chars, length);
    }

    public static IntVar[] mapCard(SetVar... vars) {
        IntVar[] cards = new IntVar[vars.length];
        for (int i = 0; i < vars.length; i++) {
            cards[i] = vars[i].getCard();
        }
        return cards;
    }

    public static IntVar[] mapLength(CStringVar... vars) {
        IntVar[] lengths = new IntVar[vars.length];
        for (int i = 0; i < vars.length; i++) {
            lengths[i] = vars[i].getLength();
        }
        return lengths;
    }

    public static IntVar[][] mapChars(CStringVar... vars) {
        IntVar[][] chars = new IntVar[vars.length][];
        for (int i = 0; i < vars.length; i++) {
            chars[i] = vars[i].getChars();
        }
        return chars;
    }
}
