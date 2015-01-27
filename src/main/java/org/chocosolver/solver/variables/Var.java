package org.chocosolver.solver.variables;

import java.util.Arrays;
import org.clafer.choco.constraint.Constraints;
import org.clafer.common.Util;
import org.chocosolver.solver.Solver;
import org.chocosolver.solver.variables.IntVar;
import org.chocosolver.solver.variables.SetVar;
import org.chocosolver.solver.variables.VariableFactory;

/**
 *
 * @author jimmy
 */
public class Var extends VariableFactory {

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

    public static CSetVar constant(int[] value, Solver solver) {
        return new CSetVar(VF.fixed(Arrays.toString(value), value, solver), VF.fixed(value.length, solver));
    }

    public static CSetVar cset(String name, int[] env, Solver solver) {
        SetVar setVar = set(name, env, solver);
        IntVar cardVar = enumerated("|" + name + "|", 0, setVar.getEnvelopeSize(), solver);
        return new CSetVar(setVar, cardVar);
    }

    public static CSetVar cset(String name, int minEnv, int maxEnv, Solver solver) {
        SetVar setVar = set(name, minEnv, maxEnv, solver);
        IntVar cardVar = enumerated("|" + name + "|", 0, setVar.getEnvelopeSize(), solver);
        return new CSetVar(setVar, cardVar);
    }

    public static CSetVar cset(String name, int[] env, int[] ker, Solver solver) {
        return cset(name, env, ker, Util.range(ker.length, env.length), solver);
    }

    public static CSetVar cset(String name, int[] env, int[] ker, int[] card, Solver solver) {
        SetVar setVar = set(name, env, ker, solver);
        IntVar cardVar = enumerated("|" + name + "|", card, solver);
        return new CSetVar(setVar, cardVar);
    }

    public static CStringVar fixed(String string, Solver solver) {
        IntVar[] chars = new IntVar[string.length()];
        for (int i = 0; i < string.length(); i++) {
            chars[i] = fixed(string.charAt(i), solver);
        }
        return new CStringVar(chars, fixed(string.length(), solver));
    }

    public static CStringVar cstring(String name, int[] charDomain, int maxLength, Solver solver) {
        int[] charsWithZero = insertIntoSortedArray(charDomain, 0);
        IntVar[] chars = new IntVar[maxLength];
        for (int i = 0; i < chars.length; i++) {
            chars[i] = enumerated(name + "[" + i + "]", charsWithZero, solver);
        }
        IntVar length = enumerated("|" + name + "|", 0, maxLength, solver);
        solver.post(Constraints.length(chars, length));
        return new CStringVar(chars, length);
    }

    public static SetVar[] mapSet(CSetVar... vars) {
        SetVar[] sets = new SetVar[vars.length];
        for (int i = 0; i < vars.length; i++) {
            sets[i] = vars[i].getSet();
        }
        return sets;
    }

    public static IntVar[] mapCard(CSetVar... vars) {
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
