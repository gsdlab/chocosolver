package org.clafer;

import gnu.trove.list.TIntList;
import gnu.trove.list.array.TIntArrayList;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Random;
import org.clafer.choco.constraint.Constraints;
import org.clafer.collection.Pair;
import org.clafer.collection.Triple;
import static org.clafer.ir.IrBoolDomain.*;
import org.clafer.ir.IrBoolVar;
import org.clafer.ir.IrDomain;
import org.clafer.ir.IrIntVar;
import org.clafer.ir.IrSetVar;
import org.clafer.ir.IrStringVar;
import org.clafer.ir.IrUtil;
import static org.clafer.ir.Irs.*;
import solver.Solver;
import solver.constraints.Constraint;
import solver.constraints.Propagator;
import solver.constraints.set.SCF;
import solver.search.strategy.ISF;
import solver.search.strategy.SetStrategyFactory;
import solver.search.strategy.selectors.SetValueSelector;
import solver.search.strategy.strategy.AbstractStrategy;
import solver.search.strategy.strategy.SetSearchStrategy;
import solver.variables.BoolVar;
import solver.variables.IntVar;
import solver.variables.SetVar;
import solver.variables.VF;
import solver.variables.Variable;
import util.ESat;

/**
 *
 * @author jimmy
 */
public abstract class ClaferTest {

    protected final Random rand = new Random();
    protected int varCount = 0;

    public boolean nextBool() {
        return rand.nextBoolean();
    }

    public int nextInt(int n) {
        return rand.nextInt(n);
    }

    public int nextIntBetween(int min, int max) {
        return rand.nextInt(max - min + 1) + min;
    }

    public IrDomain randDomain(int low, int high) {
        switch (rand.nextInt(5)) {
            case 0:
                return constantDomain(nextIntBetween(low, high));
            case 1:
            case 2:
                int a = nextIntBetween(low, high);
                int b = nextIntBetween(low, high);
                return a < b ? boundDomain(a, b) : boundDomain(b, a);
            case 3:
            case 4:
                TIntList d;
                do {
                    d = new TIntArrayList();
                    for (int i = low; i <= high; i++) {
                        if (rand.nextBoolean()) {
                            d.add(i);
                        }
                    }
                } while (d.isEmpty());
                return enumDomain(d);
            default:
                throw new IllegalStateException();
        }
    }

    public IrBoolVar randBool() {
        switch (rand.nextInt(5)) {
            case 0:
                return False;
            case 1:
                return True;
            case 2:
            case 3:
            case 4:
                return bool("Bool" + varCount++);
            default:
                throw new IllegalStateException();
        }
    }

    public IrBoolVar[] randBools(int n) {
        IrBoolVar[] bools = new IrBoolVar[n];
        for (int i = 0; i < bools.length; i++) {
            bools[i] = randBool();
        }
        return bools;
    }

    public IrIntVar randInt(int low, int high) {
        return domainInt("Int" + varCount++, randDomain(low, high));
    }

    public IrIntVar randInt() {
        return randInt(-5, 5);
    }

    public IrIntVar randPositiveInt() {
        return randInt(0, 5);
    }

    public IrIntVar[] randInts(int n, int low, int high) {
        IrIntVar[] ints = new IrIntVar[n];
        for (int i = 0; i < ints.length; i++) {
            ints[i] = randInt(low, high);
        }
        return ints;
    }

    public IrIntVar[] randInts(int n) {
        return randInts(n, -5, 5);
    }

    public IrIntVar[] randPositiveInts(int n) {
        return randInts(n, 0, 5);
    }

    public IrSetVar randSet(int low, int high) {
        IrDomain env = randDomain(low, high);
        IrDomain ker = IrUtil.intersection(randDomain(low, high), env);
        int a = nextIntBetween(ker.size(), env.size());
        int b = nextIntBetween(ker.size(), env.size());
        IrDomain card = a < b ? boundDomain(a, b) : boundDomain(b, a);
        return set("Set" + varCount++, env, ker, card);
    }

    public IrSetVar randSet() {
        return randSet(-5, 5);
    }

    public IrSetVar randPositiveSet() {
        return randSet(0, 5);
    }

    public IrSetVar[] randSets(int n, int low, int high) {
        IrSetVar[] sets = new IrSetVar[n];
        for (int i = 0; i < sets.length; i++) {
            sets[i] = randSet(low, high);
        }
        return sets;
    }

    public IrSetVar[] randSets(int n) {
        return randSets(n, -5, 5);
    }

    public IrSetVar[] randPositiveSets(int n) {
        return randSets(n, 0, 5);
    }

    public IrStringVar randString() {
        String name = "String" + varCount++;
        IrIntVar length = randInt(0, 5);
        IrIntVar[] chars = new IrIntVar[length.getDomain().getHighBound()];
        for (int i = 0; i < chars.length; i++) {
            IrDomain domain = randDomain('a', 'c');
            chars[i] = domainInt(name + "[" + i + "]",
                    i < length.getDomain().getLowBound()
                    ? domain : IrUtil.add(domain, 0));
        }
        return string(name, chars, length);
    }

    public IntVar cardVar(SetVar set, int low, int high) {
        return VF.enumerated("|" + set.getName() + "|", low, high, set.getSolver());
    }

    public IntVar cardVar(SetVar set) {
        return cardVar(set, 0, set.getEnvelopeSize());
    }

    public IntVar[] cardVars(SetVar[] sets) {
        IntVar[] cards = new IntVar[sets.length];
        for (int i = 0; i < cards.length; i++) {
            cards[i] = cardVar(sets[i]);
        }
        return cards;
    }

    public IntVar enforcedCardVar(SetVar set, int low, int high) {
        IntVar card = cardVar(set, low, high);
        set.getSolver().post(SCF.cardinality(set, card));
        return card;
    }

    public IntVar enforcedCardVar(SetVar set) {
        return enforcedCardVar(set, 0, set.getEnvelopeSize());
    }

    public IntVar[] enforcedCardVars(SetVar[] sets) {
        IntVar[] cards = new IntVar[sets.length];
        for (int i = 0; i < cards.length; i++) {
            cards[i] = enforcedCardVar(sets[i]);
        }
        return cards;
    }

    public boolean[] getValues(BoolVar[] vars) {
        boolean[] values = new boolean[vars.length];
        for (int i = 0; i < values.length; i++) {
            values[i] = vars[i].getValue() == 1;
        }
        return values;
    }

    public int[] getValues(IntVar[] vars) {
        int[] values = new int[vars.length];
        for (int i = 0; i < values.length; i++) {
            values[i] = vars[i].getValue();
        }
        return values;
    }

    public int[][] getValues(SetVar[] vars) {
        int[][] values = new int[vars.length][];
        for (int i = 0; i < values.length; i++) {
            values[i] = vars[i].getValue();
        }
        return values;
    }

    public static BoolVar toVar(IrBoolVar var, Solver solver) {
        switch (var.getDomain()) {
            case FalseDomain:
                return VF.zero(solver);
            case TrueDomain:
                return VF.one(solver);
            case BoolDomain:
                return VF.bool(var.getName(), solver);
            default:
                throw new IllegalStateException();
        }
    }

    public static BoolVar[] toVars(IrBoolVar[] vars, Solver solver) {
        BoolVar[] bools = new BoolVar[vars.length];
        for (int i = 0; i < bools.length; i++) {
            bools[i] = ClaferTest.toVar(vars[i], solver);
        }
        return bools;
    }

    public static IntVar toVar(IrIntVar var, Solver solver) {
        return VF.enumerated(var.getName(), var.getDomain().getValues(), solver);
    }

    public static IntVar[] toVars(IrIntVar[] vars, Solver solver) {
        IntVar[] ints = new IntVar[vars.length];
        for (int i = 0; i < ints.length; i++) {
            ints[i] = ClaferTest.toVar(vars[i], solver);
        }
        return ints;
    }

    public static SetVar toSetVar(IrSetVar var, Solver solver) {
        return ClaferTest.toVar(var, solver).getSet();
    }

    public static SetVar[] toSetVars(IrSetVar[] vars, Solver solver) {
        SetVar[] sets = new SetVar[vars.length];
        for (int i = 0; i < sets.length; i++) {
            sets[i] = toSetVar(vars[i], solver);
        }
        return sets;
    }

    public static CSetVar toVar(IrSetVar var, Solver solver) {
        SetVar setVar = VF.set(var.getName(), var.getEnv().getValues(), var.getKer().getValues(), solver);
        IntVar cardVar = VF.enumerated("|" + var.getName() + "|", var.getCard().getValues(), solver);
        solver.post(SCF.cardinality(setVar, cardVar));
        return new CSetVar(setVar, cardVar);
    }

    public static CSetVar[] toVars(IrSetVar[] vars, Solver solver) {
        CSetVar[] sets = new CSetVar[vars.length];
        for (int i = 0; i < sets.length; i++) {
            sets[i] = ClaferTest.toVar(vars[i], solver);
        }
        return sets;
    }

    public static CStringVar toVar(IrStringVar var, Solver solver) {
        IntVar[] chars = ClaferTest.toVars(var.getCharVars(), solver);
        IntVar length = ClaferTest.toVar(var.getLengthVar(), solver);
        solver.post(Constraints.length(chars, length));
        return new CStringVar(chars, length);
    }

    public ESat isEntailed(Constraint constraint) {
        boolean undefined = false;
        for (Propagator<?> propagator : constraint.getPropagators()) {
            switch (propagator.isEntailed()) {
                case FALSE:
                    return ESat.FALSE;
                case UNDEFINED:
                    undefined = true;
            }
        }
        return undefined ? ESat.UNDEFINED : ESat.TRUE;
    }

    public Solver randomizeStrategy(Solver solver) {
        List<IntVar> intVars = new ArrayList<>();
        List<SetVar> setVars = new ArrayList<>();
        for (Variable var : solver.getVars()) {
            if (var instanceof IntVar) {
                intVars.add((IntVar) var);
            } else if (var instanceof SetVar) {
                setVars.add((SetVar) var);
            } else {
                throw new IllegalStateException();
            }
        }
        if (rand.nextBoolean()) {
            solver.set(
                    randomSearch(setVars.toArray(new SetVar[setVars.size()])),
                    randomSearch(intVars.toArray(new IntVar[intVars.size()])));
        } else {
            solver.set(
                    randomSearch(intVars.toArray(new IntVar[intVars.size()])),
                    randomSearch(setVars.toArray(new SetVar[setVars.size()])));
        }
        return solver;
    }

    public static class CSetVar {

        private final SetVar set;
        private final IntVar card;

        public CSetVar(SetVar set, IntVar card) {
            this.set = set;
            this.card = card;
        }

        public SetVar getSet() {
            return set;
        }

        public IntVar getCard() {
            return card;
        }

        @Override
        public String toString() {
            return "<" + set + ", " + card + ">";
        }
    }

    public SetVar[] mapSet(CSetVar... vars) {
        SetVar[] sets = new SetVar[vars.length];
        for (int i = 0; i < vars.length; i++) {
            sets[i] = vars[i].getSet();
        }
        return sets;
    }

    public IntVar[] mapCard(CSetVar... vars) {
        IntVar[] cards = new IntVar[vars.length];
        for (int i = 0; i < vars.length; i++) {
            cards[i] = vars[i].getCard();
        }
        return cards;
    }

    public static class CStringVar {

        private final IntVar[] chars;
        private final IntVar length;

        public CStringVar(IntVar[] chars, IntVar length) {
            this.chars = chars;
            this.length = length;
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
    }

    public IntVar[] mapLength(CStringVar... vars) {
        IntVar[] lengths = new IntVar[vars.length];
        for (int i = 0; i < vars.length; i++) {
            lengths[i] = vars[i].getLength();
        }
        return lengths;
    }

    public IntVar[][] mapChars(CStringVar... vars) {
        IntVar[][] chars = new IntVar[vars.length][];
        for (int i = 0; i < vars.length; i++) {
            chars[i] = vars[i].getChars();
        }
        return chars;
    }

    protected static <A, B> Pair<A, B> pair(A a, B b) {
        return new Pair<>(a, b);
    }

    protected static <A, B, C> Triple<A, B, C> triple(A a, B b, C c) {
        return new Triple<>(a, b, c);
    }

    private AbstractStrategy<IntVar> randomSearch(IntVar[] vars) {
        return ISF.random(vars, rand.nextLong());
    }

    private SetSearchStrategy randomSearch(SetVar[] vars) {
        return SetStrategyFactory.generic(vars,
                new solver.search.strategy.selectors.variables.Random<SetVar>(rand.nextLong()),
                new RandomSetValueSelector(), rand.nextBoolean());
    }

    private class RandomSetValueSelector implements SetValueSelector {

        @Override
        public int selectValue(SetVar s) {
            int m = rand.nextInt(s.getEnvelopeSize() - s.getKernelSize());
            for (int i = s.getEnvelopeFirst(); i != SetVar.END; i = s.getEnvelopeNext()) {
                if (!s.kernelContains(i)) {
                    if (m == 0) {
                        return i;
                    }
                    m--;
                }
            }
            throw new IllegalStateException();
        }
    }
}
