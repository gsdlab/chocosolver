package org.clafer;

import gnu.trove.list.TIntList;
import gnu.trove.list.array.TIntArrayList;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Random;
import org.clafer.choco.constraint.Constraints;
import org.clafer.choco.constraint.RandomSetSearchStrategy;
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
import solver.search.strategy.IntStrategyFactory;
import solver.search.strategy.strategy.StrategiesSequencer;
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
    private int varCount = 0;

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
        return string(name,
                boundInt("|" + name + "|", 0, 5),
                randInts(5, 'a', 'c'));
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

    public static BoolVar toBoolVar(IrBoolVar var, Solver solver) {
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

    public static BoolVar[] toBoolVars(IrBoolVar[] vars, Solver solver) {
        BoolVar[] bools = new BoolVar[vars.length];
        for (int i = 0; i < bools.length; i++) {
            bools[i] = toBoolVar(vars[i], solver);
        }
        return bools;
    }

    public static IntVar toIntVar(IrIntVar var, Solver solver) {
        return VF.enumerated(var.getName(), var.getDomain().getValues(), solver);
    }

    public static IntVar[] toIntVars(IrIntVar[] vars, Solver solver) {
        IntVar[] ints = new IntVar[vars.length];
        for (int i = 0; i < ints.length; i++) {
            ints[i] = toIntVar(vars[i], solver);
        }
        return ints;
    }

    public static SetVar toSetVar(IrSetVar var, Solver solver) {
        return toCSetVar(var, solver).getSet();
    }

    public static SetVar[] toSetVars(IrSetVar[] vars, Solver solver) {
        SetVar[] sets = new SetVar[vars.length];
        for (int i = 0; i < sets.length; i++) {
            sets[i] = toSetVar(vars[i], solver);
        }
        return sets;
    }

    public static CSetVar toCSetVar(IrSetVar var, Solver solver) {
        SetVar setVar = VF.set(var.getName(), var.getEnv().getValues(), var.getKer().getValues(), solver);
        IntVar cardVar = VF.enumerated("|" + var.getName() + "|", var.getCard().getValues(), solver);
        solver.post(SCF.cardinality(setVar, cardVar));
        return new CSetVar(setVar, cardVar);
    }

    public static CSetVar[] toCSetVars(IrSetVar[] vars, Solver solver) {
        CSetVar[] sets = new CSetVar[vars.length];
        for (int i = 0; i < sets.length; i++) {
            sets[i] = toCSetVar(vars[i], solver);
        }
        return sets;
    }

    public static CStringVar toCStringVar(IrStringVar var, Solver solver) {
        IntVar length = toIntVar(var.getLength(), solver);
        IntVar[] chars = toIntVars(var.getChars(), solver);
        solver.post(Constraints.length(length, chars));
        return new CStringVar(length, chars);
    }

    public ESat isEntailed(Constraint constraint) {
        boolean undefined = false;
        for (Propagator propagator : constraint.getPropagators()) {
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
                    new StrategiesSequencer(solver.getEnvironment(),
                            new RandomSetSearchStrategy(setVars.toArray(new SetVar[setVars.size()])),
                            IntStrategyFactory.random(intVars.toArray(new IntVar[intVars.size()]), System.nanoTime())));
        } else {
            solver.set(
                    new StrategiesSequencer(solver.getEnvironment(),
                            IntStrategyFactory.random(intVars.toArray(new IntVar[intVars.size()]), System.nanoTime()),
                            new RandomSetSearchStrategy(setVars.toArray(new SetVar[setVars.size()]))));
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

        private final IntVar length;
        private final IntVar[] chars;

        public CStringVar(IntVar length, IntVar[] chars) {
            this.length = length;
            this.chars = chars;
        }

        public IntVar getLength() {
            return length;
        }

        public IntVar[] getChars() {
            return chars;
        }

        @Override
        public String toString() {
            return "<" + length + ", " + Arrays.toString(chars) + ">";
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
}
