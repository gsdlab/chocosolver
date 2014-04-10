package org.clafer.test;

import gnu.trove.list.TIntList;
import gnu.trove.list.array.TIntArrayList;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Random;
import org.clafer.choco.constraint.Constraints;
import org.clafer.ir.IrBoolVar;
import org.clafer.ir.IrDomain;
import org.clafer.ir.IrIntVar;
import org.clafer.ir.IrSetVar;
import org.clafer.ir.IrStringVar;
import org.clafer.ir.IrUtil;
import static org.clafer.ir.Irs.*;
import solver.Solver;
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

/**
 *
 * @author jimmy
 */
public class TestUtil {

    private static final Random rand = new Random();
    private static int varCount = 0;

    public static Solver randomizeStrategy(Solver solver) {
        List<IntVar> intVars = new ArrayList<>();
        List<SetVar> setVars = new ArrayList<>();
        for (Variable var : solver.getVars()) {
            if (!var.isInstantiated()
                    && (var.getTypeAndKind() & Variable.VIEW) == 0
                    && !(var.getName().startsWith("TMP_"))) {
                if (var instanceof IntVar) {
                    intVars.add((IntVar) var);
                } else if (var instanceof SetVar) {
                    setVars.add((SetVar) var);
                } else {
                    throw new IllegalStateException();
                }
            }
        }
        if (randBool()) {
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

    private static AbstractStrategy<IntVar> randomSearch(IntVar[] vars) {
        return ISF.random_value(vars, rand.nextLong());
    }

    private static SetSearchStrategy randomSearch(SetVar[] vars) {
        return SetStrategyFactory.generic(
                new solver.search.strategy.selectors.variables.Random<SetVar>(rand.nextLong()),
                new RandomSetValueSelector(), randBool(), vars);
    }

    private static class RandomSetValueSelector implements SetValueSelector {

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

    public static boolean randBool() {
        return rand.nextBoolean();
    }

    public static int randInt(int low, int high) {
        if (low > high) {
            throw new IllegalArgumentException();
        }
        return rand.nextInt(high - low + 1) + low;
    }

    public static int randInt() {
        return randInt(-4, 4);
    }

    public static int randPositiveInt() {
        return randInt(0, 4);
    }

    public static IrDomain randDomain(int low, int high) {
        if (low > high) {
            throw new IllegalArgumentException();
        }
        return rand.nextInt(4) == 0
                ? EmptyDomain
                : randNonEmptyDomain(low, high);
    }

    public static IrDomain randDomain() {
        return randDomain(-4, 4);
    }

    public static IrDomain randPositiveDomain() {
        return randDomain(0, 4);
    }

    public static IrDomain randNonEmptyDomain(int low, int high) {
        if (low > high) {
            throw new IllegalArgumentException();
        }
        switch (randInt(0, 4)) {
            case 0:
                return constantDomain(randInt(low, high));
            case 1:
            case 2:
                int a = randInt(low, high);
                int b = randInt(low, high);
                return a < b ? boundDomain(a, b) : boundDomain(b, a);
            case 3:
            case 4:
                TIntList d;
                do {
                    d = new TIntArrayList();
                    for (int i = low; i <= high; i++) {
                        if (randBool()) {
                            d.add(i);
                        }
                    }
                } while (d.isEmpty());
                return enumDomain(d);
            default:
                throw new IllegalStateException();
        }
    }

    public static IrDomain randNonEmptyDomain() {
        return randNonEmptyDomain(-4, 4);
    }

    public static IrDomain randNonEmptyPositiveDomain() {
        return randNonEmptyDomain(0, 4);
    }

    public static IrBoolVar randIrBoolVar() {
        switch (randInt(0, 4)) {
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

    public static IrIntVar randIrIntVar(int low, int high) {
        if (low > high) {
            throw new IllegalArgumentException();
        }
        return domainInt("Int" + varCount++, randNonEmptyDomain(low, high));
    }

    public static IrIntVar randIrIntVar() {
        return randIrIntVar(-4, 4);
    }

    public static IrIntVar randPositiveIrIntVar() {
        return randIrIntVar(0, 4);
    }

    public static IrSetVar randIrSetVar(int low, int high) {
        if (low > high) {
            throw new IllegalArgumentException();
        }
        IrDomain env = randDomain(low, high);
        IrDomain ker = IrUtil.intersection(randDomain(low, high), env);
        int a = randInt(ker.size(), env.size());
        int b = randInt(ker.size(), env.size());
        IrDomain card = a < b ? randNonEmptyDomain(a, b) : randNonEmptyDomain(b, a);
        return set("Set" + varCount++, env, ker, card);
    }

    public static IrSetVar randIrSetVar() {
        return TestUtil.randIrSetVar(-4, 4);
    }

    public static IrSetVar randPositiveIrSetVar() {
        return TestUtil.randIrSetVar(0, 4);
    }

    public static IrStringVar randIrStringVar() {
        String name = "String" + varCount++;
        IrIntVar length = randIrIntVar(0, 4);
        IrIntVar[] chars = new IrIntVar[length.getDomain().getHighBound()];
        for (int i = 0; i < chars.length; i++) {
            IrDomain domain = randNonEmptyDomain('a', 'c');
            chars[i] = domainInt(name + "[" + i + "]",
                    i < length.getDomain().getLowBound()
                    ? domain : IrUtil.add(domain, 0));
        }
        return string(name, chars, length);
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

    public static IntVar toVar(IrIntVar var, Solver solver) {
        IrDomain domain = var.getDomain();
        return domain.isBounded()
                ? VF.enumerated(var.getName(), domain.getLowBound(), domain.getHighBound(), solver)
                : VF.enumerated(var.getName(), domain.getValues(), solver);
    }

    public static CSetVar toVar(IrSetVar var, Solver solver) {
        SetVar setVar = VF.set(var.getName(), var.getEnv().getValues(), var.getKer().getValues(), solver);
        IntVar cardVar = VF.enumerated("|" + var.getName() + "|", var.getCard().getValues(), solver);
        solver.post(SCF.cardinality(setVar, cardVar));
        return new CSetVar(setVar, cardVar);
    }

    public static CStringVar toVar(IrStringVar var, Solver solver) {
        IntVar[] chars = new IntVar[var.getCharVars().length];
        for (int i = 0; i < chars.length; i++) {
            chars[i] = toVar(var.getCharVars()[i], solver);
        }
        IntVar length = toVar(var.getLengthVar(), solver);
        solver.post(Constraints.length(chars, length));
        return new CStringVar(chars, length);
    }

    public static BoolVar randBoolVar(Solver solver) {
        return toVar(randIrBoolVar(), solver);
    }

    public static IntVar randIntVar(int low, int high, Solver solver) {
        if (low > high) {
            throw new IllegalArgumentException();
        }
        return toVar(randIrIntVar(low, high), solver);
    }

    public static IntVar randIntVar(Solver solver) {
        return toVar(randIrIntVar(), solver);
    }

    public static IntVar randPositiveIntVar(Solver solver) {
        return toVar(randPositiveIrIntVar(), solver);
    }

    public static CSetVar randSetVar(int low, int high, Solver solver) {
        if (low > high) {
            throw new IllegalArgumentException();
        }
        return toVar(randIrSetVar(low, high), solver);
    }

    public static CSetVar randSetVar(Solver solver) {
        return toVar(randIrSetVar(), solver);
    }

    public static CSetVar randPositiveSetVar(Solver solver) {
        return toVar(randPositiveIrSetVar(), solver);
    }

    public static CStringVar randStringVar(Solver solver) {
        return toVar(randIrStringVar(), solver);
    }

    public static Term randTerm() {
        switch (randInt(0, 2)) {
            case 0:
                return addRandTerm(randIntTerm());
            case 1:
                return addRandTerm(randBoolTerm());
            case 2:
                return addRandTerm(randFixedTerm());
            default:
                throw new IllegalStateException();
        }
    }

    public static Term randIntTerm() {
        return new IntTerm(randIrIntVar());
    }

    public static BoolTerm randBoolTerm() {
        return new BoolTerm(randIrBoolVar());
    }

    public static FixedTerm randFixedTerm() {
        return new FixedTerm(randInt());
    }

    private static Term addRandTerm(Term view) {
        switch (randInt(0, 9)) {
            case 0:
            case 1:
                return new MinusTerm(view);
            case 3:
            case 4:
                return new OffsetTerm(view, randInt());
            default:
                return view;
        }
    }

    private static Term addRandTerm(BoolTerm bool) {
        if (randBool()) {
            return addRandTerm(new NotTerm(bool));
        }
        return addRandTerm((Term) bool);
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
}
