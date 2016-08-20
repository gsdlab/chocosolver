package org.clafer.test;

import gnu.trove.list.TIntList;
import gnu.trove.list.array.TIntArrayList;
import java.util.ArrayList;
import java.util.List;
import java.util.Random;
import org.chocosolver.solver.Model;
import org.chocosolver.solver.Solver;
import org.chocosolver.solver.constraints.Constraint;
import org.chocosolver.solver.constraints.Propagator;
import org.chocosolver.solver.search.strategy.Search;
import org.chocosolver.solver.search.strategy.selectors.values.SetValueSelector;
import org.chocosolver.solver.search.strategy.strategy.AbstractStrategy;
import org.chocosolver.solver.search.strategy.strategy.SetStrategy;
import org.chocosolver.solver.variables.BoolVar;
import org.chocosolver.solver.variables.CStringVar;
import org.chocosolver.solver.variables.IntVar;
import org.chocosolver.solver.variables.SetVar;
import org.chocosolver.solver.variables.Variable;
import org.chocosolver.util.ESat;
import org.chocosolver.util.objects.setDataStructures.ISetIterator;
import org.clafer.choco.constraint.Constraints;
import org.clafer.domain.Domain;
import org.clafer.domain.Domains;
import static org.clafer.domain.Domains.EmptyDomain;
import static org.clafer.domain.Domains.boundDomain;
import static org.clafer.domain.Domains.constantDomain;
import static org.clafer.domain.Domains.enumDomain;
import org.clafer.ir.IrBoolVar;
import org.clafer.ir.IrIntVar;
import org.clafer.ir.IrSetVar;
import org.clafer.ir.IrStringVar;
import static org.clafer.ir.Irs.False;
import static org.clafer.ir.Irs.True;
import static org.clafer.ir.Irs.bool;
import static org.clafer.ir.Irs.domainInt;
import static org.clafer.ir.Irs.set;
import static org.clafer.ir.Irs.string;

/**
 *
 * @author jimmy
 */
public class TestUtil {

    private static final Random rand = new Random();
    private static int varCount = 0;

    public static ESat isEntailed(Constraint constraint) {
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

    public static Solver randomizeStrategy(Solver solver) {
        List<IntVar> intVars = new ArrayList<>();
        List<SetVar> setVars = new ArrayList<>();
        for (Variable var : solver.getModel().getVars()) {
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
            solver.setSearch(
                    randomSearch(setVars.toArray(new SetVar[setVars.size()])),
                    randomSearch(intVars.toArray(new IntVar[intVars.size()])));
        } else {
            solver.setSearch(
                    randomSearch(intVars.toArray(new IntVar[intVars.size()])),
                    randomSearch(setVars.toArray(new SetVar[setVars.size()])));
        }
        return solver;
    }

    private static AbstractStrategy<IntVar> randomSearch(IntVar[] vars) {
        return Search.randomSearch(vars, rand.nextLong());
    }

    private static SetStrategy randomSearch(SetVar[] vars) {
        return Search.setVarSearch(
                new org.chocosolver.solver.search.strategy.selectors.variables.Random<>(rand.nextLong()),
                new RandomSetValueSelector(), randBool(), vars);
    }

    private static class RandomSetValueSelector implements SetValueSelector {

        @Override
        public int selectValue(SetVar s) {
            int m = rand.nextInt(s.getUB().size() - s.getLB().size());
            ISetIterator iter = s.getUB().iterator();
            while (iter.hasNext()) {
                int i = iter.nextInt();
                if (!s.getLB().contains(i)) {
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

    @SafeVarargs
    public static <T> T randElement(T... array) {
        return array[rand.nextInt(array.length)];
    }

    public static Domain randDomain(int low, int high) {
        if (low > high) {
            throw new IllegalArgumentException();
        }
        return rand.nextInt(4) == 0
                ? EmptyDomain
                : randNonEmptyDomain(low, high);
    }

    public static Domain randDomain() {
        return randDomain(-4, 4);
    }

    public static Domain randPositiveDomain() {
        return randDomain(0, 4);
    }

    public static Domain randNonEmptyDomain(int low, int high) {
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

    public static Domain randNonEmptyDomain() {
        return randNonEmptyDomain(-4, 4);
    }

    public static Domain randNonEmptyPositiveDomain() {
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

    public static IrIntVar randIrIntVar(String name, int low, int high) {
        if (low > high) {
            throw new IllegalArgumentException();
        }
        return domainInt(name, randNonEmptyDomain(low, high));
    }

    public static IrIntVar randIrIntVar() {
        return randIrIntVar("Int", -4, 4);
    }

    public static IrIntVar randPositiveIrIntVar() {
        return randIrIntVar("Int", 0, 4);
    }

    public static IrSetVar randIrSetVar(int low, int high) {
        if (low > high) {
            throw new IllegalArgumentException();
        }
        Domain env = randDomain(low, high);
        Domain ker = randDomain(low, high).intersection(env);
        int a = randInt(ker.size(), env.size());
        int b = randInt(ker.size(), env.size());
        Domain card = a < b ? randNonEmptyDomain(a, b) : randNonEmptyDomain(b, a);
        return set("Set" + varCount++, env, ker, card);
    }

    public static IrSetVar randIrSetVar() {
        return TestUtil.randIrSetVar(-4, 4);
    }

    public static IrSetVar randIrSetVarNoCard(int low, int high) {
        if (low > high) {
            throw new IllegalArgumentException();
        }
        Domain env = randDomain(low, high);
        Domain ker = randDomain(low, high).intersection(env);
        int a = randInt(ker.size(), env.size());
        int b = randInt(ker.size(), env.size());
        return set("Set" + varCount++, env, ker);
    }

    public static IrSetVar randIrSetVarNoCard() {
        return TestUtil.randIrSetVarNoCard(-4, 4);
    }

    public static IrSetVar randPositiveIrSetVar() {
        return TestUtil.randIrSetVar(0, 4);
    }

    public static IrStringVar randIrStringVar() {
        String name = "String" + varCount++;
        IrIntVar length = randIrIntVar("|" + name + "|", 0, 4);
        IrIntVar[] chars = new IrIntVar[length.getDomain().getHighBound()];
        for (int i = 0; i < chars.length; i++) {
            Domain domain = randNonEmptyDomain('a', 'c');
            chars[i] = domainInt(name + "[" + i + "]",
                    i < length.getDomain().getLowBound()
                            ? domain : domain.insert(0));
        }
        return string(name, chars, length);
    }

    public static IrStringVar randNonEmptyIrStringVar() {
        String name = "String" + varCount++;
        IrIntVar length = randIrIntVar("|" + name + "|", 1, 4);
        IrIntVar[] chars = new IrIntVar[length.getDomain().getHighBound()];
        for (int i = 0; i < chars.length; i++) {
            Domain domain = randNonEmptyDomain('a', 'c');
            chars[i] = domainInt(name + "[" + i + "]",
                    i < length.getDomain().getLowBound()
                            ? domain : domain.insert(0));
        }
        return string(name, chars, length);
    }

    public static BoolVar toVar(IrBoolVar var, Model model) {
        switch (var.getDomain()) {
            case FalseDomain:
                return model.boolVar(false);
            case TrueDomain:
                return model.boolVar(true);
            case TrueFalseDomain:
                return model.boolVar(var.getName());
            default:
                throw new IllegalStateException();
        }
    }

    public static IntVar toVar(IrIntVar var, Model model) {
        Domain domain = var.getDomain();
        return domain.isBounded()
                ? model.intVar(var.getName(), domain.getLowBound(), domain.getHighBound())
                : model.intVar(var.getName(), domain.getValues());
    }

    public static SetVar toVar(IrSetVar var, Model model) {
        Domain ker = var.getKer();
        Domain env = var.getEnv();
        Domain card = var.getCard();
        SetVar setVar = model.setVar(
                var.getName(),
                ker.getValues(),
                env.getValues());
        if (card.equals(Domains.boundDomain(ker.size(), env.size()))) {
            return setVar;
        }
        IntVar setCardVar = model.intVar("|" + var.getName() + "|", card.getValues());
        setVar.setCard(setCardVar);
        return setVar;
    }

    public static CStringVar toVar(IrStringVar var, Model model) {
        IntVar[] chars = new IntVar[var.getCharVars().length];
        for (int i = 0; i < chars.length; i++) {
            chars[i] = toVar(var.getCharVars()[i], model);
        }
        IntVar length = toVar(var.getLengthVar(), model);
        model.post(Constraints.length(chars, length));
        return new CStringVar(chars, length);
    }

    public static BoolVar randBoolVar(Model model) {
        return toVar(randIrBoolVar(), model);
    }

    public static IntVar randIntVar(String name, int low, int high, Model model) {
        if (low > high) {
            throw new IllegalArgumentException();
        }
        return toVar(randIrIntVar(name, low, high), model);
    }

    public static IntVar randIntVar(Model model) {
        return toVar(randIrIntVar(), model);
    }

    public static IntVar randPositiveIntVar(Model model) {
        return toVar(randPositiveIrIntVar(), model);
    }

    public static SetVar randSetVar(int low, int high, Model model) {
        if (low > high) {
            throw new IllegalArgumentException();
        }
        return toVar(randIrSetVar(low, high), model);
    }

    public static SetVar randSetVar(Model model) {
        return toVar(randIrSetVar(), model);
    }

    public static SetVar randSetVarNoCard(int low, int high, Model model) {
        if (low > high) {
            throw new IllegalArgumentException();
        }
        return toVar(randIrSetVarNoCard(low, high), model);
    }

    public static SetVar randSetVarNoCard(Model model) {
        return toVar(randIrSetVarNoCard(), model);
    }

    public static SetVar randPositiveSetVar(Model model) {
        return toVar(randPositiveIrSetVar(), model);
    }

    public static CStringVar randStringVar(Model model) {
        return toVar(randIrStringVar(), model);
    }

    public static CStringVar randNonEmptyStringVar(Model model) {
        return toVar(randNonEmptyIrStringVar(), model);
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
}
