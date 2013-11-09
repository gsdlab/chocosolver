package org.clafer.ir;

import gnu.trove.list.TIntList;
import gnu.trove.list.array.TIntArrayList;
import java.util.ArrayList;
import java.util.List;
import java.util.Random;
import org.clafer.choco.constraint.RandomSetSearchStrategy;
import static org.clafer.ir.Irs.*;
import solver.Solver;
import solver.constraints.set.SCF;
import solver.propagation.PropagationEngineFactory;
import solver.search.strategy.IntStrategyFactory;
import solver.search.strategy.strategy.StrategiesSequencer;
import solver.variables.BoolVar;
import solver.variables.IntVar;
import solver.variables.SetVar;
import solver.variables.VF;
import solver.variables.Variable;

/**
 *
 * @author jimmy
 */
public class ExprTest {

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
        IrDomain ker = randDomain(low, high);
        IrDomain env = IrUtil.union(ker, randDomain(low, high));
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
        SetVar setVar = VF.set(var.getName(), var.getEnv().getValues(), var.getKer().getValues(), solver);
        IntVar cardVar = VF.enumerated("|" + var.getName() + "|", var.getCard().getValues(), solver);
        solver.post(SCF.cardinality(setVar, cardVar));
        return setVar;
    }

    public static SetVar[] toSetVars(IrSetVar[] vars, Solver solver) {
        SetVar[] ints = new SetVar[vars.length];
        for (int i = 0; i < ints.length; i++) {
            ints[i] = toSetVar(vars[i], solver);
        }
        return ints;
    }

    public Solver randomizeStrategy(Solver solver) {
        solver.set(PropagationEngineFactory.PROPAGATORDRIVEN.make(solver));
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
}
