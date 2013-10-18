package org.clafer.ir;

import gnu.trove.list.TIntList;
import gnu.trove.list.array.TIntArrayList;
import java.util.Random;
import static org.clafer.ir.IrBoolDomain.BoolDomain;
import static org.clafer.ir.IrBoolDomain.FalseDomain;
import static org.clafer.ir.IrBoolDomain.TrueDomain;
import static org.clafer.ir.Irs.False;
import static org.clafer.ir.Irs.True;
import static org.clafer.ir.Irs.bool;
import static org.clafer.ir.Irs.boundDomain;
import static org.clafer.ir.Irs.constantDomain;
import static org.clafer.ir.Irs.domainInt;
import static org.clafer.ir.Irs.enumDomain;
import solver.Solver;
import solver.variables.BoolVar;
import solver.variables.IntVar;
import solver.variables.VF;

/**
 *
 * @author jimmy
 */
public class ExprTest {

    private final Random rand = new Random();
    private int varCount = 0;

    public int nextInt(int n) {
        return rand.nextInt(n);
    }

    public int nextIntBetween(int min, int max) {
        return rand.nextInt(max - min + 1) + min;
    }

    public IrDomain randDomain() {
        switch (rand.nextInt(5)) {
            case 0:
                return constantDomain(nextIntBetween(-5, 5));
            case 1:
            case 2:
                int a = nextIntBetween(-5, 5);
                int b = nextIntBetween(-5, 5);
                return a < b ? boundDomain(a, b) : boundDomain(b, a);
            case 3:
            case 4:
                TIntList d;
                do {
                    d = new TIntArrayList();
                    for (int i = -5; i <= 5; i++) {
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

    public IrIntVar randInt() {
        return domainInt("Int" + varCount++, randDomain());
    }

    public IrBoolVar[] randBools(int n) {
        IrBoolVar[] bools = new IrBoolVar[n];
        for (int i = 0; i < bools.length; i++) {
            bools[i] = randBool();
        }
        return bools;
    }

    public IrIntVar[] randInts(int n) {
        IrIntVar[] ints = new IrIntVar[n];
        for (int i = 0; i < ints.length; i++) {
            ints[i] = randInt();
        }
        return ints;
    }

    public BoolVar toBoolVar(IrBoolVar var, Solver solver) {
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

    public BoolVar[] toBoolVar(IrBoolVar[] vars, Solver solver) {
        BoolVar[] bools = new BoolVar[vars.length];
        for (int i = 0; i < bools.length; i++) {
            bools[i] = toBoolVar(vars[i], solver);
        }
        return bools;
    }

    public IntVar toIntVar(IrIntVar var, Solver solver) {
        return VF.enumerated(var.getName(), var.getDomain().getValues(), solver);
    }

    public IntVar[] toIntVar(IrIntVar[] vars, Solver solver) {
        IntVar[] ints = new IntVar[vars.length];
        for (int i = 0; i < ints.length; i++) {
            ints[i] = toIntVar(vars[i], solver);
        }
        return ints;
    }
}
