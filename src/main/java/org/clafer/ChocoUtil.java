package org.clafer;

import choco.Choco;
import choco.cp.model.CPModel;
import choco.cp.solver.CPSolver;
import choco.kernel.model.Model;
import choco.kernel.model.constraints.Constraint;
import choco.kernel.model.constraints.cnf.Singleton;
import choco.kernel.model.variables.integer.IntegerExpressionVariable;
import choco.kernel.model.variables.integer.IntegerVariable;
import choco.kernel.model.variables.set.SetVariable;
import choco.kernel.solver.Solver;
import java.util.ArrayList;
import java.util.List;
import org.clafer.constraint.SingletonManager;

/**
 *
 * @author jimmy
 */
public class ChocoUtil {

    public static IntegerExpressionVariable plus(IntegerExpressionVariable t, int v) {
        if (v == 0) {
            return t;
        }
        return Choco.plus(t, v);
    }

    public static void main(String[] args) {
        IntegerVariable i1 = Choco.makeIntVar("i1", 1, 10);
        IntegerVariable i2 = Choco.makeIntVar("i2", 1, 10);
        IntegerVariable i3 = Choco.makeIntVar("i3", 1, 10);
        SetVariable s1 = Choco.makeSetVar("s1", 1, 10);

        Model m = new CPModel();
        m.addConstraint(Choco.ifOnlyIf(Choco.eq(i1, i2), SingletonManager.singleton(i3, s1)));

        Solver s = new CPSolver();
        s.read(m);

        if (s.solve()) {
            do {
                System.out.println(s.solutionToString());
            } while (s.nextSolution());
        }
    }

    public static Constraint betweenCard(SetVariable set, int low, int high) {
        if (low == high) {
            return Choco.eqCard(set, low);
        }
        List<Constraint> constraints = new ArrayList<Constraint>();
        if (low != 0) {
            constraints.add(Choco.geqCard(set, low));
        }
        if (high != Integer.MAX_VALUE) {
            constraints.add(Choco.leqCard(set, high));
        }
        return and(constraints);
    }

    public static Constraint or(List<Constraint> constraints) {
        return Choco.or(constraints.toArray(new Constraint[constraints.size()]));
    }

    public static Constraint and(List<Constraint> constraints) {
        return Choco.and(constraints.toArray(new Constraint[constraints.size()]));
    }
}
