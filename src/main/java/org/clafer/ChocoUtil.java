package org.clafer;

import choco.Choco;
import choco.cp.model.CPModel;
import choco.cp.solver.CPSolver;
import choco.kernel.model.Model;
import choco.kernel.model.constraints.Constraint;
import choco.kernel.model.variables.integer.IntegerExpressionVariable;
import choco.kernel.model.variables.set.SetVariable;
import choco.kernel.solver.Solver;
import java.util.ArrayList;
import java.util.List;
import org.clafer.tree.Card;

/**
 * Extends choco.Choco
 * 
 * @author jimmy
 */
public class ChocoUtil {

    public static void main(String[] args) {
        Model m = new CPModel();
        SetVariable set = Choco.makeSetVar("s", -1, 1);
        m.addConstraint(Choco.eqCard(set, 3));
        Solver s = new CPSolver();
        s.read(m);
        if (s.solve()) {
            do {
                System.out.println(s.solutionToString());
            } while (s.nextSolution());
        }
    }

    public static IntegerExpressionVariable plus(IntegerExpressionVariable t, int v) {
        if (v == 0) {
            return t;
        }
        Integer constant = Util.getConstant(t);
        if (constant != null) {
            return Choco.constant(constant + v);
        }
        return Choco.plus(t, v);
    }

    public static Constraint betweenCard(IntegerExpressionVariable setCard, Card card) {
        return betweenCard(setCard, card.getLow(), card.getHigh());
    }

    public static Constraint betweenCard(IntegerExpressionVariable setCard, int low, int high) {
        if (low == high) {
            return Choco.eq(setCard, low);
        }
        List<Constraint> constraints = new ArrayList<Constraint>();
        if (low != 0) {
            constraints.add(Choco.geq(setCard, low));
        }
        if (high != Integer.MAX_VALUE) {
            constraints.add(Choco.leq(setCard, high));
        }
        return and(constraints);
    }

    public static Constraint betweenCard(SetVariable set, Card card) {
        return betweenCard(set, card.getLow(), card.getHigh());
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
            // TODO: do this optimization everywhere.
            if (high < set.getCard().getUppB()) {
                constraints.add(Choco.leqCard(set, high));
            }
        }
        return and(constraints);
    }

    public static IntegerExpressionVariable sum(IntegerExpressionVariable[] ivs) {
        switch (ivs.length) {
            case 0:
                return Choco.ZERO;
            case 1:
                return ivs[0];
            case 2:
                return Choco.plus(ivs[0], ivs[1]);
            default:
                return Choco.sum(ivs);
        }
    }

    public static IntegerExpressionVariable sum(List<? extends IntegerExpressionVariable> ivs) {
        return sum(ivs.toArray(new IntegerExpressionVariable[ivs.size()]));
    }

    public static Constraint or(List<Constraint> constraints) {
        return Choco.or(constraints.toArray(new Constraint[constraints.size()]));
    }

    public static Constraint and(List<Constraint> constraints) {
        return Choco.and(constraints.toArray(new Constraint[constraints.size()]));
    }
}
