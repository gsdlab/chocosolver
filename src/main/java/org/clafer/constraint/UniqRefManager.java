package org.clafer.constraint;

import static choco.Choco.*;
import choco.cp.model.CPModel;
import choco.cp.model.managers.IntConstraintManager;
import choco.cp.solver.CPSolver;
import choco.kernel.model.Model;
import choco.kernel.model.ModelException;
import choco.kernel.model.constraints.ComponentConstraint;
import choco.kernel.model.constraints.Constraint;
import choco.kernel.model.variables.integer.IntegerVariable;
import choco.kernel.solver.Solver;
import choco.kernel.solver.constraints.SConstraint;
import java.util.ArrayList;
import java.util.List;
import org.clafer.collection.IntPair;
import org.clafer.Util;

/**
 *
 * @author jimmy
 */
public class UniqRefManager extends IntConstraintManager {

    @Override
    public SConstraint makeConstraint(Solver solver, IntegerVariable[] variables, Object parameters, List<String> options) {
        if (solver instanceof CPSolver) {
            if (parameters instanceof IntPair) {
                IntPair pair = (IntPair) parameters;
                int offset = pair.getFst();
                int unused = pair.getSnd();
                IntegerVariable[] parents = new IntegerVariable[offset];
                IntegerVariable[] refs = new IntegerVariable[variables.length - offset];
                System.arraycopy(variables, 0, parents, 0, offset);
                System.arraycopy(variables, offset, refs, 0, refs.length);
                return new UniqRef(solver.getVar(parents), solver.getVar(refs), unused);
            }
        }
        throw new ModelException("Could not found a constraint manager in " + this.getClass() + " !");
    }

    public static Constraint uniqRef(IntegerVariable[] parents, IntegerVariable[] refs) {
        if (parents.length != refs.length) {
            throw new IllegalArgumentException();
        }

        // Should be right, assuming parents is from ConcreteClafer
        int unused = parents[0].getUppB();
        return new ComponentConstraint(
                UniqRefManager.class,
                new IntPair(parents.length, unused),
                Util.combine(parents, refs));
    }

    public static Constraint uniqRefPrim(IntegerVariable[] parents, IntegerVariable[] refs) {
        if (parents.length != refs.length) {
            throw new IllegalArgumentException();
        }

        // Should be right, assuming parents is from ConcreteClafer
        int unused = parents[0].getUppB();

        List<Constraint> constraints = new ArrayList<Constraint>();
        for (int i = 0; i < parents.length; i++) {
            for (int j = 0; j < parents.length; j++) {
                if (i != j) {
                    constraints.add(
                            implies(and(eq(parents[i], parents[j]), neq(parents[i], unused)),
                            neq(refs[i], refs[j])));
                }
            }
            constraints.add(implies(eq(parents[i], unused), eq(refs[i], 0)));
        }
        return and(constraints.toArray(new Constraint[constraints.size()]));
    }

    public static void main(String[] args) {
        Model m = new CPModel();

        IntegerVariable[] parents = makeIntVarArray("parent", 4, 0, 2);
        IntegerVariable[] refs = makeIntVarArray("ref", 4, -2, 2);
//        IntegerVariable[] parents = Choco.makeIntVarArray("parent", 4, 0, 30);
//        IntegerVariable[] refs = Choco.makeIntVarArray("ref", 4, -2, 30);

        m.addConstraint(UniqRefManager.uniqRef(parents, refs));

        System.out.println(Util.allSolutions(m).getStatistics());
        // #8501 solutions 601 Time (ms), 11271 Nodes, 11270 Backtracks, 0 Restarts - 
    }
}
