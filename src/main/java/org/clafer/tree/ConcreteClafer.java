package org.clafer.tree;

import org.clafer.func.MinFunc;
import org.clafer.func.LinearFunc;
import org.clafer.func.ConstFunc;
import static org.clafer.Util.*;
import static choco.Choco.*;
import choco.kernel.model.Model;
import choco.kernel.model.variables.integer.IntegerVariable;
import choco.kernel.model.variables.set.SetVariable;
import choco.kernel.solver.Solver;
import java.io.IOException;
import org.clafer.Check;
import org.clafer.ChocoUtil;
import org.clafer.constraint.BoolChannelManager;
import org.clafer.constraint.SelectNManager;
import org.clafer.func.Func;
import org.clafer.tree.analysis.Analysis;

/**
 *
 * @author jimmy
 */
public class ConcreteClafer extends AtomicClafer {

    private final Card card;
    private final SetVariable[] childSet;
    private final IntegerVariable[] parentPointers;
    private final AtomicClafer parent;

    public ConcreteClafer(String name, int scope, Card card, AtomicClafer parent) {
        super(name, scope, makeSetVar(name, 0, scope - 1), makeBooleanVarArray(name + "@Membership", scope));

        this.card = Check.notNull(card);
        this.parent = Check.notNull(parent);

        this.childSet = skipCards(name + "@Child", parent.getScope(), 0, scope - 1, card);
        this.parentPointers = makeIntVarArray(name + "@Parent", scope, 0, parent.getScope());
    }

    public Card getCard() {
        return card;
    }

    public AtomicClafer getParent() {
        return parent;
    }

    public SetVariable[] getChildSet() {
        return childSet;
    }

    public IntegerVariable[] getParentPointers() {
        return parentPointers;
    }

    // Optimize
    private static SetVariable[] skipCards(String name, int dimension, int low, int high, Card card) {
        Func<Integer,Integer> lowFunc =
                card.hasLow()
                ? new MinFunc(new LinearFunc(card.getLow(), low), high)
                : new ConstFunc(low);
        Func<Integer,Integer> highFunc =
                card.hasHigh()
                ? new MinFunc(new LinearFunc(card.getHigh(), low + card.getHigh()), high)
                : new ConstFunc(high);

        SetVariable[] skip = new SetVariable[dimension];
        for (int i = 0; i < dimension; i++) {
            skip[i] =
                    makeSetVar(name + "#" + i,
                    lowFunc.apply(i),
                    highFunc.apply(i));
        }
        return skip;
    }

    @Override
    public ConcreteClafer extending(AbstractClafer superClafer) {
        super.extending(superClafer);
        return this;
    }

    @Override
    public void build(Model model, Analysis analysis) {
        model.addConstraint(setUnion(childSet, getSet()));
        model.addConstraint(BoolChannelManager.boolChannel(getMembership(), getSet()));
        SetVariable unused = makeSetVar(getName() + "@Unused", 0, getScope() - 1);
        model.addConstraint(inverseSet(parentPointers, cons(childSet, unused)));
        IntegerVariable num = makeIntVar(getName() + "@Num", 0, parent.getScope() + 1 /*, Options.V_NO_DECISION*/);
        model.addConstraint(increasingNValue(num, parentPointers));
        // Although this constraint is redundant, it lowers the number of backtracks.
        model.addConstraint(SelectNManager.selectN(getMembership(), getSet().getCard()));

        // TODO: ifthenelse
        for (int i = 0; i < childSet.length; i++) {
            if (card.isBounded()) {
                model.addConstraint(implies(eq(parent.getMembership()[i], 1),
                        ChocoUtil.betweenCard(childSet[i], card.getLow(), card.getHigh())));
            }
            model.addConstraint(implies(eq(parent.getMembership()[i], 0),
                    eqCard(childSet[i], 0)));
        }

        super.build(model, analysis);
    }

    @Override
    protected void print(Solver solver, String indent, int parent, Appendable output)
            throws IOException {
        int[] nums = solver.getVar(childSet[parent]).getValue();
        for (int num : nums) {
            output.append(indent + getName() + num).append('\n');
            for (Clafer child : getRefAndChildren()) {
                child.print(solver, indent + "  ", num, output);
            }
            if (hasSuperClafer()) {
                getSuperClafer().print(solver, this, indent, num, output);
            }
        }
    }
}
