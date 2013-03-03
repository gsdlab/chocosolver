package org.clafer.tree;

import org.clafer.func.MinFunc;
import org.clafer.func.LinearFunc;
import org.clafer.func.IntFunc;
import org.clafer.func.ConstFunc;
import static org.clafer.Exprs.*;
import static org.clafer.Util.*;
import choco.Choco;
import choco.kernel.model.Model;
import choco.kernel.model.variables.integer.IntegerVariable;
import choco.kernel.model.variables.set.SetVariable;
import choco.kernel.solver.Solver;
import java.util.Objects;
import org.clafer.Exprs;

/**
 *
 * @author jimmy
 */
public class ConcreteClafer extends AtomicClafer {

    private final Card card;
    private final SetVariable[] childSet;
    private final IntegerVariable[] parentPointers;
    private final AbstractClafer sup;

    public ConcreteClafer(Model model, String name, int scope, Card card, AtomicClafer parent) {
        this(model, name, scope, card, parent, null);
    }

    public ConcreteClafer(Model model, String name, int scope, Card card,
            AtomicClafer parent, AbstractClafer sup) {
        super(name, scope, setVar(name, 0, scope - 1));

        this.card = card;
        this.sup = sup;

        this.childSet = skipCards(name + "@Child", parent.getScope(), 0, scope - 1, card);
        model.addConstraint(Choco.setUnion(childSet, getSet()));
        this.parentPointers = intArray(name + "@Parent", scope, 0, parent.getScope());
        SetVariable unused = setVar(name + "@Unused", 0, scope - 1);
        model.addConstraint(Choco.inverseSet(parentPointers, cons(childSet, unused)));
        IntegerVariable num = intVar(name + "@Num", 0, parent.getScope() + 1 /*, Options.V_NO_DECISION*/);
        model.addConstraint(Choco.increasingNValue(num, parentPointers));

        // TODO: ifthenelse
        if (card.isBounded()) {
            for (int i = 0; i < childSet.length; i++) {
                model.addConstraint(Choco.implies(Choco.member(i, parent.getSet()),
                        Exprs._betweenCard(childSet[i], card.getLow(), card.getHigh())));
                model.addConstraint(Choco.implies(Choco.notMember(i, parent.getSet()),
                        Choco.eqCard(childSet[i], 0)));
            }
        }

        parent.addChild(this);
        if (sup != null) {
            sup.addSubclafer(this);
        }
    }

    public Card getCard() {
        return card;
    }

    public IntegerVariable[] getParentPointers() {
        return parentPointers;
    }

    // Optimize
    private static SetVariable[] skipCards(String name, int dimension, int low, int high, Card card) {
        IntFunc lowFunc =
                card.hasLow()
                ? new MinFunc(new LinearFunc(card.getLow(), low), high)
                : new ConstFunc(low);
        IntFunc highFunc =
                card.hasHigh()
                ? new MinFunc(new LinearFunc(card.getHigh(), low + card.getHigh()), high)
                : new ConstFunc(high);

        SetVariable[] skip = new SetVariable[dimension];
        for (int i = 0; i < dimension; i++) {
            skip[i] =
                    setVar(name + "#" + i,
                    lowFunc.apply(i),
                    highFunc.apply(i));
        }
        return skip;
    }

    @Override
    protected void print(Solver solver, String indent, int parent) {
        int[] nums = solver.getVar(childSet[parent]).getValue();
        for (int num : nums) {
            System.out.println(indent + getName() + num);
            for (Clafer child : getChildren()) {
                child.print(solver, indent + "  ", num);
            }
        }
    }
}
