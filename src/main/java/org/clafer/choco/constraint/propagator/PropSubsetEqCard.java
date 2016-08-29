package org.clafer.choco.constraint.propagator;

import org.chocosolver.solver.constraints.Propagator;
import org.chocosolver.solver.constraints.PropagatorPriority;
import org.chocosolver.solver.exception.ContradictionException;
import org.chocosolver.solver.variables.IntVar;
import org.chocosolver.solver.variables.SetVar;
import org.chocosolver.solver.variables.Variable;
import org.chocosolver.util.ESat;
import org.chocosolver.util.objects.setDataStructures.ISet;
import org.chocosolver.util.objects.setDataStructures.ISetIterator;

/**
 *
 * @author jimmy
 */
public class PropSubsetEqCard extends Propagator<Variable> {

    private final SetVar sub;
    private final IntVar subCard;
    private final SetVar sup;
    private final IntVar supCard;

    public PropSubsetEqCard(SetVar sub, IntVar subCard, SetVar sup, IntVar supCard) {
        super(new Variable[]{sub, subCard, sup, supCard}, PropagatorPriority.TERNARY, false);
        this.sub = sub;
        this.subCard = subCard;
        this.sup = sup;
        this.supCard = supCard;
    }

    private static int sizeOfIntersection(ISet a, ISet b) {
        ISetIterator iter = a.iterator();
        int count = 0;
        while (iter.hasNext()) {
            if (b.contains(iter.nextInt())) {
                count++;
            }
        }
        return count;
    }

    @Override
    public void propagate(int evtmask) throws ContradictionException {
        int freeChoice = supCard.getUB() - sup.getLB().size();
        int intersection = sizeOfIntersection(sub.getUB(), sup.getLB());
        if (intersection + freeChoice <= subCard.getLB()) {
            ISetIterator ker = sup.getLB().iterator();
            while (ker.hasNext()) {
                int i = ker.nextInt();
                if (sub.getUB().contains(i)) {
                    sub.force(i, this);
                }
            }
            ISetIterator env = sup.getUB().iterator();
            while (env.hasNext()) {
                int i = env.nextInt();
                if (!sub.getUB().contains(i) && !sup.getLB().contains(i)) {
                    sup.remove(i, this);
                }
            }
        }

        int wastedChoice = sup.getLB().size() - intersection;
        supCard.updateLowerBound(subCard.getLB() + wastedChoice, this);
        subCard.updateUpperBound(supCard.getUB() - wastedChoice, this);

        if (subCard.getLB() == supCard.getUB()) {
            PropUtil.envSubsetEnv(sup, sub, this);
            PropUtil.kerSubsetKer(sup, sub, this);
        }
    }

    @Override
    public ESat isEntailed() {
        // TODO
        return ESat.TRUE;
    }
}
