package org.clafer.choco.search;

import java.util.Random;
import org.chocosolver.solver.search.strategy.selectors.values.SetValueSelector;
import org.chocosolver.solver.variables.SetVar;
import org.chocosolver.util.objects.setDataStructures.ISetIterator;

/**
 *
 * @author jimmy
 */
public class RandomSetValueSelector implements SetValueSelector {

    private final Random rand;

    public RandomSetValueSelector(Random rand) {
        this.rand = rand;
    }

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
