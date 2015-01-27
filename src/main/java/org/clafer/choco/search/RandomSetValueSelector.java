package org.clafer.choco.search;

import java.util.Random;
import org.chocosolver.solver.search.strategy.selectors.SetValueSelector;
import org.chocosolver.solver.variables.SetVar;

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
        int m = rand.nextInt(s.getEnvelopeSize() - s.getKernelSize());
        for (int i = s.getEnvelopeFirst(); i != SetVar.END; i = s.getEnvelopeNext()) {
            if (!s.kernelContains(i)) {
                if (m == 0) {
                    return i;
                }
                m--;
            }
        }
        throw new IllegalStateException();
    }
}
