package org.clafer.choco.constraint;

import solver.exception.ContradictionException;
import solver.search.strategy.assignments.DecisionOperator;
import solver.search.strategy.decision.Decision;
import solver.search.strategy.decision.fast.FastDecisionSet;
import solver.search.strategy.strategy.AbstractStrategy;
import solver.variables.SetVar;
import util.PoolManager;

/**
 *
 * @author jimmy
 */
public class SetSearchRemoveStrategy extends AbstractStrategy<SetVar> {

    private final PoolManager<FastDecisionSet> pool;

    public SetSearchRemoveStrategy(SetVar[] variables) {
        super(variables);
        pool = new PoolManager<FastDecisionSet>();
    }

    @Override
    public void init() throws ContradictionException {
    }

    @Override
    public Decision<SetVar> getDecision() {
        for (SetVar s : vars) {
            Decision<SetVar> d = computeDecision(s);
            if (d != null) {
                return d;
            }
        }
        return null;
    }

    @Override
    public Decision<SetVar> computeDecision(SetVar s) {
        if (!s.instantiated()) {
            for (int i = s.getEnvelopeFirst(); i != SetVar.END; i = s.getEnvelopeNext()) {
                if (!s.kernelContains(i)) {
                    FastDecisionSet d = pool.getE();
                    if (d == null) {
                        d = new FastDecisionSet(pool);
                    }
                    d.set(s, i, DecisionOperator.set_remove);
                    return d;
                }
            }
        }
        return null;
    }
}
