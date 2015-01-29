package org.clafer.choco.search;

import java.util.Random;
import solver.exception.ContradictionException;
import solver.search.strategy.assignments.DecisionOperator;
import solver.search.strategy.decision.Decision;
import solver.search.strategy.decision.fast.FastDecisionSet;
import solver.search.strategy.selectors.SetValueSelector;
import solver.search.strategy.selectors.VariableSelector;
import solver.search.strategy.strategy.AbstractStrategy;
import solver.variables.SetVar;
import util.PoolManager;

/**
 *
 * @author jimmy
 */
public class RandomSetDecisionStrategy extends AbstractStrategy<SetVar> {

    private final PoolManager<FastDecisionSet> pool;
    private final VariableSelector<SetVar> varSelector;
    private final SetValueSelector valSelector;
    private final Random rand;

    public RandomSetDecisionStrategy(SetVar[] scope, VariableSelector<SetVar> varSelector, SetValueSelector valSelector) {
        this(scope, varSelector, valSelector, new Random());
    }

    public RandomSetDecisionStrategy(SetVar[] scope, VariableSelector<SetVar> varSelector, SetValueSelector valSelector, Random rand) {
        super(scope);
        this.varSelector = varSelector;
        this.valSelector = valSelector;
        this.pool = new PoolManager<>();
        this.rand = rand;
    }

    @Override
    public void init() throws ContradictionException {
    }

    @Override
    public Decision<SetVar> getDecision() {
        SetVar variable = varSelector.getVariable(vars);
        return computeDecision(variable);
    }

    @Override
    public Decision<SetVar> computeDecision(SetVar s) {
        if (s == null) {
            return null;
        }
        assert !s.isInstantiated();
        FastDecisionSet d = pool.getE();
        if (d == null) {
            d = new FastDecisionSet(pool);
        }
        d.set(s, valSelector.selectValue(s), rand.nextBoolean() ? DecisionOperator.set_force : DecisionOperator.set_remove);
        return d;
    }
}
