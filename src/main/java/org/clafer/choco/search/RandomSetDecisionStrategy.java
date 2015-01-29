package org.clafer.choco.search;

import java.util.Random;
import org.chocosolver.solver.exception.ContradictionException;
import org.chocosolver.solver.search.strategy.assignments.DecisionOperator;
import org.chocosolver.solver.search.strategy.decision.Decision;
import org.chocosolver.solver.search.strategy.decision.fast.FastDecisionSet;
import org.chocosolver.solver.search.strategy.selectors.SetValueSelector;
import org.chocosolver.solver.search.strategy.selectors.VariableSelector;
import org.chocosolver.solver.search.strategy.strategy.AbstractStrategy;
import org.chocosolver.solver.variables.SetVar;
import org.chocosolver.util.PoolManager;

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
