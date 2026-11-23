package org.clafer;

import org.clafer.ast.AstConcreteClafer;
import org.clafer.ast.AstModel;
import static org.clafer.ast.Asts.$this;
import static org.clafer.ast.Asts.IntType;
import static org.clafer.ast.Asts.Mandatory;
import static org.clafer.ast.Asts.Optional;
import static org.clafer.ast.Asts.card;
import static org.clafer.ast.Asts.constant;
import static org.clafer.ast.Asts.equal;
import static org.clafer.ast.Asts.global;
import static org.clafer.ast.Asts.joinRef;
import static org.clafer.ast.Asts.newModel;
import org.clafer.compiler.ClaferCompiler;
import org.clafer.compiler.ClaferOptimizer;
import org.clafer.objective.Objective;
import org.clafer.scope.Scope;
import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;
import org.junit.Test;

/**
 *
 * @author jimmy
 */
public class SimpleOptimizationTest {

    /**
     * <pre>
     * A ->> int 3
     *     [this.ref = 2]
     *
     * << max A >>
     * </pre>
     */
    @Test(timeout = 60000)
    public void testMaximalSolutionKnown() {
        AstModel model = newModel();

        AstConcreteClafer a = model.addChild("A").refTo(IntType).withCard(3, 3);
        a.addConstraint(equal(joinRef($this()), constant(2)));

        ClaferOptimizer solver = ClaferCompiler.compile(model, Scope.defaultScope(3),
                Objective.maximize(joinRef(a)));
        int count = 0;
        while (solver.find()) {
            assertArrayEquals(new int[]{2}, solver.optimalValues());
            count++;
        }
        assertEquals(1, count);
    }

    /**
     * <pre>
     * A ->> int 3
     *     [this.ref = 2]
     *
     * << min A >>
     * </pre>
     */
    @Test(timeout = 60000)
    public void testMinimalSolutionKnown() {
        AstModel model = newModel();

        AstConcreteClafer a = model.addChild("A").refTo(IntType).withCard(3, 3);
        a.addConstraint(equal(joinRef($this()), constant(2)));

        ClaferOptimizer solver = ClaferCompiler.compile(model, Scope.defaultScope(3),
                Objective.minimize(joinRef(a)));
        int count = 0;
        while (solver.find()) {
            assertArrayEquals(new int[]{2}, solver.optimalValues());
            count++;
        }
        assertEquals(1, count);
    }

    /**
     * <pre>
     * A -> int
     * B ?
     *
     * << max A >>
     * </pre>
     */
    @Test(timeout = 60000)
    public void testMaximalMultipleSolutions() {
        AstModel model = newModel();

        AstConcreteClafer a = model.addChild("A").refTo(IntType).withCard(Mandatory);
        AstConcreteClafer b = model.addChild("B").withCard(Optional);

        ClaferOptimizer solver = ClaferCompiler.compile(model, Scope.intLow(-4).intHigh(4),
                Objective.maximize(joinRef(a)));
        int count = 0;
        while (solver.find()) {
            assertArrayEquals(new int[]{4}, solver.optimalValues());
            count++;
        }
        assertEquals(2, count);
    }

    /**
     * <pre>
     * A -> int
     * B ?
     *
     * << min A >>
     * </pre>
     */
    @Test(timeout = 60000)
    public void testMinimalMultipleSolutions() {
        AstModel model = newModel();

        AstConcreteClafer a = model.addChild("A").refTo(IntType).withCard(Mandatory);
        AstConcreteClafer b = model.addChild("B").withCard(Optional);

        ClaferOptimizer solver = ClaferCompiler.compile(model, Scope.intLow(-4).intHigh(4),
                Objective.minimize(joinRef(a)));
        int count = 0;
        while (solver.find()) {
            assertArrayEquals(new int[]{-4}, solver.optimalValues());
            count++;
        }
        assertEquals(2, count);
    }

    /**
     * <pre>
     * A -> int
     * [#A = 0]
     *
     * << max A >>
     * </pre>
     */
    @Test(timeout = 60000)
    public void testMaximalNoSolutions() {
        AstModel model = newModel();

        AstConcreteClafer a = model.addChild("A").refTo(IntType).withCard(Mandatory);
        model.addConstraint(equal(card(global(a)), constant(0)));

        ClaferOptimizer solver = ClaferCompiler.compile(model, Scope.intLow(-4).intHigh(4),
                Objective.maximize(joinRef(a)));
        assertEquals(0, solver.allInstances().length);
    }

    /**
     * <pre>
     * A -> int
     * [#A = 0]
     *
     * << min A >>
     * </pre>
     */
    @Test(timeout = 60000)
    public void testMinimalNoSolutions() {
        AstModel model = newModel();

        AstConcreteClafer a = model.addChild("A").refTo(IntType).withCard(Mandatory);
        model.addConstraint(equal(card(global(a)), constant(0)));

        ClaferOptimizer solver = ClaferCompiler.compile(model, Scope.intLow(-4).intHigh(4),
                Objective.minimize(joinRef(a)));
        assertEquals(0, solver.allInstances().length);
    }
}
