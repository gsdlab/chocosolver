package org.clafer;

import org.clafer.ast.AstConcreteClafer;
import org.clafer.ast.AstModel;
import static org.clafer.ast.Asts.*;
import org.clafer.compiler.ClaferCompiler;
import org.clafer.objective.Objective;
import org.clafer.compiler.ClaferOptimizer;
import org.clafer.scope.Scope;
import static org.junit.Assert.*;
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
                Objective.maximize(joinRef(global(a))));
        int count = 0;
        while (solver.find()) {
            assertEquals(2, solver.instance().getFst().intValue());
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
                Objective.minimize(joinRef(global(a))));
        int count = 0;
        while (solver.find()) {
            assertEquals(2, solver.instance().getFst().intValue());
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
                Objective.maximize(joinRef(global(a))));
        int count = 0;
        while (solver.find()) {
            assertEquals(4, solver.instance().getFst().intValue());
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
                Objective.minimize(joinRef(global(a))));
        int count = 0;
        while (solver.find()) {
            assertEquals(-4, solver.instance().getFst().intValue());
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
                Objective.maximize(joinRef(global(a))));
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
                Objective.minimize(joinRef(global(a))));
        assertEquals(0, solver.allInstances().length);
    }
}
