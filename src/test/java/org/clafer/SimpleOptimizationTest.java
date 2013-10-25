package org.clafer;

import org.clafer.ast.AstConcreteClafer;
import org.clafer.ast.AstModel;
import static org.clafer.ast.Asts.*;
import org.clafer.compiler.ClaferCompiler;
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
     * << min A >>
     * </pre>
     */
    @Test
    public void testMinimalSolutionKnown() {
        AstModel model = newModel();

        AstConcreteClafer a = model.addChild("A").refTo(IntType).withCard(3, 3);
        a.addConstraint(equal(joinRef($this()), constant(2)));

        ClaferOptimizer solver = ClaferCompiler.compileMinimize(model, Scope.defaultScope(3), a.getRef());
        assertEquals(6, solver.optimal().getFst().intValue());
    }

    /**
     * <pre>
     * A ->> int 3
     *     [this.ref = 2]
     *
     * << max A >>
     * </pre>
     */
    public void testMaximalSolutionKnown() {
        AstModel model = newModel();

        AstConcreteClafer a = model.addChild("A").refTo(IntType).withCard(3, 3);
        a.addConstraint(equal(joinRef($this()), constant(2)));

        ClaferOptimizer solver = ClaferCompiler.compileMaximize(model, Scope.defaultScope(3), a.getRef());
        assertEquals(6, solver.optimal().getFst().intValue());
    }
}
