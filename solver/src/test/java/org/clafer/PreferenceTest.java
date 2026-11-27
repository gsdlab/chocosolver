package org.clafer;

import org.clafer.ast.AstConcreteClafer;
import org.clafer.ast.AstModel;
import static org.clafer.ast.Asts.newModel;
import org.clafer.compiler.ClaferCompiler;
import org.clafer.compiler.ClaferOption;
import org.clafer.compiler.ClaferSearchStrategy;
import org.clafer.compiler.ClaferSolver;
import org.clafer.instance.InstanceModel;
import org.clafer.scope.Scope;
import static org.junit.Assert.assertEquals;
import org.junit.Test;

/**
 *
 * @author jimmy
 */
public class PreferenceTest {

    @Test(timeout = 60000)
    public void testPreferSmallerInstances() {
        AstModel model = newModel();

        AstConcreteClafer a = model.addChild("A");

        ClaferSolver solver = ClaferCompiler.compile(model, Scope.defaultScope(3), ClaferOption.Default.setStrategy(ClaferSearchStrategy.PreferSmallerInstances));
        int i = 0;
        while (solver.find()) {
            InstanceModel instance = solver.instance();
            assertEquals(i++, instance.getTopClafers(a).length);
        }
        assertEquals(4, solver.instanceCount());
    }

    @Test(timeout = 60000)
    public void testPreferLargerInstances() {
        AstModel model = newModel();

        AstConcreteClafer a = model.addChild("A");

        ClaferSolver solver = ClaferCompiler.compile(model, Scope.defaultScope(3), ClaferOption.Default.setStrategy(ClaferSearchStrategy.PreferLargerInstances));
        int i = 3;
        while (solver.find()) {
            InstanceModel instance = solver.instance();
            assertEquals(i--, instance.getTopClafers(a).length);
        }
        assertEquals(4, solver.instanceCount());
    }
}
