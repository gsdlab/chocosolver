package org.clafer;

import org.clafer.ast.AstConcreteClafer;
import org.clafer.ast.AstModel;
import static org.clafer.ast.Asts.*;
import org.clafer.compiler.ClaferCompiler;
import org.clafer.compiler.ClaferSolver;
import org.clafer.scope.Scope;
import static org.junit.Assert.*;
import org.junit.Test;

/**
 *
 * @author jimmy
 */
public class StringTest {

    /**
     * <pre>
     * A -> string 1..2
     * </pre>
     */
    @Test(timeout = 60000)
    public void testStrings() {
        AstModel model = newModel();

        AstConcreteClafer a = model.addChild("A").refTo(StringType).withCard(1, 2);

        ClaferSolver solver = ClaferCompiler.compile(model, Scope.setScope(a, 2));
        assertEquals(10, solver.allInstances().length);
    }
}
