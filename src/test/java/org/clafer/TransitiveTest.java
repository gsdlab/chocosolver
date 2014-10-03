package org.clafer;

import org.clafer.ast.AstConcreteClafer;
import org.clafer.ast.AstModel;
import static org.clafer.ast.Asts.*;
import org.clafer.compiler.ClaferCompiler;
import org.clafer.compiler.ClaferSolver;
import org.clafer.scope.Scope;
import static org.junit.Assert.assertEquals;
import org.junit.Test;

/**
 *
 * @author jimmy
 */
public class TransitiveTest {

    /**
     * <pre>
     * Person *
     *     Child -> Person *
     *     [ this not in this.(Person -> Child . Child -> ref)* ]
     * </pre>
     */
    @Test(timeout = 60000)
    public void testAcyclic() {
        AstModel model = newModel();

        AstConcreteClafer person = model.addChild("Person");
        AstConcreteClafer child = person.addChild("Child").refToUnique(person);
        person.addConstraint(notIn($this(), join($this(), transitiveClosure(join(relation(child), ref(child))))));

        ClaferSolver solver = ClaferCompiler.compile(model, Scope.defaultScope(2));
        assertEquals(4, solver.allInstances().length);
    }
}
