package org.clafer;

import org.clafer.ast.AstAbstractClafer;
import org.clafer.ast.AstConcreteClafer;
import org.clafer.ast.AstModel;
import static org.clafer.ast.Asts.*;
import static org.clafer.ast.Asts.newModel;
import org.clafer.ast.scope.Scope;
import org.clafer.compiler.ClaferCompiler;
import org.clafer.compiler.ClaferSolver;
import static org.junit.Assert.assertEquals;
import org.junit.Test;

/**
 *
 * @author jimmy
 */
public class SetArithmeticModelTest {

    /**
     * <pre>
     * abstract Feature
     *     Cost ->> integer
     * Backup : Feature 1..2
     * Firewall : Feature 1..2
     * [(Backup ++ Firewall).Cost.ref = 4]
     * </pre>
     */
    @Test(timeout = 60000)
    public void testUnion() {
        AstModel model = newModel();
        
        AstAbstractClafer feature = model.addAbstractClafer("Feature");
        AstConcreteClafer cost = feature.addChild("Cost").withCard(1, 1).refTo(IntType);
        AstConcreteClafer backup = model.addChild("Backup").extending(feature).withCard(1, 2);
        AstConcreteClafer firewall = model.addChild("Firewall").extending(feature).withCard(1, 2);
        model.addConstraint(equal(joinRef(join(setUnion(global(backup), global(firewall)), cost)), constant(4)));
        
        ClaferSolver solver = ClaferCompiler.compile(model, Scope.defaultScope(4).toScope());
        assertEquals(4, solver.allInstances().length);
    }
}
