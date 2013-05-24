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
import solver.search.loop.monitors.SearchMonitorFactory;

/**
 *
 * @author jimmy
 */
public class SetArithmeticTest {

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

    /**
     * <pre>
     * Backup 1..2
     * Firewall 1..2
     * [|Backup ++ Firewall| = 3]
     * </pre>
     */
    @Test(timeout = 60000)
    public void testUnionOnClaferType() {
        AstModel model = newModel();

        AstConcreteClafer backup = model.addChild("Backup").withCard(1, 2);
        AstConcreteClafer firewall = model.addChild("Firewall").withCard(1, 2);
        model.addConstraint(equal(card(setUnion(global(backup), global(firewall))), constant(3)));

        ClaferSolver solver = ClaferCompiler.compile(model, Scope.defaultScope(4).toScope());
        assertEquals(2, solver.allInstances().length);
    }

    /**
     * <pre>
     * abstract Feature
     * Backup : Feature 1..2
     * Firewall : Feature *
     * [(Backup ++ Firewall) = Backup]
     * </pre>
     */
    @Test(timeout = 60000)
    public void testEqualityOnSubTypes() {
        AstModel model = newModel();

        AstAbstractClafer feature = model.addAbstractClafer("Feature");
        AstConcreteClafer backup = model.addChild("Backup").extending(feature).withCard(1, 2);
        AstConcreteClafer firewall = model.addChild("Firewall").extending(feature);
        model.addConstraint(equal(setUnion(global(backup), global(firewall)), global(backup)));

        ClaferSolver solver = ClaferCompiler.compile(model, Scope.defaultScope(4).toScope());
        assertEquals(2, solver.allInstances().length);
    }

    /**
     * <pre>
     * Backup 1..2
     * Feature ->> Backup 3..4
     * [Feature.ref = Backup]
     * </pre>
     */
    @Test(timeout = 60000)
    public void testEqualityOnRefs() {
        AstModel model = newModel();

        AstConcreteClafer backup = model.addChild("Backup").withCard(1, 2);
        AstConcreteClafer feature = model.addChild("Feature").withCard(3, 4).refTo(backup);
        model.addConstraint(equal(joinRef(global(feature)), global(backup)));

        ClaferSolver solver = ClaferCompiler.compile(model, Scope.defaultScope(4).toScope());
        // Assuming no reference symmetry breaking.
        assertEquals(22, solver.allInstances().length);
    }
}
