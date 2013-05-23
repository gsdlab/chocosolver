package org.clafer;

import org.clafer.ast.AstAbstractClafer;
import org.clafer.ast.AstConcreteClafer;
import org.clafer.ast.AstLocal;
import org.clafer.ast.AstModel;
import static org.clafer.ast.Asts.*;
import org.clafer.ast.scope.Scope;
import org.clafer.compiler.ClaferCompiler;
import org.clafer.compiler.ClaferSolver;
import static org.junit.Assert.assertEquals;
import org.junit.Test;

/**
 *
 * @author jimmy
 */
public class QuantifierTest {

    /**
     * <pre>
     * abstract Feature
     *     Cost ->> integer
     * Backup : Feature 2..3
     * Firewall : Feature ?
     * Guard : Feature ?
     * [some a : Backup ++ Firewall | a.Cost.ref = 2]
     * </pre>
     */
    @Test
    public void testSome() {
        /*
         * import Control.Monad
         *
         * solutions = do
         *     backupCard <- [2..3]
         *     backup <- sequence $ replicate backupCard [-1..1]
         *     
         *     firewallCard <- [0, 1]
         *     firewall <- sequence $ replicate firewallCard [-1..1]
         *     
         *     guarddCard <- [0, 1]
         *     guardd <- sequence $ replicate guarddCard [-1..1]
         *     
         *     guard $ any (== 1) (backup ++ firewall)
         *     
         *     return (backup, firewall)
         */
        AstModel model = newModel();

        AstAbstractClafer feature = model.addAbstractClafer("Feature");
        AstConcreteClafer cost = feature.addChild("Cost").withCard(1, 1).refTo(IntType);
        AstConcreteClafer backup = model.addChild("Backup").withCard(2, 3).extending(feature);
        AstConcreteClafer firewall = model.addChild("Firewall").withCard(0, 1).extending(feature);
        AstConcreteClafer guard = model.addChild("Guard").withCard(0, 1).extending(feature);
        AstLocal x = local("x");
        model.addConstraint(some(decl(x, setUnion(global(backup), global(firewall))),
                equal(joinRef(join(x, cost)), constant(1))));

        ClaferSolver solver = ClaferCompiler.compile(model, Scope.defaultScope(5).intLow(-1).intHigh(1).toScope());
        System.out.println(solver.solver);
        assertEquals(432, solver.allInstances().length);
    }
}
