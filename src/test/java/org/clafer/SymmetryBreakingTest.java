package org.clafer;

import org.clafer.ast.AstConcreteClafer;
import org.clafer.ast.AstModel;
import org.clafer.ast.Asts;
import org.clafer.compiler.ClaferCompiler;
import org.clafer.compiler.ClaferSolver;
import static org.junit.Assert.*;
import org.junit.Test;

/**
 *
 * @author jimmy
 */
public class SymmetryBreakingTest {

    @Test
    public void breakChildrenSwap() {
        AstModel model = Asts.newModel();

        AstConcreteClafer patron = model.addTopClafer("Patron").withCard(2, 2);
        AstConcreteClafer food = patron.addChild("Food").withCard(1, 2);
        AstConcreteClafer drink = patron.addChild("Drink").withCard(1, 2);

        ClaferSolver solver = ClaferCompiler.compile(model, new Scope(3));

        assertEquals(5, solver.allInstances().length);
    }
}
