package org.clafer;

import java.util.Set;
import org.clafer.ast.AstConcreteClafer;
import org.clafer.ast.AstModel;
import static org.clafer.ast.Asts.*;
import org.clafer.collection.Pair;
import org.clafer.compiler.ClaferCompiler;
import org.clafer.compiler.ClaferUnsat;
import org.clafer.instance.InstanceModel;
import org.clafer.scope.Scope;
import static org.junit.Assert.assertEquals;
import org.junit.Test;

/**
 *
 * @author jimmy
 */
public class UnsatTest {

    /**
     * <pre>
     * Mob
     * Duck ?
     * Witch ?
     * Floats ?
     * [Floats => Duck]
     * [Floats &lt;=&gt; Witch]
     * [!Duck]
     * [Witch]
     * </pre>
     */
    @Test(timeout = 60000)
    public void testWitchsFloatDucksFloat() {
        AstModel model = newModel();

        AstConcreteClafer mob = model.addChild("Mob").withCard(1, 1);
        AstConcreteClafer duck = model.addChild("Duck").withCard(0, 1);
        AstConcreteClafer witch = model.addChild("Witch").withCard(0, 1);
        AstConcreteClafer floats = model.addChild("Floats").withCard(0, 1);
        model.addConstraint(implies(some(floats), some(duck)));
        model.addConstraint(ifOnlyIf(some(floats), some(witch)));
        model.addConstraint(none(duck));
        model.addConstraint(some(witch));

        ClaferUnsat unsat = ClaferCompiler.compileUnsat(model, Scope.defaultScope(1));

        assertEquals(1, unsat.minUnsat().getFst().size());
    }

    /**
     * <pre>
     * A ?
     * B ?
     * C ?
     * [A & !B]
     * [A & !C]
     * [!A]
     * </pre>
     */
    @Test(timeout = 60000)
    public void testOneUnsat() {
        AstModel model = newModel();

        AstConcreteClafer a = model.addChild("A").withCard(0, 1).refTo(IntType);
        AstConcreteClafer b = model.addChild("B").withCard(0, 1);
        AstConcreteClafer c = model.addChild("C").withCard(0, 1);
        model.addConstraint(equal(joinRef(global(a)), constant(2)));
        model.addConstraint(and(some(a), none(b)));
        model.addConstraint(and(some(a), none(c)));
        model.addConstraint(none(a));

        ClaferUnsat unsat = ClaferCompiler.compileUnsat(model, Scope.defaultScope(1));
        Pair<Set<String>, InstanceModel> unsatInstance = unsat.minUnsat();

        assertEquals(1, unsatInstance.getFst().size());
        assertEquals(1, unsatInstance.getSnd().getTopClafers().length);
        assertEquals(a, unsatInstance.getSnd().getTopClafers()[0].getType());
        assertEquals(0, unsatInstance.getSnd().getTopClafers()[0].getId());
        assertEquals(2, unsatInstance.getSnd().getTopClafers()[0].getRef());
    }
}
