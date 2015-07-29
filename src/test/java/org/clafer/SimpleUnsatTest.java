package org.clafer;

import java.util.Arrays;
import java.util.HashSet;
import java.util.Set;
import org.clafer.ast.AstConcreteClafer;
import org.clafer.ast.AstConstraint;
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
public class SimpleUnsatTest {

    private static Set<AstConstraint> set(AstConstraint... items) {
        return new HashSet<>(Arrays.asList(items));
    }

    /**
     * <pre>
     * A ?
     * [#A = 2]
     * [#A = 1]
     * </pre>
     */
    @Test(timeout = 60000)
    public void testUnsatSolutionKnown() {
        AstModel model = newModel();

        AstConcreteClafer a = model.addChild("A").withCard(Optional);
        AstConstraint bad = model.addConstraint(equal(card(global(a)), constant(2)));
        AstConstraint good = model.addConstraint(equal(card(global(a)), constant(1)));

        assertEquals(set(bad), ClaferCompiler.compileUnsat(model, Scope.defaultScope(2)).minUnsat().getFst());
        assertEquals(set(bad), ClaferCompiler.compileUnsat(model, Scope.defaultScope(2)).unsatCore());
    }

    /**
     * <pre>
     * Mob ?
     * Duck ?
     * Witch ?
     * Floats ?
     * [Floats => Duck]
     * [Duck &lt;=&gt; Witch]
     * [!Duck]
     * [Witch]
     * [Floats]
     * [Mob]
     * </pre>
     */
    @Test(timeout = 60000)
    public void testWitchsFloatDucksFloat() {
        AstModel model = newModel();

        AstConcreteClafer mob = model.addChild("Mob").withCard(0, 1);
        AstConcreteClafer duck = model.addChild("Duck").withCard(0, 1);
        AstConcreteClafer witch = model.addChild("Witch").withCard(0, 1);
        AstConcreteClafer floats = model.addChild("Floats").withCard(0, 1);
        AstConstraint c1 = model.addConstraint(implies(some(floats), some(duck)));
        AstConstraint c2 = model.addConstraint(ifOnlyIf(some(duck), some(witch)));
        AstConstraint c3 = model.addConstraint(none(duck));
        AstConstraint c4 = model.addConstraint(some(witch));
        AstConstraint c5 = model.addConstraint(some(floats));
        AstConstraint c6 = model.addConstraint(some(mob));

        assertEquals(set(c3), ClaferCompiler.compileUnsat(model, Scope.defaultScope(1))
                .minUnsat().getFst());
        assertEquals(set(c1, c2, c3, c4, c5), ClaferCompiler.compileUnsat(model, Scope.defaultScope(1))
                .unsatCore());
    }

    /**
     * <pre>
     * A -> integer ?
     * B ?
     * C ?
     * [A.ref = 2]
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
        AstConstraint c1 = model.addConstraint(equal(joinRef(a), constant(2)));
        AstConstraint c2 = model.addConstraint(and(some(a), none(b)));
        AstConstraint c3 = model.addConstraint(and(some(a), none(c)));
        AstConstraint c4 = model.addConstraint(none(a));

        ClaferUnsat unsat = ClaferCompiler.compileUnsat(model, Scope.defaultScope(1));
        Pair<Set<AstConstraint>, InstanceModel> unsatInstance = unsat.minUnsat();

        assertEquals(1, unsatInstance.getFst().size());
        assertEquals(c4, unsatInstance.getFst().iterator().next());
        assertEquals(1, unsatInstance.getSnd().getTopClafers().length);
        assertEquals(a, unsatInstance.getSnd().getTopClafer(a).getType());
        assertEquals(0, unsatInstance.getSnd().getTopClafer(a).getId());
        assertEquals(2, unsatInstance.getSnd().getTopClafer(a).getRef());

        assertEquals(set(c1, c2, c3, c4),
                ClaferCompiler.compileUnsat(model, Scope.defaultScope(1)).unsatCore());
    }

    /**
     * <pre>
     * A
     * B -> A 1..2
     * [one B]
     * </pre>
     */
    @Test(timeout = 60000)
    public void testSat() {
        AstModel model = newModel();

        AstConcreteClafer a = model.addChild("A").withCard(Mandatory);
        AstConcreteClafer b = model.addChild("B").refToUnique(a).withCard(1, 2);
        model.addConstraint(one(b));

        assertEquals(set(), ClaferCompiler.compileUnsat(model, Scope.defaultScope(2)).minUnsat().getFst());
        assertEquals(set(), ClaferCompiler.compileUnsat(model, Scope.defaultScope(2)).unsatCore());
    }
}
