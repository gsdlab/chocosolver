package org.clafer;

import java.util.Arrays;
import java.util.HashSet;
import java.util.Set;
import org.clafer.assertion.Assertion;
import org.clafer.ast.AstConcreteClafer;
import org.clafer.ast.AstModel;
import static org.clafer.ast.Asts.IntType;
import static org.clafer.ast.Asts.Mandatory;
import static org.clafer.ast.Asts.constant;
import static org.clafer.ast.Asts.equal;
import static org.clafer.ast.Asts.greaterThan;
import static org.clafer.ast.Asts.joinRef;
import static org.clafer.ast.Asts.lessThan;
import static org.clafer.ast.Asts.newModel;
import org.clafer.compiler.ClaferAsserter;
import org.clafer.compiler.ClaferCompiler;
import org.clafer.instance.InstanceModel;
import org.clafer.scope.Scope;
import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import org.junit.Test;

/**
 *
 * @author jimmy
 */
public class SimpleAssertionTest {

    /**
     * <pre>
     * Age -> integer
     * [Age.ref = 3]
     * </pre>
     */
    @Test(timeout = 60000)
    public void testTautologyAssertion() {
        AstModel model = newModel();

        AstConcreteClafer age = model.addChild("Age").withCard(Mandatory).refTo(IntType);
        model.addConstraint(equal(joinRef(age), constant(3)));

        Assertion assertion = new Assertion(equal(joinRef(age), constant(3)));
        ClaferAsserter solver = ClaferCompiler.compile(model, Scope.defaultScope(2), assertion);
        assertFalse(solver.find());
    }

    /**
     * <pre>
     * Age -> integer
     * [Age.ref = 3]
     * </pre>
     */
    @Test(timeout = 60000)
    public void testFalseTautologyAssertion() {
        AstModel model = newModel();

        AstConcreteClafer age = model.addChild("Age").withCard(Mandatory).refTo(IntType);
        model.addConstraint(equal(joinRef(age), constant(3)));

        Assertion assertion = new Assertion(equal(joinRef(age), constant(2)));
        ClaferAsserter solver = ClaferCompiler.compile(model, Scope.defaultScope(2), assertion);
        assertTrue(solver.find());
        assertArrayEquals(new Assertion[]{assertion}, solver.failedAssertions());
        assertFalse(solver.find());
    }

    /**
     * <pre>
     * Age -> integer
     * </pre>
     */
    @Test(timeout = 60000)
    public void testMultipleAssertions() {
        AstModel model = newModel();

        AstConcreteClafer age = model.addChild("Age").withCard(Mandatory).refTo(IntType);

        Assertion assertionLessThan1 = new Assertion(lessThan(joinRef(age), constant(1)));
        Assertion assertionLessThan2 = new Assertion(lessThan(joinRef(age), constant(2)));
        Assertion assertionGreaterThan1 = new Assertion(greaterThan(joinRef(age), constant(1)));
        Assertion assertionGreaterThan2 = new Assertion(greaterThan(joinRef(age), constant(2)));
        ClaferAsserter solver = ClaferCompiler.compile(model, Scope.defaultScope(2), assertionLessThan1, assertionLessThan2, assertionGreaterThan1, assertionGreaterThan2);

        Set<Assertion> assertions = new HashSet<>(Arrays.asList(assertionLessThan1, assertionLessThan2, assertionGreaterThan1, assertionGreaterThan2));
        while (!assertions.isEmpty()) {
            assertTrue(solver.find());
            InstanceModel instance = solver.instance();
            boolean removed = false;
            for (Assertion failedAssertion : solver.failedAssertions()) {
                removed |= assertions.remove(failedAssertion);
                if (failedAssertion.equals(assertionLessThan1)) {
                    assertFalse(((Integer) instance.getTopClafer(age).getRef()) < 1);
                } else if (failedAssertion.equals(assertionLessThan2)) {
                    assertFalse(((Integer) instance.getTopClafer(age).getRef()) < 2);
                } else if (failedAssertion.equals(assertionGreaterThan1)) {
                    assertFalse(((Integer) instance.getTopClafer(age).getRef()) > 1);
                } else {
                    assert failedAssertion.equals(assertionGreaterThan2);
                    assertFalse(((Integer) instance.getTopClafer(age).getRef()) > 2);
                }
            }
            assertTrue(removed);
        }
        assertFalse(solver.find());
    }
}
