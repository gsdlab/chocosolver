package org.clafer;

import org.clafer.ast.AstConcreteClafer;
import org.clafer.ast.AstModel;
import static org.clafer.ast.Asts.IntType;
import static org.clafer.ast.Asts.Mandatory;
import static org.clafer.ast.Asts.add;
import static org.clafer.ast.Asts.constant;
import static org.clafer.ast.Asts.equal;
import static org.clafer.ast.Asts.greaterThanEqual;
import static org.clafer.ast.Asts.joinRef;
import static org.clafer.ast.Asts.mul;
import static org.clafer.ast.Asts.newModel;
import org.clafer.compiler.ClaferCompiler;
import org.clafer.compiler.ClaferOptimizer;
import org.clafer.objective.Objective;
import org.clafer.scope.Scope;
import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertTrue;
import org.junit.Test;

/**
 *
 * @author jimmy
 */
public class LinearProgramTest {

    @Test(timeout = 60000)
    public void testAlmostColinear() throws Exception {
        AstModel m = newModel();
        AstConcreteClafer a = m.addChild("a").refToUnique(IntType).withCard(Mandatory);
        AstConcreteClafer b = m.addChild("b").refToUnique(IntType).withCard(Mandatory);
        AstConcreteClafer c = m.addChild("c").refToUnique(IntType).withCard(Mandatory);
        AstConcreteClafer d = m.addChild("d").refToUnique(IntType).withCard(Mandatory);
        m.addConstraint(equal(
                add(mul(100, joinRef(a)), mul(99, joinRef(c)), mul(-101, joinRef(d))),
                197));
        m.addConstraint(equal(
                add(mul(101, joinRef(a)), mul(99, joinRef(b)), mul(199, joinRef(c)), mul(-100, joinRef(d))),
                302));
        ClaferOptimizer s = ClaferCompiler.compile(m,
                Scope.defaultScope(1).intLow(0).intHigh(4).mulLow(-200).mulHigh(200),
                Objective.minimize(joinRef(d)));
        assertTrue(s.find());
        assertArrayEquals(new int[]{2}, s.optimalValues());
    }

    @Test(timeout = 60000)
    public void testLargeCoefficients() throws Exception {
        AstModel m = newModel();
        AstConcreteClafer a = m.addChild("a").refToUnique(IntType).withCard(Mandatory);
        AstConcreteClafer b = m.addChild("b").refToUnique(IntType).withCard(Mandatory);
        AstConcreteClafer c = m.addChild("c").refToUnique(IntType).withCard(Mandatory);
        m.addConstraint(equal(
                add(mul(12345, joinRef(a)), mul(23456, joinRef(b)), mul(-54321, joinRef(c))),
                40737));
        ClaferOptimizer s = ClaferCompiler.compile(m,
                Scope.defaultScope(1).intLow(0).intHigh(4).mulLow(-250000).mulHigh(250000),
                Objective.minimize(joinRef(c)));
        assertTrue(s.find());
        assertArrayEquals(new int[]{1}, s.optimalValues());
    }

    @Test(timeout = 60000)
    public void testCommonSubexpression() {
        AstModel m = newModel();
        AstConcreteClafer a = m.addChild("a").refToUnique(IntType).withCard(Mandatory);
        AstConcreteClafer b = m.addChild("b").refToUnique(IntType).withCard(Mandatory);
        AstConcreteClafer c = m.addChild("c").refToUnique(IntType).withCard(Mandatory);
        AstConcreteClafer d = m.addChild("d").refToUnique(IntType).withCard(Mandatory);
        AstConcreteClafer e = m.addChild("e").refToUnique(IntType).withCard(Mandatory);
        AstConcreteClafer f = m.addChild("f").refToUnique(IntType).withCard(Mandatory);
        AstConcreteClafer g = m.addChild("g").refToUnique(IntType).withCard(Mandatory);
        AstConcreteClafer h = m.addChild("h").refToUnique(IntType).withCard(Mandatory);
        AstConcreteClafer i = m.addChild("i").refToUnique(IntType).withCard(Mandatory);
        AstConcreteClafer j = m.addChild("j").refToUnique(IntType).withCard(Mandatory);
        AstConcreteClafer k = m.addChild("k").refToUnique(IntType).withCard(Mandatory);
        AstConcreteClafer l = m.addChild("l").refToUnique(IntType).withCard(Mandatory);
        AstConcreteClafer z = m.addChild("z").refToUnique(IntType).withCard(Mandatory);
        m.addConstraint(greaterThanEqual(add(joinRef(a), joinRef(b)), constant(4)));
        m.addConstraint(greaterThanEqual(add(joinRef(c), joinRef(d)), constant(4)));
        m.addConstraint(greaterThanEqual(add(joinRef(e), joinRef(f)), constant(4)));
        m.addConstraint(greaterThanEqual(add(joinRef(g), joinRef(h)), constant(4)));
        m.addConstraint(greaterThanEqual(add(joinRef(i), joinRef(j)), constant(4)));
        m.addConstraint(greaterThanEqual(add(joinRef(k), joinRef(l)), constant(4)));
        m.addConstraint(equal(add(
                joinRef(a), joinRef(b), joinRef(c), joinRef(d), joinRef(e), joinRef(f),
                joinRef(g), joinRef(h), joinRef(i), joinRef(j), joinRef(k), joinRef(l)),
                joinRef(z)));

        ClaferOptimizer s = ClaferCompiler.compile(m, Scope.defaultScope(1).intLow(-32).intHigh(32),
                Objective.minimize(joinRef(z)));
        assertTrue(s.find());
        assertArrayEquals(new int[]{24}, s.optimalValues());
    }
}
