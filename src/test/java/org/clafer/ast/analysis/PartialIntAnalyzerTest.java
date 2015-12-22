package org.clafer.ast.analysis;

import java.util.Arrays;
import org.clafer.ast.AstAbstractClafer;
import org.clafer.ast.AstConcreteClafer;
import org.clafer.ast.AstModel;
import static org.clafer.ast.Asts.*;
import org.clafer.domain.Domain;
import static org.clafer.domain.Domains.*;
import org.clafer.scope.Scopable;
import org.clafer.scope.Scope;
import static org.junit.Assert.*;
import org.junit.Test;

/**
 *
 * @author jimmy
 */
public class PartialIntAnalyzerTest {

    private Analysis analyze(AstModel model, Scopable scope) {
        return Analysis.analyze(model, scope,
                new TypeAnalyzer(),
                new GlobalCardAnalyzer(),
                new ScopeAnalyzer(),
                new CardAnalyzer(),
                new FormatAnalyzer(),
                new AbstractOffsetAnalyzer(),
                new PartialSolutionAnalyzer(),
                PartialIntAnalyzer::analyze);
    }

    /**
     * <pre>
     * abstract Feature
     *     cost -> integer
     * xor A
     *     B : Feature
     *         [this.cost.ref = 3]
     *     C : Feature
     *         [this.cost.ref = 4]
     *     D : Feature
     *         [this.cost.ref = 6]
     * </pre>
     */
    @Test
    public void testKnown() {
        AstModel model = newModel();

        AstAbstractClafer feature = model.addAbstract("Feature");
        AstConcreteClafer cost = feature.addChild("cost").refToUnique(IntType).withCard(Mandatory);
        AstConcreteClafer a = model.addChild("A").withGroupCard(1, 1).withCard(Mandatory);
        AstConcreteClafer b = a.addChild("B").extending(feature).withCard(Optional);
        b.addConstraint(equal(joinRef(join($this(), cost)), constant(3)));
        AstConcreteClafer c = a.addChild("C").extending(feature).withCard(Optional);
        c.addConstraint(equal(joinRef(join($this(), cost)), constant(4)));
        AstConcreteClafer d = a.addChild("D").extending(feature).withCard(Optional);
        d.addConstraint(equal(joinRef(join($this(), cost)), constant(6)));

        Analysis analysis = analyze(model, Scope.defaultScope(3));

        Domain[] partialInts = analysis.getPartialInts(cost.getRef());
        assertNotNull(partialInts);
        assertEquals(3, partialInts.length);
        assertEquals(constantDomain(3), partialInts[analysis.getOffsets(feature).getOffset(b)]);
        assertEquals(constantDomain(4), partialInts[analysis.getOffsets(feature).getOffset(c)]);
        assertEquals(constantDomain(6), partialInts[analysis.getOffsets(feature).getOffset(d)]);
    }

    /**
     * <pre>
     * abstract Feature
     *     cost -> integer
     * xor A
     *     B : Feature
     *         [this.cost.ref = 3]
     *     C : Feature
     *         [this.cost.ref = 4]
     *     D : Feature
     *         [this.cost.ref = 6]
     * E : Feature
     * </pre>
     */
    @Test
    public void testKnownAndUnknown() {
        AstModel model = newModel();

        AstAbstractClafer feature = model.addAbstract("Feature");
        AstConcreteClafer cost = feature.addChild("cost").refToUnique(IntType).withCard(Mandatory);
        AstConcreteClafer a = model.addChild("A").withGroupCard(1, 1).withCard(Mandatory);
        AstConcreteClafer b = a.addChild("B").extending(feature).withCard(Optional);
        b.addConstraint(equal(joinRef(join($this(), cost)), constant(3)));
        AstConcreteClafer c = a.addChild("C").extending(feature).withCard(Optional);
        c.addConstraint(equal(joinRef(join($this(), cost)), constant(4)));
        AstConcreteClafer d = a.addChild("D").extending(feature).withCard(Optional);
        d.addConstraint(equal(joinRef(join($this(), cost)), constant(6)));
        AstConcreteClafer e = model.addChild("E").extending(feature).withCard(Mandatory);

        Analysis analysis = analyze(model, Scope.defaultScope(4).intLow(-3).intHigh(3));

        Domain[] partialInts = analysis.getPartialInts(cost.getRef());
        assertNotNull(partialInts);
        assertEquals(4, partialInts.length);
        assertEquals(constantDomain(3), partialInts[analysis.getOffsets(feature).getOffset(b)]);
        assertEquals(constantDomain(4), partialInts[analysis.getOffsets(feature).getOffset(c)]);
        assertEquals(constantDomain(6), partialInts[analysis.getOffsets(feature).getOffset(d)]);
        assertEquals(boundDomain(-3, 3), partialInts[analysis.getOffsets(feature).getOffset(e)]);
    }

    /**
     * <pre>
     * abstract Feature
     *     cost -> integer 1..2
     * A : Feature
     *     [this.cost.ref = 4]
     * B : Feature ?
     * </pre>
     */
    @Test
    public void testPartiallyKnown() {
        AstModel model = newModel();

        AstAbstractClafer feature = model.addAbstract("Feature");
        AstConcreteClafer cost = feature.addChild("cost").refToUnique(IntType).withCard(1, 2);
        AstConcreteClafer a = model.addChild("A").extending(feature).withCard(Mandatory);
        a.addConstraint(equal(joinRef(join($this(), cost)), constant(4)));
        AstConcreteClafer b = model.addChild("B").extending(feature).withCard(Optional);

        Analysis analysis = analyze(model, Scope.defaultScope(4).intLow(-3).intHigh(3));

        assertTrue(analysis.getOffsets(feature).getOffset(a) < analysis.getOffsets(feature).getOffset(b));
        Domain[] partialInts = analysis.getPartialInts(cost.getRef());
        assertNotNull(partialInts);
        assertEquals(4, partialInts.length);
        assertEquals(constantDomain(4), partialInts[0]);
        assertEquals(boundDomain(-3, 4), partialInts[1]);
        assertEquals(boundDomain(-3, 3), partialInts[2]);
        assertEquals(boundDomain(-3, 3), partialInts[3]);
    }

    /**
     * <pre>
     * A +
     *     B -> int
     *     xor C +
     *         C1 ?
     *             [ this.parent.parent.B.ref = 1 ]
     *         C2 ?
     *             [ B.ref = 2 ]
     * </pre>
     */
    @Test
    public void testConditional() {
        AstModel model = newModel();

        AstConcreteClafer a = model.addChild("A").withCard(Many);
        AstConcreteClafer b = a.addChild("B");
        AstConcreteClafer c = b.addChild("C").refToUnique(IntType).withCard(Mandatory);
        AstConcreteClafer d = a.addChild("D").withGroupCard(1, 1).withCard(Many);
        AstConcreteClafer d1 = d.addChild("D1").withCard(Optional);
        AstConcreteClafer d2 = d.addChild("D2").withCard(Optional);
        d1.addConstraint(equal(joinRef(join(join(joinParent(joinParent($this())), b), c)), constant(1)));
        d2.addConstraint(equal(joinRef(c), constant(2)));

        Analysis analysis = analyze(model, Scope.defaultScope(3).intLow(-3).intHigh(3));
        assertArrayEquals(
                new Domain[]{boundDomain(1, 2), boundDomain(1, 2), boundDomain(1, 2)},
                analysis.getPartialInts(c.getRef()));
    }
}
