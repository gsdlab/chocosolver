package org.clafer.ast.analysis;

import gnu.trove.set.TIntSet;
import gnu.trove.set.hash.TIntHashSet;
import org.clafer.ast.AstAbstractClafer;
import org.clafer.ast.AstConcreteClafer;
import org.clafer.ast.AstModel;
import static org.clafer.ast.Asts.*;
import org.clafer.domain.Domain;
import org.clafer.domain.Domains;
import org.clafer.scope.Scopable;
import org.clafer.scope.Scope;
import static org.junit.Assert.*;
import org.junit.Test;

/**
 *
 * @author jimmy
 */
public class PartialIntAnalyzerTest {

    private static TIntSet set(int... values) {
        return new TIntHashSet(values);
    }

    private Analysis analyze(AstModel model, Scopable scope) {
        return Analysis.analyze(model, scope,
                new GlobalCardAnalyzer(),
                new ScopeAnalyzer(),
                new CardAnalyzer(),
                new FormatAnalyzer(),
                new AbstractOffsetAnalyzer(),
                new PartialSolutionAnalyzer(),
                new PartialIntAnalyzer());
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
        assertEquals(Domains.constantDomain(3), partialInts[analysis.getOffsets(feature).getOffset(b)]);
        assertEquals(Domains.constantDomain(4), partialInts[analysis.getOffsets(feature).getOffset(c)]);
        assertEquals(Domains.constantDomain(6), partialInts[analysis.getOffsets(feature).getOffset(d)]);
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
        assertEquals(Domains.constantDomain(3), partialInts[analysis.getOffsets(feature).getOffset(b)]);
        assertEquals(Domains.constantDomain(4), partialInts[analysis.getOffsets(feature).getOffset(c)]);
        assertEquals(Domains.constantDomain(6), partialInts[analysis.getOffsets(feature).getOffset(d)]);
        assertEquals(Domains.boundDomain(-3, 3), partialInts[analysis.getOffsets(feature).getOffset(e)]);
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
        assertEquals(Domains.constantDomain(4), partialInts[0]);
        assertEquals(Domains.boundDomain(-3, 4), partialInts[1]);
        assertEquals(Domains.boundDomain(-3, 3), partialInts[2]);
        assertEquals(Domains.boundDomain(-3, 3), partialInts[3]);
    }
}
