package org.clafer.ast.analysis;

import org.clafer.ast.AstAbstractClafer;
import org.clafer.ast.AstConcreteClafer;
import org.clafer.ast.AstModel;
import static org.clafer.ast.Asts.$this;
import static org.clafer.ast.Asts.IntType;
import static org.clafer.ast.Asts.Mandatory;
import static org.clafer.ast.Asts.Many;
import static org.clafer.ast.Asts.Optional;
import static org.clafer.ast.Asts.constant;
import static org.clafer.ast.Asts.equal;
import static org.clafer.ast.Asts.join;
import static org.clafer.ast.Asts.joinParent;
import static org.clafer.ast.Asts.joinRef;
import static org.clafer.ast.Asts.newModel;
import static org.clafer.ast.Asts.or;
import org.clafer.domain.Domain;
import org.clafer.domain.Domains;
import static org.clafer.domain.Domains.ZeroOneDomain;
import static org.clafer.domain.Domains.boundDomain;
import static org.clafer.domain.Domains.constantDomain;
import static org.clafer.domain.Domains.enumDomain;
import org.clafer.scope.Scopable;
import org.clafer.scope.Scope;
import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import org.junit.Ignore;
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
     * Feature *
     *     cost -> integer *
     *     [ this.cost = 3 ]
     * </pre>
     */
    @Test
    public void testSingleConcrete() {
        AstModel model = newModel();

        AstConcreteClafer feature = model.addChild("Feature");
        AstConcreteClafer cost = feature.addChild("cost").refToUnique(IntType);
        feature.addConstraint(equal(joinRef(join($this(), cost)), constant(3)));

        Analysis analysis = analyze(model, Scope.defaultScope(3));

        Domain[] partialInts = analysis.getPartialInts(cost);
        assertArrayEquals(new Domain[]{constantDomain(3), constantDomain(3), constantDomain(3)}, partialInts);
    }

    /**
     * <pre>
     * Feature *
     *     cost -> integer *
     * Expensive
     *     [ cost . ref = 3 ]
     * </pre>
     */
    @Test
    public void testGlobal() {
        AstModel model = newModel();

        AstConcreteClafer feature = model.addChild("Feature");
        AstConcreteClafer cost = feature.addChild("cost").refToUnique(IntType);
        AstConcreteClafer expensive = model.addChild("Expensive").withCard(Mandatory);
        expensive.addConstraint(equal(joinRef(cost), constant(3)));

        Analysis analysis = analyze(model, Scope.defaultScope(3));

        Domain[] partialInts = analysis.getPartialInts(cost);
        assertArrayEquals(new Domain[]{constantDomain(3), constantDomain(3), constantDomain(3)}, partialInts);
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

        Domain[] partialInts = analysis.getPartialInts(cost);
        assertNotNull(partialInts);
        assertEquals(3, partialInts.length);
        assertEquals(constantDomain(3), partialInts[analysis.getOffsets(feature).getOffset(b)]);
        assertEquals(constantDomain(4), partialInts[analysis.getOffsets(feature).getOffset(c)]);
        assertEquals(constantDomain(6), partialInts[analysis.getOffsets(feature).getOffset(d)]);
    }

    /**
     * <pre>
     * abstract Feature
     *     cost -> integer +
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
    public void testKnownMany() {
        AstModel model = newModel();

        AstAbstractClafer feature = model.addAbstract("Feature");
        AstConcreteClafer cost = feature.addChild("cost").refToUnique(IntType).withCard(Many);
        AstConcreteClafer a = model.addChild("A").withGroupCard(1, 1).withCard(Mandatory);
        AstConcreteClafer b = a.addChild("B").extending(feature).withCard(Optional);
        b.addConstraint(equal(joinRef(join($this(), cost)), constant(3)));
        AstConcreteClafer c = a.addChild("C").extending(feature).withCard(Optional);
        c.addConstraint(equal(joinRef(join($this(), cost)), constant(4)));
        AstConcreteClafer d = a.addChild("D").extending(feature).withCard(Optional);
        d.addConstraint(equal(joinRef(join($this(), cost)), constant(6)));

        Analysis analysis = analyze(model, Scope.defaultScope(3));

        Domain[] partialInts = analysis.getPartialInts(cost);
        assertNotNull(partialInts);
        assertEquals(3, partialInts.length);
        assertEquals(enumDomain(3, 4, 6), partialInts[analysis.getOffsets(feature).getOffset(b)]);
        assertEquals(enumDomain(3, 4, 6), partialInts[analysis.getOffsets(feature).getOffset(c)]);
        assertEquals(enumDomain(3, 4, 6), partialInts[analysis.getOffsets(feature).getOffset(d)]);
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

        Domain[] partialInts = analysis.getPartialInts(cost);
        assertNotNull(partialInts);
        assertEquals(4, partialInts.length);
        assertEquals(constantDomain(3), partialInts[analysis.getOffsets(feature).getOffset(b)]);
        assertEquals(constantDomain(4), partialInts[analysis.getOffsets(feature).getOffset(c)]);
        assertEquals(constantDomain(6), partialInts[analysis.getOffsets(feature).getOffset(d)]);
        assertNull(partialInts[analysis.getOffsets(feature).getOffset(e)]);
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
        Domain[] partialInts = analysis.getPartialInts(cost);
        assertNotNull(partialInts);
        assertEquals(4, partialInts.length);
        assertEquals(constantDomain(4), partialInts[0]);
        assertNull(partialInts[1]);
        assertNull(partialInts[2]);
        assertNull(partialInts[3]);
    }

    /**
     * <pre>
     * abstract Feature
     *     cost -> integer
     * A : Feature ?
     *     [ this.cost.ref = this.B.cost.ref ]
     *     B : Feature ?
     *         [ this.cost = 2 ]
     * </pre>
     */
    @Test
    public void testEquality() {
        AstModel model = newModel();

        AstAbstractClafer feature = model.addAbstract("Feature");
        AstConcreteClafer cost = feature.addChild("cost").refToUnique(IntType).withCard(Mandatory);
        AstConcreteClafer a = model.addChild("A").extending(feature).withCard(Optional);
        AstConcreteClafer b = a.addChild("B").extending(feature).withCard(Optional);
        a.addConstraint(equal(
                joinRef(join($this(), cost)),
                joinRef(join(join($this(), b), cost))));
        b.addConstraint(equal(joinRef(join($this(), cost)), constant(2)));

        Analysis analysis = analyze(model, Scope.defaultScope(4).intLow(-3).intHigh(3));

        Domain[] partialInts = analysis.getPartialInts(cost);
        assertArrayEquals(new Domain[]{constantDomain(2), constantDomain(2)}, partialInts);
    }

    /**
     * <pre>
     * A +
     *     B
     *         C -> int
     *     xor D +
     *         D1 ?
     *             [ this.parent.parent.B.C.ref = 1 ]
     *         D2 ?
     *             [ B.ref = 2 ]
     * </pre>
     */
    @Ignore
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
                analysis.getPartialInts(c));
    }

    /**
     * <pre>
     * A -> int
     * B -> int
     * [ A = 0 | A = 1 ]
     * </pre>
     */
    @Test
    public void testOrKnown() {
        AstModel model = newModel();

        AstConcreteClafer a = model.addChild("A").refToUnique(IntType).withCard(Mandatory);
        AstConcreteClafer b = model.addChild("B").refToUnique(IntType).withCard(Mandatory);
        model.addConstraint(or(equal(joinRef(a), constant(0)), equal(joinRef(a), constant(1))));

        Analysis analysis = analyze(model, Scope.defaultScope(1).intLow(-2).intHigh(2));

        assertArrayEquals(new Domain[]{ZeroOneDomain}, analysis.getPartialInts(a));
        assertArrayEquals(new Domain[]{null}, analysis.getPartialInts(b));
    }

    /**
     * <pre>
     * A -> int
     * B -> int
     * [ A = 1 | B = 1 ]
     * </pre>
     */
    @Test
    public void testOrUnknown() {
        AstModel model = newModel();

        AstConcreteClafer a = model.addChild("A").refToUnique(IntType).withCard(Mandatory);
        AstConcreteClafer b = model.addChild("B").refToUnique(IntType).withCard(Mandatory);
        model.addConstraint(or(equal(joinRef(a), constant(1)), equal(joinRef(b), constant(1))));

        Analysis analysis = analyze(model, Scope.defaultScope(1).intLow(-2).intHigh(2));

        assertArrayEquals(new Domain[]{null}, analysis.getPartialInts(a));
        assertArrayEquals(new Domain[]{null}, analysis.getPartialInts(b));
    }

    /**
     * <pre>
     * abstract A
     *     abstract B -> int
     *
     * X : A
     *     Y : B
     *         [ this.dref = 4 ]
     *     Z : B
     *         [ this.dref = 3 ]
     * </pre>
     */
    @Test
    public void testRefRefinement() {
        AstModel model = newModel();

        AstAbstractClafer a = model.addAbstract("A");
        AstAbstractClafer b = a.addAbstractChild("B").refToUnique(IntType);

        AstConcreteClafer x = model.addChild("X").extending(a).withCard(Mandatory);
        AstConcreteClafer y = x.addChild("Y").extending(b).withCard(Mandatory);
        y.addConstraint(equal(joinRef($this()), 4));
        AstConcreteClafer z = x.addChild("Z").extending(b).withCard(Mandatory);
        z.addConstraint(equal(joinRef($this()), 3));

        Analysis analysis = analyze(model, Scope.defaultScope(1).intLow(-2).intHigh(2));

        assertArrayEquals(new Domain[]{Domains.constantDomain(4)}, analysis.getPartialInts(y));
        assertArrayEquals(new Domain[]{Domains.constantDomain(3)}, analysis.getPartialInts(z));
    }
}
