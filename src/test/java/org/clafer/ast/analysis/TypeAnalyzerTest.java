package org.clafer.ast.analysis;

import org.clafer.ast.AstAbstractClafer;
import org.clafer.ast.AstConcreteClafer;
import org.clafer.ast.AstModel;
import static org.clafer.ast.Asts.*;
import org.clafer.scope.Scope;
import org.junit.Test;

/**
 *
 * @author jimmy
 */
public class TypeAnalyzerTest {

    /**
     * <pre>
     * abstract A
     *     a -> int
     * abstract B : A
     *     b -> int
     * C : B
     *     c -> int
     * D : B ?
     *     d -> int
     *
     * [(C ++ B).d.ref = 1]
     * </pre>
     */
    @Test(expected = TypeException.class)
    public void testJoinTypeException() {
        AstModel model = newModel();

        AstAbstractClafer A = model.addAbstract("A");
        AstConcreteClafer a = A.addChild("a").withCard(Mandatory).refToUnique(IntType);
        AstAbstractClafer B = model.addAbstract("B").extending(A);
        AstConcreteClafer b = B.addChild("b").withCard(Mandatory).refToUnique(IntType);
        AstConcreteClafer C = model.addChild("C").extending(B).withCard(Mandatory);
        AstConcreteClafer c = C.addChild("c").withCard(Mandatory).refToUnique(IntType);
        AstConcreteClafer D = model.addChild("D").extending(B).withCard(Optional);
        AstConcreteClafer d = D.addChild("d").withCard(Mandatory).refToUnique(IntType);
        model.addConstraint(equal(joinRef(join(union(global(C), global(B)), d)), constant(1)));

        Analysis.analyze(model, Scope.defaultScope(1), new TypeAnalyzer());
    }

    /**
     * <pre>
     * abstract A -> int
     * abstract B : A
     * C : B
     * D -> int
     *
     * [(C ++ D).ref = 1]
     * </pre>
     */
    @Test(expected = TypeException.class)
    public void testUpcastJoinRefTypeException() {
        AstModel model = newModel();

        AstAbstractClafer a = model.addAbstract("A").refToUnique(IntType);
        AstAbstractClafer b = model.addAbstract("B").extending(a);
        AstConcreteClafer c = model.addChild("C").extending(b).withCard(Mandatory);
        AstConcreteClafer d = model.addChild("D").refToUnique(IntType).withCard(Mandatory);
        model.addConstraint(equal(joinRef(union(global(c), global(d))), constant(1)));

        Analysis.analyze(model, Scope.defaultScope(1), new TypeAnalyzer());
    }

    /**
     * <pre>
     * abstract A
     * B : A
     * C ?
     * [ C in A ++ B ]
     * </pre>
     */
    @Test(expected = TypeException.class)
    public void testEqualityTypeException() {
        AstModel model = newModel();

        AstAbstractClafer a = model.addAbstract("A");
        AstConcreteClafer b = model.addChild("B").extending(a).withCard(Mandatory);
        AstConcreteClafer c = model.addChild("C").withCard(Optional);
        model.addConstraint(equal(global(c), union(global(a), global(b))));

        Analysis.analyze(model, Scope.defaultScope(1), new TypeAnalyzer());
    }

    /**
     * <pre>
     * A
     * B -> int
     * [#(A ++ B.ref) = 2]
     * </pre>
     */
    @Test(expected = TypeException.class)
    public void testUnionWithIntTypeException() {
        AstModel model = newModel();

        AstConcreteClafer a = model.addChild("A").withCard(Mandatory);
        AstConcreteClafer b = model.addChild("B").withCard(Mandatory).refToUnique(IntType);
        model.addConstraint(equal(card(union(global(a), joinRef(global(b)))), constant(2)));

        Analysis.analyze(model, Scope.defaultScope(1), new TypeAnalyzer());
    }

    /**
     * <pre>
     * abstract A
     * B : A
     * C
     * [ C in A ++ B ]
     * </pre>
     */
    @Test(expected = TypeException.class)
    public void testMembershipTypeException() {
        AstModel model = newModel();

        AstAbstractClafer a = model.addAbstract("A");
        AstConcreteClafer b = model.addChild("B").extending(a).withCard(Mandatory);
        AstConcreteClafer c = model.addChild("C").withCard(Mandatory);
        model.addConstraint(in(global(c), union(global(a), global(b))));

        Analysis.analyze(model, Scope.defaultScope(1), new TypeAnalyzer());
    }

    /**
     * <pre>
     * A -> int
     * B -> A
     * [ A = sum B ]
     * </pre>
     */
    @Test(expected = TypeException.class)
    public void testSumNonInteger() {
        AstModel model = newModel();

        AstConcreteClafer a = model.addChild("a").refToUnique(IntType).withCard(Mandatory);
        AstConcreteClafer b = model.addChild("b").refToUnique(a).withCard(Mandatory);
        model.addConstraint(equal(global(a), sum(global(b))));

        Analysis.analyze(model, Scope.defaultScope(1), new TypeAnalyzer());
    }

    /**
     * <pre>
     * A -> int
     * B -> int
     * C -> int
     * [ C.ref = sum (A ++ B) ]
     * </pre>
     */
    @Test(expected = TypeException.class)
    public void testSumDifferentTypes() {
        AstModel model = newModel();

        AstConcreteClafer a = model.addChild("a").refToUnique(IntType).withCard(Mandatory);
        AstConcreteClafer b = model.addChild("b").refToUnique(IntType).withCard(Mandatory);
        AstConcreteClafer c = model.addChild("c").refToUnique(IntType).withCard(Mandatory);
        model.addConstraint(equal(joinRef(c), sum(union(global(a), global(b)))));

        Analysis.analyze(model, Scope.defaultScope(1), new TypeAnalyzer());
    }
}
