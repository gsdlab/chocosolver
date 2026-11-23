package org.clafer;

import java.util.HashSet;
import java.util.Set;
import org.clafer.ast.AstAbstractClafer;
import org.clafer.ast.AstConcreteClafer;
import org.clafer.ast.AstModel;
import static org.clafer.ast.Asts.$this;
import static org.clafer.ast.Asts.IntType;
import static org.clafer.ast.Asts.Mandatory;
import static org.clafer.ast.Asts.Optional;
import static org.clafer.ast.Asts.card;
import static org.clafer.ast.Asts.connected;
import static org.clafer.ast.Asts.domainRestriction;
import static org.clafer.ast.Asts.equal;
import static org.clafer.ast.Asts.global;
import static org.clafer.ast.Asts.in;
import static org.clafer.ast.Asts.inverse;
import static org.clafer.ast.Asts.join;
import static org.clafer.ast.Asts.joinParent;
import static org.clafer.ast.Asts.joinRef;
import static org.clafer.ast.Asts.newModel;
import static org.clafer.ast.Asts.parent;
import static org.clafer.ast.Asts.rangeRestriction;
import static org.clafer.ast.Asts.ref;
import static org.clafer.ast.Asts.relation;
import static org.clafer.ast.Asts.transitiveClosure;
import static org.clafer.ast.Asts.union;
import org.clafer.collection.Pair;
import org.clafer.compiler.ClaferCompiler;
import org.clafer.compiler.ClaferSolver;
import org.clafer.instance.InstanceClafer;
import org.clafer.instance.InstanceModel;
import org.clafer.scope.Scope;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotEquals;
import static org.junit.Assert.assertTrue;
import org.junit.Test;

/**
 *
 * @author jimmy
 */
public class RelationalTest {

    /**
     * <pre>
     * A *
     *     B ->> A*
     * [ (B -> parent) = (B -> ref) ]
     * </pre>
     */
    @Test(timeout = 60000)
    public void testFunctionEqualityFunction() {
        AstModel model = newModel();

        AstConcreteClafer a = model.addChild("A");
        AstConcreteClafer b = a.addChild("B").refTo(a);
        model.addConstraint(equal(parent(b), ref(b)));

        ClaferSolver solver = ClaferCompiler.compile(model, Scope.defaultScope(3).intLow(-3).intHigh(3));
        assertEquals(18, solver.allInstances().length);
    }

    /**
     * <pre>
     * A -> B *
     *     B *
     * [ (A -> ref) = (A -> B) ]
     * </pre>
     */
    @Test(timeout = 60000)
    public void testFunctionEqualityRelation() {
        AstModel model = newModel();

        AstConcreteClafer a = model.addChild("A");
        AstConcreteClafer b = a.addChild("B");
        a.refToUnique(b);
        model.addConstraint(equal(ref(a), relation(b)));

        ClaferSolver solver = ClaferCompiler.compile(model, Scope.defaultScope(3).intLow(-3).intHigh(3));
        assertEquals(4, solver.allInstances().length);
    }

    /**
     * <pre>
     * A *
     *     B ->> A *
     *     C ->> A *
     * [ (A -> B) . (B -> A) = (A -> C) . (C -> A)]
     * </pre>
     */
    @Test(timeout = 60000)
    public void testRelationEqualityRelation() {
        AstModel model = newModel();

        AstConcreteClafer a = model.addChild("A");
        AstConcreteClafer b = a.addChild("B").refTo(a);
        AstConcreteClafer c = a.addChild("C").refTo(a);
        model.addConstraint(equal(
                join(relation(b), relation(b.getRef())),
                join(relation(c), relation(c.getRef()))));

        ClaferSolver solver = ClaferCompiler.compile(model, Scope.defaultScope(3).intLow(-3).intHigh(3));
        assertEquals(362, solver.allInstances().length);
    }

    /**
     * <pre>
     * Cost ->> int *
     * [ #(Cost -> int) = 2 ]
     * </pre>
     */
    @Test(timeout = 60000)
    public void testFunctionCardinality() {
        AstModel model = newModel();

        AstConcreteClafer cost = model.addChild("Cost").refTo(IntType);
        model.addConstraint(equal(card(relation(cost.getRef())), 2));

        ClaferSolver solver = ClaferCompiler.compile(model, Scope.defaultScope(3).intLow(-3).intHigh(3));
        assertEquals(28, solver.allInstances().length);
    }

    /**
     * <pre>
     * Product *
     *     Feature *
     * [ #(Product -> Feature) = 3 ]
     * </pre>
     */
    @Test(timeout = 60000)
    public void testRelationCardinality() {
        AstModel model = newModel();

        AstConcreteClafer product = model.addChild("Product");
        AstConcreteClafer feature = product.addChild("Feature");
        model.addConstraint(equal(card(relation(feature)), 3));

        ClaferSolver solver = ClaferCompiler.compile(model, Scope.defaultScope(3).intLow(-3).intHigh(3));
        assertEquals(6, solver.allInstances().length);
    }

    /**
     * <pre>
     * A *
     * B -> A *
     * C -> A *
     *
     * [ #(B -> ref ++ C -> ref) = 4 ]
     * </pre>
     */
    @Test(timeout = 60000)
    public void testFunctionUnionFunction() {
        AstModel model = newModel();

        AstConcreteClafer a = model.addChild("A");
        AstConcreteClafer b = model.addChild("B").refTo(a);
        AstConcreteClafer c = model.addChild("C").refTo(a);
        model.addConstraint(equal(card(union(ref(b), ref(c))), 4));

        ClaferSolver solver = ClaferCompiler.compile(model, Scope.defaultScope(3));
        assertEquals(36, solver.allInstances().length);
    }

    /**
     * <pre>
     * A
     *     B *
     *     C *
     * [ #(A -> B ++ A -> C) == 4 ]
     * </pre>
     */
    @Test(timeout = 60000)
    public void testRelationUnionRelation() {
        AstModel model = newModel();

        AstConcreteClafer a = model.addChild("A").withCard(Mandatory);
        AstConcreteClafer b = a.addChild("B");
        AstConcreteClafer c = a.addChild("C");
        model.addConstraint(equal(card(union(relation(b), relation(c))), 4));

        ClaferSolver solver = ClaferCompiler.compile(model, Scope.defaultScope(3));
        assertEquals(3, solver.allInstances().length);
    }

    /**
     * <pre>
     * Cost -> int
     * [ Cost . (Cost -> int) = 2 ]
     * </pre>
     */
    @Test(timeout = 60000)
    public void testSingletonJoinFunction() {
        AstModel model = newModel();

        AstConcreteClafer cost = model.addChild("Cost").refTo(IntType).withCard(Mandatory);
        model.addConstraint(equal(join(global(cost), relation(cost.getRef())), 2));

        ClaferSolver solver = ClaferCompiler.compile(model, Scope.defaultScope(3).intLow(-3).intHigh(3));
        while (solver.find()) {
            InstanceModel instance = solver.instance();
            for (InstanceClafer c : instance.getTopClafers(cost)) {
                assertEquals(2, c.getRef());
            }
        }
        assertEquals(1, solver.instanceCount());
    }

    /**
     * <pre>
     * Product
     *     Feature *
     * [ #(Product . (Product -> Feature)) = 3 ]
     * </pre>
     */
    @Test(timeout = 60000)
    public void testSingletonJoinRelation() {
        AstModel model = newModel();

        AstConcreteClafer product = model.addChild("Product").withCard(Mandatory);
        AstConcreteClafer feature = product.addChild("Feature");
        model.addConstraint(equal(card(join(global(product), relation(feature))), 3));

        ClaferSolver solver = ClaferCompiler.compile(model, Scope.defaultScope(3).intLow(-3).intHigh(3));
        assertEquals(1, solver.allInstances().length);
    }

    /**
     * <pre>
     * Cost ->> int *
     * [ Cost . (Cost -> int) = 2 ]
     * </pre>
     */
    @Test(timeout = 60000)
    public void testSetJoinFunction() {
        AstModel model = newModel();

        AstConcreteClafer cost = model.addChild("Cost").refTo(IntType);
        model.addConstraint(equal(join(global(cost), relation(cost.getRef())), 2));

        ClaferSolver solver = ClaferCompiler.compile(model, Scope.defaultScope(3).intLow(-3).intHigh(3));
        while (solver.find()) {
            InstanceModel instance = solver.instance();
            for (InstanceClafer c : instance.getTopClafers(cost)) {
                assertEquals(2, c.getRef());
            }
        }
        assertEquals(3, solver.instanceCount());
    }

    /**
     * <pre>
     * Product
     *     Feature *
     * [ #(Product . (Product -> Feature)) = 3 ]
     * </pre>
     */
    @Test(timeout = 60000)
    public void testSetJoinRelation() {
        AstModel model = newModel();

        AstConcreteClafer product = model.addChild("Product");
        AstConcreteClafer feature = product.addChild("Feature");
        model.addConstraint(equal(card(join(global(product), relation(feature))), 3));

        ClaferSolver solver = ClaferCompiler.compile(model, Scope.defaultScope(3).intLow(-3).intHigh(3));
        assertEquals(6, solver.allInstances().length);
    }

    /**
     * <pre>
     * Product ->> Feature *
     * Feature ->> int *
     * [ #((Product -> Feature) . (Feature -> int)) = 2 ]
     * </pre>
     */
    @Test(timeout = 60000)
    public void testFunctionJoinFunction() {
        AstModel model = newModel();

        AstConcreteClafer feature = model.addChild("Feature").refTo(IntType);
        AstConcreteClafer product = model.addChild("Product").refTo(feature);
        model.addConstraint(equal(card(join(relation(product.getRef()), relation(feature.getRef()))), 2));

        ClaferSolver solver = ClaferCompiler.compile(model, Scope.defaultScope(2).intLow(-2).intHigh(2));
        while (solver.find()) {
            InstanceModel instance = solver.instance();
            Set<Pair<Integer, Integer>> tuples = new HashSet<>();
            for (InstanceClafer p : instance.getTopClafers(product)) {
                InstanceClafer f = (InstanceClafer) p.getRef();
                tuples.add(new Pair<>(p.getId(), (Integer) f.getRef()));
            }
            assertEquals(2, tuples.size());
        }
        assertEquals(45, solver.instanceCount());
    }

    /**
     * <pre>
     * A *
     *     B *
     * C ->> A *
     * [ #((C -> A) . (A -> B)) = 3 ]
     * </pre>
     */
    @Test(timeout = 60000)
    public void testFunctionJoinRelation() {
        AstModel model = newModel();

        AstConcreteClafer a = model.addChild("A");
        AstConcreteClafer b = a.addChild("B");
        AstConcreteClafer c = model.addChild("C").refTo(a);
        model.addConstraint(equal(card(join(relation(c.getRef()), relation(b))), 3));

        ClaferSolver solver = ClaferCompiler.compile(model, Scope.defaultScope(3).intLow(-2).intHigh(2));
        assertEquals(23, solver.allInstances().length);
    }

    /**
     * <pre>
     * A *
     *     B ->> int *
     * [ #((A -> B) . (B -> int)) = 3 ]
     * </pre>
     */
    @Test(timeout = 60000)
    public void testRelationJoinFunction() {
        AstModel model = newModel();

        AstConcreteClafer a = model.addChild("A");
        AstConcreteClafer b = a.addChild("B").refTo(IntType);
        model.addConstraint(equal(card(join(relation(b), relation(b.getRef()))), 3));

        ClaferSolver solver = ClaferCompiler.compile(model, Scope.defaultScope(3).intLow(-2).intHigh(2));
        assertEquals(165, solver.allInstances().length);
    }

    /**
     * <pre>
     * A *
     *     B *
     *         C *
     * [ #((A -> B) . (B -> C)) = 3 ]
     * </pre>
     */
    @Test(timeout = 60000)
    public void testRelationJoinRelation() {
        AstModel model = newModel();

        AstConcreteClafer a = model.addChild("A");
        AstConcreteClafer b = a.addChild("B");
        AstConcreteClafer c = b.addChild("C");
        model.addConstraint(equal(card(join(relation(b), relation(c))), 3));

        ClaferSolver solver = ClaferCompiler.compile(model, Scope.defaultScope(3).intLow(-2).intHigh(2));
        assertEquals(37, solver.allInstances().length);
    }

    /**
     * <pre>
     * A ?
     *     B -> C 0..2
     * C 0..2
     *     D 0..2
     * [ #((A -> B) . (B -> ref) . (C -> D)) = 3 ]
     * </pre>
     */
    @Test(timeout = 60000)
    public void testRelationJoinFunctionJoinRelation() {
        AstModel model = newModel();

        AstConcreteClafer a = model.addChild("A").withCard(Optional);
        AstConcreteClafer b = a.addChild("B").withCard(0, 2);
        AstConcreteClafer c = model.addChild("C").withCard(0, 2);
        AstConcreteClafer d = c.addChild("D").withCard(0, 2);
        b.refTo(c);
        model.addConstraint(equal(card(join(relation(b), join(ref(b), relation(d)))), 3));

        ClaferSolver solver = ClaferCompiler.compile(model, Scope.defaultScope(3));
        assertTrue(solver.find());
        InstanceModel instance = solver.instance();
        assertEquals(1, instance.getTopClafers(a).length);
        InstanceClafer[] B = instance.getTopClafer(a).getChildren(b);
        assertEquals(2, B.length);
        assertNotEquals(B[0].getRef(), B[1].getRef());
        InstanceClafer[] C = instance.getTopClafers(c);
        assertEquals(2, C.length);
        assertEquals(2, C[0].getChildren(d).length);
        assertEquals(1, C[1].getChildren(d).length);
        assertFalse(solver.find());
    }

    /**
     * <pre>
     * abstract A
     *     B -> int *
     * C : A *
     * D : A *
     * E -> A *
     * [ E.ref . (C <: B) . ref = 1 ]
     * </pre>
     */
    @Test(timeout = 60000)
    public void testDomainRestriction() {
        AstModel model = newModel();

        AstAbstractClafer a = model.addAbstract("A");
        AstConcreteClafer b = a.addChild("B").refTo(IntType);
        AstConcreteClafer c = model.addChild("C").extending(a);
        AstConcreteClafer d = model.addChild("D").extending(a);
        AstConcreteClafer e = model.addChild("E").refToUnique(a);
        model.addConstraint(equal(joinRef(join(joinRef(e), domainRestriction(global(c), relation(b)))), 1));

        ClaferSolver solver = ClaferCompiler.compile(model, Scope.defaultScope(2).intLow(-1).intHigh(1));
        while (solver.find()) {
            for (InstanceClafer ei : solver.instance().getTopClafers(e)) {
                InstanceClafer ai = (InstanceClafer) ei.getRef();
                if (ai.getType().equals(c)) {
                    for (InstanceClafer bi : ai.getChildren(b)) {
                        assertEquals(1, bi.getRef());
                    }
                }
            }
        }
        assertEquals(80, solver.instanceCount());
    }

    /**
     * <pre>
     * A *
     *     B -> int *
     * C -> A *
     * [ A . (C.ref <: B) . ref = 1 ]
     * </pre>
     */
    @Test(timeout = 60000)
    public void testDomainRestrictionRef() {
        AstModel model = newModel();

        AstConcreteClafer a = model.addChild("A");
        AstConcreteClafer b = a.addChild("B").refTo(IntType);
        AstConcreteClafer c = model.addChild("C").refTo(a);
        model.addConstraint(equal(joinRef(join(global(a), domainRestriction(joinRef(c), relation(b)))), 1));

        ClaferSolver solver = ClaferCompiler.compile(model, Scope.defaultScope(2).intLow(-1).intHigh(1));
        while (solver.find()) {
            for (InstanceClafer ci : solver.instance().getTopClafers(c)) {
                InstanceClafer ai = (InstanceClafer) ci.getRef();
                for (InstanceClafer bi : ai.getChildren(b)) {
                    assertEquals(1, bi.getRef());
                }
            }
        }
        assertEquals(17, solver.instanceCount());
    }

    /**
     * <pre>
     * A *
     *     B -> int *
     * C -> B *
     * [ A . (B :> C.ref) . ref = 1 ]
     * </pre>
     */
    @Test(timeout = 60000)
    public void testRangeRestriction() {
        AstModel model = newModel();

        AstConcreteClafer a = model.addChild("A");
        AstConcreteClafer b = a.addChild("B").refTo(IntType);
        AstConcreteClafer c = model.addChild("C").refTo(b);
        model.addConstraint(equal(joinRef(join(global(a), rangeRestriction(relation(b), joinRef(c)))), 1));

        ClaferSolver solver = ClaferCompiler.compile(model, Scope.defaultScope(2).intLow(-1).intHigh(1));
        while (solver.find()) {
            for (InstanceClafer ci : solver.instance().getTopClafers(c)) {
                InstanceClafer bi = (InstanceClafer) ci.getRef();
                assertEquals(1, bi.getRef());
            }
        }
        assertEquals(25, solver.instanceCount());
    }

    /**
     * <pre>
     * abstract A -> int
     * B : A *
     * C : A *
     * D -> A *
     * [ D . (Dref :> B) = 3 ]
     * </pre>
     */
    @Test(timeout = 60000)
    public void testRangeRestrictionRef() {
        AstModel model = newModel();

        AstAbstractClafer a = model.addAbstract("A").refTo(IntType);
        AstConcreteClafer b = model.addChild("B").extending(a);
        AstConcreteClafer c = model.addChild("C").extending(a);
        AstConcreteClafer d = model.addChild("D").refTo(a);
        model.addConstraint(equal(joinRef(join(global(d), rangeRestriction(ref(d), global(b)))), 1));

        ClaferSolver solver = ClaferCompiler.compile(model, Scope.defaultScope(2).intLow(-1).intHigh(1));
        while (solver.find()) {
            for (InstanceClafer di : solver.instance().getTopClafers(d)) {
                InstanceClafer bi = (InstanceClafer) di.getRef();
                if (bi.getType().equals(b)) {
                    assertEquals(1, bi.getRef());
                }
            }
        }
        assertEquals(138, solver.instanceCount());
    }

    /**
     * <pre>
     * A *
     *     B *
     *         [ this . parent = this . ~B ]
     * </pre>
     */
    @Test(timeout = 60000)
    public void testInverse() {
        AstModel model = newModel();

        AstConcreteClafer a = model.addChild("A");
        AstConcreteClafer b = a.addChild("B");
        b.addConstraint(equal(joinParent($this()), join($this(), inverse(relation(b)))));

        ClaferSolver solver = ClaferCompiler.compile(model, Scope.defaultScope(4));
        assertEquals(38, solver.allInstances().length);
    }

    /**
     * <pre>
     * abstract Book
     *     author -> Author +
     *     [ Book in this.book ]
     *
     * abstract Author
     *     book -> Book +
     *     [ Author in this.author ]
     *
     * B : Book 4
     * A : Author 3
     * </pre>
     */
    @Test(timeout = 600000)
    public void testInverseEquality() {
        AstModel model = newModel();

        AstAbstractClafer Book = model.addAbstract("Book");
        AstConcreteClafer author = Book.addChild("author").withCard(1);
        AstAbstractClafer Author = model.addAbstract("Author");
        AstConcreteClafer book = Author.addChild("book").withCard(1);
        author.refToUnique(Author);
        book.refToUnique(Book);
        AstConcreteClafer B = model.addChild("B").withCard(4, 4).extending(Book);
        AstConcreteClafer A = model.addChild("A").withCard(3, 3).extending(Author);
        author.addConstraint(in(joinParent($this()), joinRef(join(joinRef($this()), book))));
        book.addConstraint(in(joinParent($this()), joinRef(join(joinRef($this()), author))));

        ClaferSolver solver = ClaferCompiler.compile(model, Scope.setScope(Book, 5).setScope(author, 30).setScope(Author, 6).setScope(book, 30).setScope(B, 5).setScope(A, 6));
        assertEquals(3284, solver.allInstances().length);
    }

    /**
     * <pre>
     * A 2
     *     B ->> B +
     * [ #((A -> B) . ~(B -> ref)) = 2 ]
     * </pre>
     */
    @Test(timeout = 60000)
    public void testInverseJoinInverse() {
        AstModel model = newModel();

        AstConcreteClafer a = model.addChild("A").withCard(2, 2);
        AstConcreteClafer b = a.addChild("B").withCard(1);
        b.refTo(b);
        model.addConstraint(equal(card(join(inverse(ref(b)), inverse(relation(b)))), 2));

        ClaferSolver solver = ClaferCompiler.compile(model, Scope.defaultScope(3));
        // Can be reduced with better symmetry breaking.
        assertEquals(8, solver.allInstances().length);
    }

    /**
     * <pre>
     * A -> A *
     *     [ #(this . (A -> ref)*) = 2 ]
     * </pre>
     */
    @Test(timeout = 60000)
    public void testTransitiveClosure() {
        AstModel model = newModel();

        AstConcreteClafer a = model.addChild("A");
        a.refToUnique(a);
        a.addConstraint(equal(card(join($this(), transitiveClosure(ref(a)))), 2));

        ClaferSolver solver = ClaferCompiler.compile(model, Scope.defaultScope(4).intLow(-2).intHigh(2));
        // Can be reduced to 2 with better symmetry breaking.
        assertEquals(5, solver.allInstances().length);
    }

    /**
     * <pre>
     * A 2
     *     B ->> B 2
     * [ #((A -> B) . (B -> ref)*) = 2 ]
     * </pre>
     */
    @Test(timeout = 60000)
    public void testRelationJoinTransitiveClosure() {
        AstModel model = newModel();

        AstConcreteClafer a = model.addChild("A").withCard(2, 2);
        AstConcreteClafer b = a.addChild("B").withCard(2, 2);
        b.refTo(b);
        model.addConstraint(equal(card(join(relation(b), transitiveClosure(ref(b)))), 2));

        ClaferSolver solver = ClaferCompiler.compile(model, Scope.defaultScope(4));
        // Can be reduced to 2 with better symmetry breaking.
        assertEquals(3, solver.allInstances().length);
    }

    /**
     * <pre>
     * A +
     *     B ->> B 2
     * [ #((A -> B) . (B -> ref)*) = 2 ]
     * </pre>
     */
    @Test(timeout = 60000)
    public void testVariableRelationJoinTransitiveClosure() {
        AstModel model = newModel();

        AstConcreteClafer a = model.addChild("A").withCard(2);
        AstConcreteClafer b = a.addChild("B").withCard(2, 2);
        b.refTo(b);
        model.addConstraint(equal(card(join(relation(b), transitiveClosure(ref(b)))), 2));

        ClaferSolver solver = ClaferCompiler.compile(model, Scope.defaultScope(4));
        // Can be reduced to 2 with better symmetry breaking.
        assertEquals(3, solver.allInstances().length);
    }

    @Test(timeout = 60000)
    public void testConnected() throws Exception {
        AstModel m = newModel();
        AstConcreteClafer node = m.addChild("Node");
        AstConcreteClafer edge = m.addChild("Edge");
        AstConcreteClafer loc = edge.addChild("loc").withCard(2, 2).refToUnique(node);
        m.addConstraint(connected(global(node), join(inverse(join(relation(loc), ref(loc))), join(relation(loc), ref(loc)))));

        ClaferSolver solver = ClaferCompiler.compile(m, Scope.defaultScope(4));
        assertEquals(5, solver.allInstances().length);
    }
}
