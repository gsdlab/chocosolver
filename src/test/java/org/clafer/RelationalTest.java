package org.clafer;

import java.util.HashSet;
import java.util.Set;
import org.clafer.ast.AstConcreteClafer;
import org.clafer.ast.AstModel;
import static org.clafer.ast.Asts.*;
import org.clafer.collection.Pair;
import org.clafer.compiler.ClaferCompiler;
import org.clafer.compiler.ClaferSolver;
import org.clafer.instance.InstanceClafer;
import org.clafer.instance.InstanceModel;
import org.clafer.scope.Scope;
import static org.junit.Assert.*;
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
     *     B ->> int
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
     * A *
     *     B *
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

    @Test(timeout = 60000)
    public void testConnected() throws Exception {
        AstModel m = newModel();
        AstConcreteClafer node = m.addChild("Node");
        AstConcreteClafer edge = m.addChild("Edge");
        AstConcreteClafer loc = edge.addChild("loc").withCard(2, 2).refToUnique(node);
        m.addConstraint(connected(global(node), join(inverse(join(relation(loc), ref(loc))), join(relation(loc), ref(loc)))));

        ClaferSolver solver = ClaferCompiler.compile(m, Scope.defaultScope(4));
        assertEquals(4, solver.allInstances().length);
    }
}
