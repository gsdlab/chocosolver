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
     * Cost ->> int *
     * [ #(Cost -> int) = 2 ]
     * </pre>
     */
    @Test(timeout = 60000)
    public void testFunctionCardinality() {
        AstModel model = newModel();

        AstConcreteClafer cost = model.addChild("Cost").refTo(IntType);
        model.addConstraint(equal(card(relation(cost.getRef())), constant(2)));

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
        model.addConstraint(equal(card(relation(feature)), constant(3)));

        ClaferSolver solver = ClaferCompiler.compile(model, Scope.defaultScope(3).intLow(-3).intHigh(3));
        assertEquals(28, solver.allInstances().length);
    }

    /**
     * <pre>
     * Cost -> int
     * [ Cost . (Cost -> int) = 2 ]
     * </pre>
     */
    @Test(timeout = 60000)
    public void testSingeletonJoinFunction() {
        AstModel model = newModel();

        AstConcreteClafer cost = model.addChild("Cost").refTo(IntType).withCard(Mandatory);
        model.addConstraint(equal(join(global(cost), relation(cost.getRef())), constant(2)));

        ClaferSolver solver = ClaferCompiler.compile(model, Scope.defaultScope(3).intLow(-3).intHigh(3));
        while (solver.find()) {
            InstanceModel instance = solver.instance();
            for (InstanceClafer c : instance.getTopClafers(cost)) {
                assert c.getRef().getValue().equals(2);
            }
        }
        assertEquals(1, solver.instanceCount());
    }

    /**
     * <pre>
     * Cost ->> int *
     * [ Cost . (Cost -> int) = 2 ]
     * </pre>
     * </pre>
     */
    @Test(timeout = 60000)
    public void testSetJoinFunction() {
        AstModel model = newModel();

        AstConcreteClafer cost = model.addChild("Cost").refTo(IntType);
        model.addConstraint(equal(join(global(cost), relation(cost.getRef())), constant(2)));

        ClaferSolver solver = ClaferCompiler.compile(model, Scope.defaultScope(3).intLow(-3).intHigh(3));
        while (solver.find()) {
            InstanceModel instance = solver.instance();
            for (InstanceClafer c : instance.getTopClafers(cost)) {
                assert c.getRef().getValue().equals(2);
            }
        }
        assertEquals(3, solver.instanceCount());
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
        model.addConstraint(equal(card(join(relation(product.getRef()), relation(feature.getRef()))), constant(2)));

        ClaferSolver solver = ClaferCompiler.compile(model, Scope.defaultScope(2).intLow(-2).intHigh(2));
        while (solver.find()) {
            InstanceModel instance = solver.instance();
            Set<Pair<Integer, Integer>> tuples = new HashSet<>();
            InstanceClafer[] fs = instance.getTopClafers(feature);
            for (InstanceClafer p : instance.getTopClafers(product)) {
                tuples.add(new Pair<Integer, Integer>(p.getId(), (Integer) fs[(Integer) p.getRef().getValue()].getRef().getValue()));
            }
            assert tuples.size() == 2;
        }
        assertEquals(45, solver.instanceCount());
    }
}
