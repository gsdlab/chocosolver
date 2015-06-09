package org.clafer;

import org.chocosolver.solver.Solver;
import org.chocosolver.solver.cstrs.GCF;
import org.chocosolver.solver.search.GraphStrategyFactory;
import org.chocosolver.solver.variables.GVF;
import org.chocosolver.solver.variables.IUndirectedGraphVar;
import org.chocosolver.util.objects.graphs.UndirectedGraph;
import org.chocosolver.util.objects.setDataStructures.SetType;
import org.clafer.ast.AstAbstractClafer;
import org.clafer.ast.AstConcreteClafer;
import org.clafer.ast.AstModel;
import org.clafer.compiler.ClaferCompiler;
import org.clafer.compiler.ClaferSolver;
import org.clafer.instance.InstanceClafer;
import org.clafer.instance.InstanceModel;
import org.clafer.scope.Scope;
import org.junit.Test;

import java.util.HashSet;
import java.util.Set;

import static org.clafer.ast.Asts.*;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

/**
 *
 * @author ed
 */
public class ConnectedTest {


    @Test(timeout = 600000)
    public void testConnected() {
        AstModel model = newModel();

        AstAbstractClafer Node = model.addAbstract("Node");
        AstConcreteClafer edge = Node.addChild("edge").refToUnique(Node);

        AstConcreteClafer n1 = model.addChild("n1").extending(Node).withCard(1,1);

        AstConcreteClafer n2 = model.addChild("n2").extending(Node).withCard(1,1);
        AstConcreteClafer n3 = model.addChild("n3").extending(Node).withCard(1,1);
        AstConcreteClafer n4 = model.addChild("n4").extending(Node).withCard(1,1);


        n1.addConstraint(equal(joinRef(join($this(), edge)), global(n2)));
        //n2.addConstraint(equal(joinRef(join($this(), edge)), global(n1))); //sym

        n2.addConstraint(equal(joinRef(join($this(), edge)), union(global(n1),global(n3))));
        //n3.addConstraint(equal(joinRef(join($this(), edge)), global(n2))); //sym

        //n3.addConstraint(none(join($this(), edge)));
        n4.addConstraint(one(join($this(), edge)));

        AstConcreteClafer wire = model.addChild("wire").withCard(1);
        AstConcreteClafer node = wire.addChild("node").refToUnique(Node);
        wire.addConstraint(equal(joinRef(join($this(), node)), union(union(union(global(n1),global(n2)), global(n3)), global(n4))));

        wire.addConstraint(connected(join($this(), node), join(relation(edge), ref(edge)), false));

        ClaferSolver solver = ClaferCompiler.compile(model, Scope.defaultScope(6));

        int count = 0;
        while (solver.find()) {
            count++;
            // Print the solution in a format similar to ClaferIG.
            System.out.println("=========== " + count + " ===========");
            System.out.println(solver.instance());
        }

        assertEquals(3, count);
    }


    @Test(timeout = 600000)
    public void test_directed_connected() {
        AstModel model = newModel();

        AstAbstractClafer Node = model.addAbstract("Node");
        AstConcreteClafer edge = Node.addChild("edge").refToUnique(Node);

        AstConcreteClafer n1 = model.addChild("n1").extending(Node).withCard(1,1);

        AstConcreteClafer n2 = model.addChild("n2").extending(Node).withCard(1,1);
        AstConcreteClafer n3 = model.addChild("n3").extending(Node).withCard(1,1);
        AstConcreteClafer n4 = model.addChild("n4").extending(Node).withCard(1,1);


        n1.addConstraint(equal(joinRef(join($this(), edge)), global(n2)));
        n2.addConstraint(equal(joinRef(join($this(), edge)), global(n3)));
        n3.addConstraint(equal(joinRef(join($this(), edge)), global(n4))); //sym
        n4.addConstraint(one(join($this(), edge)));

        AstConcreteClafer wire = model.addChild("wire").withCard(1);
        AstConcreteClafer node = wire.addChild("node").refToUnique(Node);
        wire.addConstraint(equal(joinRef(join($this(), node)), union(union(union(global(n1),global(n2)), global(n3)), global(n4))));

        wire.addConstraint(connected(join($this(), node), join(relation(edge), ref(edge)), true));

        ClaferSolver solver = ClaferCompiler.compile(model, Scope.defaultScope(4));

        int count = 0;
        while (solver.find()) {
            count++;
            // Print the solution in a format similar to ClaferIG.
            System.out.println("=========== " + count + " ===========");
            System.out.println(solver.instance());
        }

        assertEquals(1, count);
    }

    @Test(timeout = 600000)
    public void test_directed_connected_nonconstant_nodes() {
        AstModel model = newModel();

        AstAbstractClafer Node = model.addAbstract("Node");
        AstConcreteClafer edge = Node.addChild("edge").refToUnique(Node);

        AstConcreteClafer n1 = model.addChild("n1").extending(Node).withCard(1,1);

        AstConcreteClafer n2 = model.addChild("n2").extending(Node).withCard(1,1);
        AstConcreteClafer n3 = model.addChild("n3").extending(Node).withCard(1,1);
        AstConcreteClafer n4 = model.addChild("n4").extending(Node).withCard(1,1);


        n1.addConstraint(equal(joinRef(join($this(), edge)), global(n2)));
        n2.addConstraint(equal(joinRef(join($this(), edge)), global(n3)));
        n3.addConstraint(equal(joinRef(join($this(), edge)), global(n4))); //sym
        n4.addConstraint(one(join($this(), edge)));

        AstConcreteClafer wire = model.addChild("wire").withCard(1);
        AstConcreteClafer node = wire.addChild("node").refToUnique(Node);
        wire.addConstraint(in(union(union(global(n1), global(n2)), global(n3)), joinRef(join($this(), node))));

        wire.addConstraint(connected(join($this(), node), join(relation(edge), ref(edge)), true));

        ClaferSolver solver = ClaferCompiler.compile(model, Scope.defaultScope(4));

        int count = 0;
        while (solver.find()) {
            count++;
            // Print the solution in a format similar to ClaferIG.
            System.out.println("=========== " + count + " ===========");
            System.out.println(solver.instance());
        }

        assertEquals(1, count);
    }

    @Test(timeout = 600000)
    public void testConnected_nonconstant_nodes() {
        AstModel model = newModel();

        AstAbstractClafer Node = model.addAbstract("Node");
        AstConcreteClafer edge = Node.addChild("edge").refToUnique(Node);

        AstConcreteClafer n1 = model.addChild("n1").extending(Node).withCard(1,1);

        AstConcreteClafer n2 = model.addChild("n2").extending(Node).withCard(1,1);
        AstConcreteClafer n3 = model.addChild("n3").extending(Node).withCard(1,1);
        AstConcreteClafer n4 = model.addChild("n4").extending(Node).withCard(1,1);


        n1.addConstraint(equal(joinRef(join($this(), edge)), global(n2)));
        n2.addConstraint(equal(joinRef(join($this(), edge)), global(n3)));
        n3.addConstraint(none(join($this(), edge)));
        n4.addConstraint(one(join($this(), edge)));

        AstConcreteClafer wire = model.addChild("wire").withCard(1);
        AstConcreteClafer node = wire.addChild("node").refToUnique(Node);
        wire.addConstraint(in(union(union(global(n1), global(n2)), global(n3)), joinRef(join($this(), node))));

        wire.addConstraint(connected(join($this(), node), join(relation(edge), ref(edge)), false));

        ClaferSolver solver = ClaferCompiler.compile(model, Scope.defaultScope(4));

        int count = 0;
        while (solver.find()) {
            count++;
            // Print the solution in a format similar to ClaferIG.
            System.out.println("=========== " + count + " ===========");
            System.out.println(solver.instance());
        }

        assertEquals(6, count);
    }

    @Test(timeout = 600000)
    public void testConnected_propagator() {

        Solver s = new Solver();
        UndirectedGraph LB = new UndirectedGraph(s, 2, SetType.BITSET, false);
        UndirectedGraph UB = new UndirectedGraph(s, 2, SetType.BITSET, false);
        UB.addNode(0);
        UB.addNode(1);
        UB.addEdge(0, 1);
        IUndirectedGraphVar g = GVF.undirected_graph_var("g", LB, UB, s);

        s.post(GCF.connected(g));
        s.set(GraphStrategyFactory.lexico(g));

        if (s.findSolution()) {
            do {
                System.out.println(s.isSatisfied());
                System.out.println(g);
            } while (s.nextSolution());
        }

    }




}
