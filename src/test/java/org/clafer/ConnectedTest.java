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
import org.clafer.ast.AstStringConstant;
import org.clafer.collection.Triple;
import org.clafer.compiler.ClaferCompiler;
import org.clafer.compiler.ClaferSolver;
import org.clafer.instance.InstanceClafer;
import org.clafer.instance.InstanceModel;
import org.clafer.javascript.Javascript;
import org.clafer.objective.Objective;
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
            System.out.println("=========== " + count + " ============");
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



    @Test(timeout = 600000)
    public void testWiring() {
        AstModel model = null;
        Triple<AstModel, Scope, Objective[]> triple = null;
        try {
            triple = Javascript.readModel(
                    "scope({c0_Edge:3, c0_Location:4, c0_length:3, c0_location:6, c0_possibleEdge:3, c0_takenEdge:3});\n" +
                            "defaultScope(1);\n" +
                            "intRange(-8, 7);\n" +
                            "stringLength(16);\n" +
                            "\n" +
                            "c0_Location = Abstract(\"c0_Location\");\n" +
                            "c0_Edge = Abstract(\"c0_Edge\");\n" +
                            "c0_Wire = Abstract(\"c0_Wire\");\n" +
                            "c0_location = c0_Edge.addChild(\"c0_location\").withCard(2, 2);\n" +
                            "c0_length = c0_Edge.addChild(\"c0_length\").withCard(1, 1);\n" +
                            "c0_possibleEdge = c0_Wire.addChild(\"c0_possibleEdge\");\n" +
                            "c0_takenEdge = c0_Wire.addChild(\"c0_takenEdge\");\n" +
                            "c0_SimpleGraph = Clafer(\"c0_SimpleGraph\").withCard(1, 1);\n" +
                            "c0_l1 = c0_SimpleGraph.addChild(\"c0_l1\").withCard(1, 1).extending(c0_Location);\n" +
                            "c0_l2 = c0_SimpleGraph.addChild(\"c0_l2\").withCard(1, 1).extending(c0_Location);\n" +
                            "c0_l3 = c0_SimpleGraph.addChild(\"c0_l3\").withCard(1, 1).extending(c0_Location);\n" +
                            "c0_l4 = c0_SimpleGraph.addChild(\"c0_l4\").withCard(1, 1).extending(c0_Location);\n" +
                            "c0_e1 = c0_SimpleGraph.addChild(\"c0_e1\").withCard(1, 1).extending(c0_Edge);\n" +
                            "c0_e2 = c0_SimpleGraph.addChild(\"c0_e2\").withCard(1, 1).extending(c0_Edge);\n" +
                            "c0_e3 = c0_SimpleGraph.addChild(\"c0_e3\").withCard(1, 1).extending(c0_Edge);\n" +
                            "c0_w1 = Clafer(\"c0_w1\").withCard(1, 1).extending(c0_Wire);\n" +
                            "c0_location.refToUnique(c0_Location);\n" +
                            "c0_length.refToUnique(Int);\n" +
                            "c0_possibleEdge.refToUnique(c0_Edge);\n" +
                            "c0_takenEdge.refToUnique(c0_Edge);\n" +
                            "c0_Wire.addConstraint($in(joinRef(join($this(), c0_takenEdge)), joinRef(join($this(), c0_possibleEdge))));\n" +
                            "c0_e1.addConstraint(equal(joinRef(join($this(), c0_location)), union(join(joinParent($this()), c0_l1), join(joinParent($this()), c0_l2))));\n" +
                            "c0_e1.addConstraint(equal(joinRef(join($this(), c0_length)), constant(1)));\n" +
                            "c0_e2.addConstraint(equal(joinRef(join($this(), c0_location)), union(join(joinParent($this()), c0_l2), join(joinParent($this()), c0_l3))));\n" +
                            "c0_e2.addConstraint(equal(joinRef(join($this(), c0_length)), constant(1)));\n" +
                            "c0_e3.addConstraint(equal(joinRef(join($this(), c0_location)), union(join(joinParent($this()), c0_l2), join(joinParent($this()), c0_l4))));\n" +
                            "c0_e3.addConstraint(equal(joinRef(join($this(), c0_length)), constant(2)));\n" +
                            "c0_w1.addConstraint(equal(joinRef(join($this(), c0_possibleEdge)), union(union(join(global(c0_SimpleGraph), c0_e1), join(global(c0_SimpleGraph), c0_e2)), join(global(c0_SimpleGraph), c0_e3))));\n"
            );
        }
        catch(Exception e){}
    /*
        AstAbstractClafer Location = model.addAbstract("Location");
        AstAbstractClafer Edge = model.addAbstract("Edge");
        AstConcreteClafer location = Edge.addChild("location").withCard(2,2);
        AstConcreteClafer length = Edge.addChild("length").withCard(1,1);

        AstAbstractClafer Wire = model.addAbstract("Wire");
        AstConcreteClafer possibleEdge = Wire.addChild("possibleEdge");
        AstConcreteClafer takenEdge = Wire.addChild("takenEdge");
*/
        model = triple.getFst();
        Scope scope = triple.getSnd();
        ClaferSolver solver = ClaferCompiler.compile(model, scope);

        int count = 0;
        while (solver.find()) {
            count++;
            // Print the solution in a format similar to ClaferIG.
            System.out.println("=========== " + count + " ===========");
            System.out.println(solver.instance());
        }

        assertEquals(6, count);

    }



}
