package org.clafer;

import org.chocosolver.solver.Solver;
import org.chocosolver.solver.cstrs.GCF;
import org.chocosolver.solver.search.GraphStrategyFactory;
import org.chocosolver.solver.variables.*;
import org.chocosolver.util.objects.graphs.UndirectedGraph;
import org.chocosolver.util.objects.setDataStructures.SetType;
import org.clafer.ast.*;
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
    public void test1() throws Exception{
        AstModel m = Asts.newModel();
        AstConcreteClafer node = m.addChild("Node");
        AstConcreteClafer edge = m.addChild("Edge");
        AstConcreteClafer loc = edge.addChild("loc").withCard(2,2).refToUnique(node);
        m.addConstraint(connected(global(node), join(inverse(join(relation(loc), ref(loc))), join(relation(loc), ref(loc)))));

        ClaferSolver solver = ClaferCompiler.compile(m, Scope.defaultScope(4));
        int count = 0;
        while (solver.find()) {
            count++;
            // Print the solution in a format similar to ClaferIG.
            System.out.println("=========== " + count + " ============");
            System.out.println(solver.instance());
        }

        assertEquals(4, count);
    }




}
