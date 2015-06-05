package org.clafer;

import org.chocosolver.solver.Solver;
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
        n2.addConstraint(equal(joinRef(join($this(), edge)), global(n3)));
        n3.addConstraint(none(join($this(), edge)));
        n4.addConstraint(one(join($this(), edge)));

        AstConcreteClafer wire = model.addChild("wire").withCard(1);
        AstConcreteClafer node = wire.addChild("node").refToUnique(Node);
        wire.addConstraint(equal(joinRef(join($this(), node)), union(union(union(global(n1),global(n2)), global(n3)), global(n4))));

        wire.addConstraint(connected(join($this(), node), join(relation(edge), ref(edge)), false));

        ClaferSolver solver = ClaferCompiler.compile(model, Scope.defaultScope(4));
        /*
        while (solver.find()) {
            // Print the solution in a format similar to ClaferIG.
            System.out.println("=======================");
            System.out.println(solver.instance());
        }
        */

        assertEquals(3, solver.allInstances().length);
    }





}
