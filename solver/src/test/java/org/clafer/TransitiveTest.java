package org.clafer;

import java.util.HashSet;
import java.util.Set;
import org.clafer.ast.AstAbstractClafer;
import org.clafer.ast.AstConcreteClafer;
import org.clafer.ast.AstModel;
import static org.clafer.ast.Asts.$this;
import static org.clafer.ast.Asts.Optional;
import static org.clafer.ast.Asts.equal;
import static org.clafer.ast.Asts.global;
import static org.clafer.ast.Asts.none;
import org.clafer.ast.Card;
import static org.clafer.ast.Asts.Mandatory;
import static org.clafer.ast.Asts.in;
import static org.clafer.ast.Asts.join;
import static org.clafer.ast.Asts.joinRef;
import static org.clafer.ast.Asts.newModel;
import static org.clafer.ast.Asts.notIn;
import static org.clafer.ast.Asts.ref;
import static org.clafer.ast.Asts.relation;
import static org.clafer.ast.Asts.transitiveClosure;
import static org.clafer.ast.Asts.transitiveReflexiveClosure;
import org.clafer.compiler.ClaferCompiler;
import org.clafer.compiler.ClaferSolver;
import org.clafer.instance.InstanceClafer;
import org.clafer.instance.InstanceModel;
import org.clafer.scope.Scope;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import org.junit.Test;

/**
 *
 * @author jimmy
 */
public class TransitiveTest {

    /**
     * <pre>
     * Person *
     *     Child -> Person *
     *     [ this not in this.(Person -> Child . Child -> ref)* ]
     * </pre>
     */
    @Test(timeout = 60000)
    public void testAcyclic() {
        AstModel model = newModel();

        AstConcreteClafer person = model.addChild("Person");
        AstConcreteClafer child = person.addChild("Child").refToUnique(person);
        person.addConstraint(notIn($this(), join($this(), transitiveClosure(join(relation(child), ref(child))))));

        ClaferSolver solver = ClaferCompiler.compile(model, Scope.defaultScope(2));
        assertEquals(4, solver.allInstances().length);
    }

    private Set<InstanceClafer> getChildrenRecurisve(InstanceClafer parent, AstConcreteClafer childType) {
        Set<InstanceClafer> children = new HashSet<>();
        children.add(parent);
        for (InstanceClafer child : parent.getChildren(childType)) {
            children.addAll(getChildrenRecurisve((InstanceClafer) child.getRef(), childType));
        }
        return children;
    }

    /**
     * <pre>
     * abstract Person *
     *     Child -> Person *
     *     [ this not in this.(Person -> Child . Child -> ref)* ]
     * Alice : Person
     *     [ AliceDescendant.ref in this.(Person -> Child . Child -> ref)** ]
     * Bob : Person
     * Carol : Person
     * AliceDescendant -> Person
     * </pre>
     */
    @Test(timeout = 60000)
    public void testDescendant() {
        AstModel model = newModel();

        AstAbstractClafer person = model.addAbstract("Person");
        AstConcreteClafer child = person.addChild("Child").refToUnique(person);
        person.addConstraint(notIn($this(), join($this(), transitiveClosure(join(relation(child), ref(child))))));
        AstConcreteClafer alice = model.addChild("Alice").extending(person).withCard(Mandatory);
        AstConcreteClafer bob = model.addChild("Bob").extending(person).withCard(Mandatory);
        AstConcreteClafer carol = model.addChild("Carol").extending(person).withCard(Mandatory);
        AstConcreteClafer aliceDescendant = model.addChild("AliceDescendant").refToUnique(person).withCard(Mandatory);
        alice.addConstraint(in(joinRef(aliceDescendant), join($this(), transitiveReflexiveClosure(join(relation(child), ref(child))))));

        ClaferSolver solver = ClaferCompiler.compile(model, Scope.defaultScope(2));
        while (solver.find()) {
            InstanceModel instance = solver.instance();
            Set<InstanceClafer> ac = getChildrenRecurisve(instance.getTopClafer(alice), child);
            for (InstanceClafer ad : instance.getTopClafers(aliceDescendant)) {
                assertTrue(ac.contains((InstanceClafer) ad.getRef()));
            }
        }
        assertEquals(36, solver.instanceCount());
    }



    /**
     * The closure follows the relation more than one step (gsdlab/chocosolver#47).
     *
     * <pre>
     * abstract Node
     *     succ -> Node ?
     * N1 : Node
     *     [ succ = N2 ]
     * N2 : Node
     *     [ succ = N3 ]
     * N3 : Node
     *     [ no succ ]
     * reach -> Node *
     * [ reach = N1.(Node -> succ . succ -> ref)* ]
     * </pre>
     */
    @Test(timeout = 60000)
    public void testChain() {
        AstModel model = newModel();

        AstAbstractClafer node = model.addAbstract("Node");
        AstConcreteClafer succ = node.addChild("succ").refToUnique(node).withCard(Optional);
        AstConcreteClafer n1 = model.addChild("N1").extending(node).withCard(Mandatory);
        AstConcreteClafer n2 = model.addChild("N2").extending(node).withCard(Mandatory);
        AstConcreteClafer n3 = model.addChild("N3").extending(node).withCard(Mandatory);
        n1.addConstraint(equal(joinRef(join($this(), succ)), global(n2)));
        n2.addConstraint(equal(joinRef(join($this(), succ)), global(n3)));
        n3.addConstraint(none(join($this(), succ)));
        AstConcreteClafer reach = model.addChild("reach").refToUnique(node).withCard(new Card(0));
        model.addConstraint(equal(joinRef(global(reach)), join(global(n1), transitiveClosure(join(relation(succ), ref(succ))))));

        ClaferSolver solver = ClaferCompiler.compile(model, Scope.defaultScope(3));
        assertTrue(solver.find());
        InstanceModel instance = solver.instance();
        Set<Object> targets = new HashSet<>();
        for (InstanceClafer r : instance.getTopClafers(reach)) {
            targets.add(r.getRef());
        }
        Set<Object> expected = new HashSet<>();
        expected.add(instance.getTopClafer(n2));
        expected.add(instance.getTopClafer(n3));
        assertEquals(expected, targets);
        assertFalse(solver.find());
    }

}
