package org.clafer;

import java.util.HashSet;
import java.util.Set;
import org.clafer.ast.AstAbstractClafer;
import org.clafer.ast.AstConcreteClafer;
import org.clafer.ast.AstModel;
import static org.clafer.ast.Asts.*;
import org.clafer.compiler.ClaferCompiler;
import org.clafer.compiler.ClaferSolver;
import org.clafer.instance.InstanceClafer;
import org.clafer.instance.InstanceModel;
import org.clafer.scope.Scope;
import static org.junit.Assert.assertEquals;
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
}
