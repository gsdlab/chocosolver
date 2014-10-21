package org.clafer;

import java.util.ArrayList;
import java.util.List;
import org.clafer.ast.AstAbstractClafer;
import org.clafer.ast.AstClafer;
import org.clafer.ast.AstConcreteClafer;
import org.clafer.ast.AstModel;
import static org.clafer.ast.Asts.*;
import org.clafer.collection.Pair;
import org.clafer.compiler.ClaferCompiler;
import org.clafer.compiler.ClaferSolver;
import org.clafer.instance.InstanceClafer;
import org.clafer.instance.InstanceModel;
import org.clafer.scope.Scope;
import static org.junit.Assert.assertEquals;
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
            List<Pair<AstClafer, Integer>> descendantNames = new ArrayList<>();
            for (InstanceClafer a : instance.getTopClafers(alice)) {
                InstanceClafer cur = a;
                descendantNames.add(new Pair<>(cur.getType(), cur.getId()));
            }
            for (InstanceClafer ad : instance.getTopClafers(aliceDescendant)) {
                System.out.println(ad);
                System.out.println("  " + ad.getRef());
                System.out.println("    " + ad.getRef().getValue());
            }
        }
        assertEquals(36, solver.instanceCount());
    }
}
