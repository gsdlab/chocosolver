
import org.clafer.ast.AstConcreteClafer;
import org.clafer.ast.AstModel;
import static org.clafer.ast.Asts.$this;
import static org.clafer.ast.Asts.Mandatory;
import static org.clafer.ast.Asts.card;
import static org.clafer.ast.Asts.equal;
import static org.clafer.ast.Asts.join;
import static org.clafer.ast.Asts.lessThanEqual;
import static org.clafer.ast.Asts.newModel;
import org.clafer.ast.analysis.InsufficientScopeException;
import org.clafer.collection.Pair;
import org.clafer.compiler.ClaferCompiler;
import org.clafer.scope.Scope;
import static org.junit.Assert.assertEquals;
import org.junit.Test;

/**
 *
 * @author jimmy
 */
public class FBBTTest {

    /**
     * <pre>
     * A 2..*
     *     B
     *     C *
     *     [ #(this.B) = #(this.C) ]
     * </pre>
     */
    @Test(timeout = 60000)
    public void testFBBTWithEqualityConstraint() {
        AstModel model = newModel();
        AstConcreteClafer a = model.addChild("A").withCard(2);
        AstConcreteClafer b = a.addChild("B").withCard(Mandatory);
        AstConcreteClafer c = a.addChild("C");
        a.addConstraint(equal(card(join($this(), b)), card(join($this(), c))));

        Pair<Scope, Scope> scopeBounds = ClaferCompiler.fbbt(model, Scope.setScope(a, 5).setScope(b, 10).setScope(c, 100));
        Scope lbScope = scopeBounds.getFst();
        Scope ubScope = scopeBounds.getSnd();

        assertEquals(2, lbScope.getScope(a));
        assertEquals(2, lbScope.getScope(b));
        assertEquals(2, lbScope.getScope(c));

        assertEquals(5, ubScope.getScope(a));
        assertEquals(5, ubScope.getScope(b));
        assertEquals(5, ubScope.getScope(c));
    }

    /**
     * <pre>
     * A 2..*
     *     B
     *     C *
     *     [ #(this.B) <= #(this.C) ]
     * </pre>
     */
    @Test(timeout = 60000)
    public void testFBBTWithInEqualityConstraint() {
        AstModel model = newModel();
        AstConcreteClafer a = model.addChild("A").withCard(2);
        AstConcreteClafer b = a.addChild("B").withCard(Mandatory);
        AstConcreteClafer c = a.addChild("C");
        a.addConstraint(lessThanEqual(card(join($this(), b)), card(join($this(), c))));

        Pair<Scope, Scope> scopeBounds = ClaferCompiler.fbbt(model, Scope.setScope(a, 5).setScope(b, 10).setScope(c, 100));
        Scope lbScope = scopeBounds.getFst();
        Scope ubScope = scopeBounds.getSnd();

        assertEquals(2, lbScope.getScope(a));
        assertEquals(2, lbScope.getScope(b));
        assertEquals(2, lbScope.getScope(c));

        assertEquals(5, ubScope.getScope(a));
        assertEquals(5, ubScope.getScope(b));
        assertEquals(100, ubScope.getScope(c));
    }

    /**
     * <pre>
     * A 2..*
     *     B
     *     C *
     *     [ #(this.B) = #(this.C) ]
     * </pre>
     */
    @Test(timeout = 60000, expected = InsufficientScopeException.class)
    public void testFBBTWithInsufficientScope() {
        AstModel model = newModel();
        AstConcreteClafer a = model.addChild("A").withCard(2);
        AstConcreteClafer b = a.addChild("B").withCard(Mandatory);
        AstConcreteClafer c = a.addChild("C");
        a.addConstraint(equal(card(join($this(), b)), card(join($this(), c))));

        ClaferCompiler.fbbt(model, Scope.setScope(a, 1).setScope(b, 10).setScope(c, 100));
    }
}
