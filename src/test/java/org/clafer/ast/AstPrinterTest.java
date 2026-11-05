package org.clafer.ast;

import static org.clafer.ast.Asts.IntType;
import static org.clafer.ast.Asts.Mandatory;
import static org.clafer.ast.Asts.StringType;
import static org.clafer.ast.Asts.$this;
import static org.clafer.ast.Asts.add;
import static org.clafer.ast.Asts.all;
import static org.clafer.ast.Asts.card;
import static org.clafer.ast.Asts.concat;
import static org.clafer.ast.Asts.constant;
import static org.clafer.ast.Asts.decl;
import static org.clafer.ast.Asts.equal;
import static org.clafer.ast.Asts.global;
import static org.clafer.ast.Asts.join;
import static org.clafer.ast.Asts.joinParent;
import static org.clafer.ast.Asts.joinRef;
import static org.clafer.ast.Asts.local;
import static org.clafer.ast.Asts.minus;
import static org.clafer.ast.Asts.newModel;
import static org.clafer.ast.Asts.not;
import static org.clafer.ast.Asts.or;
import static org.clafer.ast.Asts.product;
import static org.clafer.ast.Asts.some;
import static org.clafer.ast.Asts.sub;
import static org.clafer.ast.Asts.sum;
import static org.clafer.ast.Asts.transitiveClosure;
import static org.clafer.ast.Asts.union;
import static org.clafer.ast.Asts.upcast;
import static org.junit.Assert.assertEquals;
import org.junit.Test;

public class AstPrinterTest {

    @Test
    public void testClafers() {
        AstModel model = newModel();
        AstAbstractClafer feature = model.addAbstract("Feature");
        feature.addChild("cost").refToUnique(IntType).withCard(Mandatory);
        AstConcreteClafer phone = model.addChild("Phone").extending(feature).withCard(2, 2).withGroupCard(1, 1);
        phone.addChild("apps").refTo(feature);
        phone.addAbstractChild("Port");
        model.addConstraint(some(global(phone)));
        assertEquals(""
                + "[(#Phone) >= 1]\n"
                + "abstract 0..* Feature 0..* {\n"
                + "  0..* cost -> int 1..1 {}\n"
                + "}\n"
                + "1..1 Phone : Feature 2..2 {\n"
                + "  abstract 0..* Port 0..* {}\n"
                + "  0..* apps ->> Feature 0..* {}\n"
                + "}\n",
                AstPrinter.print(model));
    }

    /**
     * A clafer without its own group card has its super clafer's.
     */
    @Test
    public void testInheritedGroupCard() {
        AstModel model = newModel();
        AstAbstractClafer device = model.addAbstract("Device").withGroupCard(1, 1);
        model.addChild("Phone").extending(device).withCard(1, 1);
        model.addChild("Tablet").extending(device).withCard(1, 1).withGroupCard(0, 2);
        assertEquals(""
                + "abstract 1..1 Device 0..* {}\n"
                + "1..1 Phone : Device 1..1 {}\n"
                + "0..2 Tablet : Device 1..1 {}\n",
                AstPrinter.print(model));
    }

    /**
     * A clafer without its own reference has its super clafer's.
     */
    @Test
    public void testInheritedRef() {
        AstModel model = newModel();
        AstAbstractClafer t = model.addAbstract("T").refTo(IntType);
        model.addChild("A").extending(t).withCard(1, 1);
        model.addChild("B").extending(t).withCard(1, 1).refToUnique(IntType);
        assertEquals(""
                + "abstract 0..* T ->> int 0..* {}\n"
                + "0..* A : T ->> int 1..1 {}\n"
                + "0..* B : T -> int 1..1 {}\n",
                AstPrinter.print(model));
    }

    /**
     * sum and product add (multiply) the dref of each instance.
     */
    @Test
    public void testSumOfDrefs() {
        AstModel model = newModel();
        AstConcreteClafer phone = model.addChild("Phone");
        AstConcreteClafer cost = phone.addChild("cost").refToUnique(IntType);
        assertEquals("sum Phone . cost . dref", AstPrinter.print(sum(join(global(phone), cost))));
        assertEquals("product Phone . cost . dref", AstPrinter.print(product(join(global(phone), cost))));
        assertEquals("sum Phone . cost . dref . dref", AstPrinter.print(sum(joinRef(join(global(phone), cost)))));
    }

    /**
     * A negative constant and the negation of a number print differently.
     */
    @Test
    public void testNegativeNumbers() {
        assertEquals("-1", AstPrinter.print(constant(-1)));
        assertEquals("-(1)", AstPrinter.print(minus(constant(1))));
        assertEquals("-(-1)", AstPrinter.print(minus(constant(-1))));
        assertEquals("2 - -1", AstPrinter.print(sub(constant(2), constant(-1))));
    }

    @Test
    public void testNavigationBindsTightest() {
        AstModel model = newModel();
        AstConcreteClafer a = model.addChild("A");
        AstConcreteClafer b = a.addChild("B").refToUnique(IntType);
        assertEquals("this . B . dref + 1",
                AstPrinter.print(add(joinRef(join($this(), b)), constant(1))));
        assertEquals("#this . parent", AstPrinter.print(card(joinParent($this()))));
        assertEquals("A . (B ++ B)", AstPrinter.print(join(global(a), union(new AstChildRelation(b), new AstChildRelation(b)))));
        assertEquals("!(this . B . dref = 1)", AstPrinter.print(not(equal(joinRef(join($this(), b)), constant(1)))));
        assertEquals("((#A) = 1) || ((#A) = 2)",
                AstPrinter.print(or(equal(card(global(a)), constant(1)), equal(card(global(a)), constant(2)))));
    }

    @Test
    public void testExpressions() {
        AstModel model = newModel();
        AstConcreteClafer a = model.addChild("A");
        AstConcreteClafer next = a.addChild("next").refTo(a);
        AstLocal x = local("x");
        assertEquals("all x : A | (#x . next) >= 1", AstPrinter.print(all(decl(x, global(a)), some(join(x, next)))));
        assertEquals("^dref(next)", AstPrinter.print(transitiveClosure(new AstRefRelation(next.getRef()))));
        assertEquals("*dref(next)", AstPrinter.print(transitiveClosure(new AstRefRelation(next.getRef()), true)));
        assertEquals("concat(\"a\\\"b\\n\", \"\\u00e9\")", AstPrinter.print(concat(constant("a\"b\n"), constant("\u00e9"))));
        assertEquals("{[1, 2]} :: A", AstPrinter.print(constant(a, 1, 2)));
        assertEquals("upcast(A, string)", AstPrinter.print(upcast(global(a), new ProductType(StringType))));
    }
}
