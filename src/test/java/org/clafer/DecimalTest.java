package org.clafer;

import java.io.File;
import java.io.IOException;
import java.math.BigDecimal;
import java.net.URISyntaxException;
import java.util.ArrayList;
import java.util.List;
import org.clafer.ast.AstAbstractClafer;
import org.clafer.ast.AstBoolExpr;
import org.clafer.ast.AstConcreteClafer;
import org.clafer.ast.AstLocal;
import org.clafer.ast.AstModel;
import org.clafer.ast.AstSetExpr;
import static org.clafer.ast.Asts.$this;
import static org.clafer.ast.Asts.DecimalType;
import static org.clafer.ast.Asts.add;
import static org.clafer.ast.Asts.all;
import static org.clafer.ast.Asts.constant;
import static org.clafer.ast.Asts.decimal;
import static org.clafer.ast.Asts.decl;
import static org.clafer.ast.Asts.div;
import static org.clafer.ast.Asts.equal;
import static org.clafer.ast.Asts.global;
import static org.clafer.ast.Asts.greaterThan;
import static org.clafer.ast.Asts.greaterThanEqual;
import static org.clafer.ast.Asts.ifThenElse;
import static org.clafer.ast.Asts.in;
import static org.clafer.ast.Asts.joinRef;
import static org.clafer.ast.Asts.lessThanEqual;
import static org.clafer.ast.Asts.local;
import static org.clafer.ast.Asts.max;
import static org.clafer.ast.Asts.min;
import static org.clafer.ast.Asts.minus;
import static org.clafer.ast.Asts.mod;
import static org.clafer.ast.Asts.mul;
import static org.clafer.ast.Asts.newModel;
import static org.clafer.ast.Asts.notEqual;
import static org.clafer.ast.Asts.some;
import static org.clafer.ast.Asts.sub;
import static org.clafer.ast.Asts.sum;
import static org.clafer.ast.Asts.union;
import org.clafer.compiler.ClaferCompiler;
import org.clafer.compiler.ClaferOptimizer;
import org.clafer.compiler.ClaferSearch;
import org.clafer.compiler.ClaferSolver;
import org.clafer.instance.InstanceClafer;
import org.clafer.javascript.Javascript;
import org.clafer.javascript.JavascriptFile;
import org.clafer.objective.Objective;
import org.clafer.scope.Scope;
import org.clafer.scope.ScopeBuilder;
import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;
import org.junit.Test;

/**
 * Decimals: numbers with a fixed number of fractional digits, the decimal
 * scale, solved as scaled integers. {@code decimal * decimal} and divisions
 * round half away from zero.
 *
 * @author Michał Antkiewicz
 */
public class DecimalTest {

    private static BigDecimal d(String value) {
        return new BigDecimal(value);
    }

    /**
     * A model with a single decimal {@code X}, constrained by the given
     * constraint of {@code X}.
     */
    private static class Single {

        final AstModel model = newModel();
        final AstConcreteClafer x = model.addChild("X").withCard(1, 1).refTo(DecimalType);
        final AstSetExpr X = joinRef(global(x));

        Single constrain(AstBoolExpr constraint) {
            model.addConstraint(constraint);
            return this;
        }

        List<Object> values(ScopeBuilder scope) {
            ClaferSolver solver = ClaferCompiler.compile(model, scope);
            List<Object> values = new ArrayList<>();
            while (solver.find()) {
                for (InstanceClafer instance : solver.instance().getTopClafers(x)) {
                    values.add(instance.getRef());
                }
            }
            return values;
        }

        Object value(ScopeBuilder scope) {
            List<Object> values = values(scope);
            assertEquals("Number of instances, " + values, 1, values.size());
            return values.get(0);
        }
    }

    private static Single single() {
        return new Single();
    }

    private static ScopeBuilder scale(int decimalScale) {
        return Scope.decimalScale(decimalScale).defaultScope(3);
    }

    /**
     * <pre>
     * X -> decimal
     * [ X = 19.99 ]
     * </pre>
     */
    @Test(timeout = 60000)
    public void testConstant() {
        Single s = single();
        assertEquals(d("19.99"), s.constrain(equal(s.X, decimal("19.99"))).value(scale(2)));
    }

    /**
     * An integer times a decimal is exact; integral decimals print with one
     * fractional digit.
     * <pre>
     * X -> decimal
     * [ X = 100 * 1.99 ]
     * </pre>
     */
    @Test(timeout = 60000)
    public void testIntTimesDecimal() {
        Single s = single();
        assertEquals(d("199.0"), s.constrain(equal(s.X, mul(constant(100), decimal("1.99")))).value(scale(2)));
    }

    /**
     * Integers mix with decimals in sums and differences.
     * <pre>
     * X -> decimal
     * [ X = 1.25 + 3 - 0.5 ]
     * </pre>
     */
    @Test(timeout = 60000)
    public void testAddSub() {
        Single s = single();
        assertEquals(d("3.75"), s.constrain(equal(s.X,
                sub(add(decimal("1.25"), constant(3)), decimal("0.5")))).value(scale(2)));
    }

    /**
     * <pre>
     * Price -> decimal = 19.99
     * Rate -> decimal = 0.13
     * X -> decimal = Price * Rate      // 2.5987
     * </pre>
     */
    @Test(timeout = 60000)
    public void testMulRounds() {
        Single s = single();
        AstConcreteClafer price = s.model.addChild("Price").withCard(1, 1).refTo(DecimalType);
        AstConcreteClafer rate = s.model.addChild("Rate").withCard(1, 1).refTo(DecimalType);
        s.constrain(equal(joinRef(global(price)), decimal("19.99")));
        s.constrain(equal(joinRef(global(rate)), decimal("0.13")));
        s.constrain(equal(s.X, mul(joinRef(global(price)), joinRef(global(rate)))));
        assertEquals(d("2.6"), s.value(scale(2)));
    }

    /**
     * Rounding is half away from zero.
     */
    @Test(timeout = 60000)
    public void testMulRoundsHalfAwayFromZero() {
        Single negative = single();
        // -19.99 * 0.13 = -2.5987
        assertEquals(d("-2.6"), negative.constrain(equal(negative.X,
                mul(minus(decimal("19.99")), decimal("0.13")))).value(scale(2)));
        Single tie = single();
        // 0.50 * 0.25 = 0.125
        assertEquals(d("0.13"), tie.constrain(equal(tie.X,
                mul(decimal("0.50"), decimal("0.25")))).value(scale(2)));
        Single negativeTie = single();
        assertEquals(d("-0.13"), negativeTie.constrain(equal(negativeTie.X,
                mul(decimal("-0.50"), decimal("0.25")))).value(scale(2)));
    }

    /**
     * <pre>
     * X -> decimal
     * [ X * X = 2 ]
     * </pre>
     * No decimal with 2 digits: 1.41 * 1.41 = 1.99 and 1.42 * 1.42 = 2.02.
     */
    @Test(timeout = 60000)
    public void testMulOnlyOnTheGrid() {
        Single s = single();
        assertEquals(0, s.constrain(equal(mul(s.X, s.X), constant(2))).values(scale(2)).size());
    }

    /**
     * Division with a decimal rounds; a division of integers stays an integer
     * division.
     */
    @Test(timeout = 60000)
    public void testDiv() {
        Single share = single();
        assertEquals(d("6.67"), share.constrain(equal(share.X, div(decimal("20.00"), constant(3)))).value(scale(2)));
        Single intByDecimal = single();
        assertEquals(d("0.67"), intByDecimal.constrain(equal(intByDecimal.X, div(constant(2), decimal("3.0")))).value(scale(2)));
        Single negative = single();
        assertEquals(d("-0.67"), negative.constrain(equal(negative.X, div(constant(2), decimal("-3.0")))).value(scale(2)));
        Single intByInt = single();
        assertEquals(d("0.0"), intByInt.constrain(equal(intByInt.X, div(constant(2), constant(3)))).value(scale(2)));
    }

    /**
     * Division of variables, so the division is propagated.
     * <pre>
     * Total -> decimal = 10
     * Count -> decimal
     * [ 1 <= Count && Count <= 4 ]
     * X -> decimal = Total / Count
     * [ X > 3 ]
     * </pre>
     */
    @Test(timeout = 60000)
    public void testDivVariables() {
        AstModel model = newModel();
        AstConcreteClafer total = model.addChild("Total").withCard(1, 1).refTo(DecimalType);
        AstConcreteClafer count = model.addChild("Count").withCard(1, 1).refTo(DecimalType);
        AstConcreteClafer x = model.addChild("X").withCard(1, 1).refTo(DecimalType);
        model.addConstraint(equal(joinRef(global(total)), constant(10)));
        model.addConstraint(greaterThanEqual(joinRef(global(count)), constant(1)));
        model.addConstraint(lessThanEqual(joinRef(global(count)), constant(4)));
        model.addConstraint(equal(joinRef(global(x)), div(joinRef(global(total)), joinRef(global(count)))));
        model.addConstraint(greaterThan(joinRef(global(x)), constant(3)));
        // Count in [1.0, 3.33] at scale 2: 10 / 3.33 = 3.0030, rounds to 3.00, so it is excluded.
        ClaferSolver solver = ClaferCompiler.compile(model, scale(2));
        int instances = 0;
        while (solver.find()) {
            for (InstanceClafer c : solver.instance().getTopClafers(count)) {
                BigDecimal value = (BigDecimal) c.getRef();
                assertTrue(value + " <= 3.32", value.compareTo(d("3.32")) <= 0);
            }
            instances++;
        }
        // Count = 1.00, 1.01, ..., 3.32
        assertEquals(233, instances);
    }

    /**
     * <pre>
     * X -> decimal
     * [ X = 7.5 % 2 ]
     * </pre>
     */
    @Test(timeout = 60000)
    public void testMod() {
        Single s = single();
        assertEquals(d("1.5"), s.constrain(equal(s.X, mod(decimal("7.5"), constant(2)))).value(scale(1)));
    }

    /**
     * <pre>
     * X -> decimal
     * [ X >= 1.25 && X <= 1.3 ]
     * </pre>
     */
    @Test(timeout = 60000)
    public void testCompare() {
        Single s = single();
        s.constrain(greaterThanEqual(s.X, decimal("1.25")));
        s.constrain(lessThanEqual(s.X, decimal("1.3")));
        List<Object> values = s.values(scale(2));
        assertEquals(6, values.size());
        assertTrue(values.contains(d("1.25")));
        assertTrue(values.contains(d("1.3")));
    }

    /**
     * The default range of decimals is the largest that fits Choco's integers:
     * 214748.35 with 2 digits.
     * <pre>
     * X -> decimal
     * [ X > 214748.3 ]
     * </pre>
     */
    @Test(timeout = 60000)
    public void testDefaultRange() {
        Single s = single();
        List<Object> values = s.constrain(greaterThan(s.X, decimal("214748.3"))).values(scale(2));
        assertEquals(5, values.size());
        assertTrue(values.contains(d("214748.35")));
    }

    /**
     * <pre>
     * X -> decimal
     * [ X >= 0 ]
     * </pre>
     * with the decimal range 0..0.5
     */
    @Test(timeout = 60000)
    public void testDecimalRange() {
        Single s = single();
        List<Object> values = s.constrain(greaterThanEqual(s.X, constant(0)))
                .values(scale(1).decimalLow(d("-0.5")).decimalHigh(d("0.5")));
        assertEquals(6, values.size());
    }

    /**
     * <pre>
     * abstract Item -> decimal
     * Book : Item ?
     *     [ this.ref = 12.5 ]
     * Pen : Item ?
     *     [ this.ref = 1.99 ]
     * Bag : Item ?
     *     [ this.ref = 30 ]
     * Total -> decimal = sum Item
     * [ Total <= 20 ]
     * </pre>
     */
    @Test(timeout = 60000)
    public void testSum() {
        AstModel model = newModel();
        AstAbstractClafer item = model.addAbstract("Item").refTo(DecimalType);
        AstConcreteClafer book = model.addChild("Book").withCard(0, 1).extending(item);
        AstConcreteClafer pen = model.addChild("Pen").withCard(0, 1).extending(item);
        AstConcreteClafer bag = model.addChild("Bag").withCard(0, 1).extending(item);
        book.addConstraint(equal(joinRef($this()), decimal("12.5")));
        pen.addConstraint(equal(joinRef($this()), decimal("1.99")));
        bag.addConstraint(equal(joinRef($this()), constant(30)));
        AstConcreteClafer total = model.addChild("Total").withCard(1, 1).refTo(DecimalType);
        model.addConstraint(equal(joinRef(global(total)), sum(global(item))));
        model.addConstraint(lessThanEqual(joinRef(global(total)), constant(20)));

        ClaferSolver solver = ClaferCompiler.compile(model, scale(2));
        List<Object> totals = new ArrayList<>();
        while (solver.find()) {
            for (InstanceClafer t : solver.instance().getTopClafers(total)) {
                totals.add(t.getRef());
            }
        }
        assertEquals(4, totals.size());
        assertTrue(totals.containsAll(List.of(d("0.0"), d("12.5"), d("1.99"), d("14.49"))));

        ClaferOptimizer optimizer = ClaferCompiler.compile(model, scale(2),
                Objective.maximize(joinRef(global(total))));
        assertTrue(optimizer.find());
        // The optimizer reports the scaled decimal.
        assertArrayEquals(new int[]{1449}, optimizer.optimalValues());
        assertEquals(d("14.49"), optimizer.instance().getTopClafers(total)[0].getRef());
        assertFalse(optimizer.find());
    }

    /**
     * <pre>
     * abstract Item -> decimal
     * Book : Item
     *     [ this.ref = 12.5 ]
     * Pen : Item
     *     [ this.ref = 1.99 ]
     * X -> decimal = max Item.ref - min Item.ref
     * </pre>
     */
    @Test(timeout = 60000)
    public void testMinMax() {
        Single s = single();
        AstAbstractClafer item = s.model.addAbstract("Item").refTo(DecimalType);
        AstConcreteClafer book = s.model.addChild("Book").withCard(1, 1).extending(item);
        AstConcreteClafer pen = s.model.addChild("Pen").withCard(1, 1).extending(item);
        book.addConstraint(equal(joinRef($this()), decimal("12.5")));
        pen.addConstraint(equal(joinRef($this()), decimal("1.99")));
        s.constrain(equal(s.X, sub(max(joinRef(global(item))), min(joinRef(global(item))))));
        assertEquals(d("10.51"), s.value(scale(2)));
    }

    /**
     * A quantified constraint with a rounded product, which is reified for
     * each Item.
     * <pre>
     * abstract Item -> decimal
     * Book : Item ?
     *     [ this.ref = 12.5 ]
     * Pen : Item ?
     *     [ this.ref = 1.99 ]
     * Bag : Item ?
     *     [ this.ref = 30 ]
     * [ all i : Item | i.ref * 1.5 <= 30 ]
     * </pre>
     */
    @Test(timeout = 60000)
    public void testQuantifier() {
        // The products of the Items that do not exist are those of their
        // uninitialized references, beyond the range of decimals; they must not
        // exclude the instances.
        AstLocal i = local("i");
        assertItems(model -> model.addConstraint(all(decl(i, global(item(model))),
                lessThanEqual(mul(joinRef(i), decimal("1.5")), constant(30)))));
        // The same constraint in the context of Item.
        assertItems(model -> item(model).addConstraint(
                lessThanEqual(mul(joinRef($this()), decimal("1.5")), constant(30))));
        // A division: 12.5 / 0.6 = 20.83, 1.99 / 0.6 = 3.32, 30 / 0.6 = 50
        AstLocal j = local("j");
        assertItems(model -> model.addConstraint(all(decl(j, global(item(model))),
                lessThanEqual(div(joinRef(j), decimal("0.6")), constant(25)))));
    }

    private static AstAbstractClafer item(AstModel model) {
        return model.getAbstracts().stream().filter(c -> c.getName().equals("Item")).findFirst().get();
    }

    /**
     * Items Book (12.5), Pen (1.99) and Bag (30), all optional, and a
     * constraint that excludes the Bag.
     */
    private static void assertItems(java.util.function.Consumer<AstModel> constraint) {
        AstModel model = newModel();
        AstAbstractClafer item = model.addAbstract("Item").refTo(DecimalType);
        AstConcreteClafer book = model.addChild("Book").withCard(0, 1).extending(item);
        AstConcreteClafer pen = model.addChild("Pen").withCard(0, 1).extending(item);
        AstConcreteClafer bag = model.addChild("Bag").withCard(0, 1).extending(item);
        book.addConstraint(equal(joinRef($this()), decimal("12.5")));
        pen.addConstraint(equal(joinRef($this()), decimal("1.99")));
        bag.addConstraint(equal(joinRef($this()), constant(30)));
        constraint.accept(model);

        ClaferSolver solver = ClaferCompiler.compile(model, scale(2));
        int instances = 0;
        while (solver.find()) {
            assertEquals(0, solver.instance().getTopClafers(bag).length);
            instances++;
        }
        // The subsets of Book and Pen.
        assertEquals(4, instances);
    }

    /**
     * <pre>
     * X -> decimal
     * [ X >= 1 && X <= 1.2 && X != 1.1 ]
     * </pre>
     */
    @Test(timeout = 60000)
    public void testNotEqual() {
        Single s = single();
        s.constrain(greaterThanEqual(s.X, constant(1)));
        s.constrain(lessThanEqual(s.X, decimal("1.2")));
        s.constrain(notEqual(s.X, decimal("1.1")));
        List<Object> values = s.values(scale(1));
        assertEquals(2, values.size());
        assertTrue(values.containsAll(List.of(d("1.0"), d("1.2"))));
    }

    /**
     * <pre>
     * X -> decimal
     * [ X in (1, 2.5) ]
     * </pre>
     */
    @Test(timeout = 60000)
    public void testMembership() {
        Single s = single();
        List<Object> values = s.constrain(in(s.X, union(constant(1), decimal("2.5")))).values(scale(1));
        assertEquals(2, values.size());
        assertTrue(values.containsAll(List.of(d("1.0"), d("2.5"))));
    }

    /**
     * <pre>
     * Flag ?
     * X -> decimal = if some Flag then 1 else 0.5
     * </pre>
     */
    @Test(timeout = 60000)
    public void testIfThenElse() {
        Single s = single();
        AstConcreteClafer flag = s.model.addChild("Flag").withCard(0, 1);
        List<Object> values = s.constrain(equal(s.X,
                ifThenElse(some(global(flag)), constant(1), decimal("0.5")))).values(scale(1));
        assertEquals(2, values.size());
        assertTrue(values.containsAll(List.of(d("1.0"), d("0.5"))));
    }

    /**
     * Decimals print without scientific notation.
     */
    @Test(timeout = 60000)
    public void testPrintSmall() throws IOException {
        Single s = single();
        s.constrain(equal(s.X, decimal("0.0000001")));
        ClaferSolver solver = ClaferCompiler.compile(s.model, scale(7));
        assertTrue(solver.find());
        StringBuilder out = new StringBuilder();
        solver.instance().print(out);
        assertEquals("X -> 0.0000001\n", out.toString());
    }

    @Test(timeout = 60000)
    public void testTooManyDigits() {
        Single s = single();
        try {
            s.constrain(equal(s.X, decimal("1.25"))).values(scale(1));
            fail("1.25 has more digits than the scale 1");
        } catch (RuntimeException e) {
            assertTrue(e.getMessage(), e.getMessage().contains("more fractional digits than the decimal scale 1"));
        }
    }

    @Test(timeout = 60000)
    public void testOutOfRange() {
        Single s = single();
        try {
            s.constrain(equal(s.X, decimal("214748.36"))).values(scale(2));
            fail("214748.36 is beyond the range of decimals with 2 digits");
        } catch (RuntimeException e) {
            assertTrue(e.getMessage(), e.getMessage().contains("out of the range of decimals"));
        }
    }

    /**
     * An attributed feature model with decimal costs and weights, see the
     * comment in decimal_bike.js.
     */
    @Test(timeout = 60000)
    public void testBike() throws IOException, URISyntaxException {
        JavascriptFile js = Javascript.readModel(
                new File(DecimalTest.class.getResource("/optimization/decimal_bike.js").toURI()));
        AstConcreteClafer aBike = (AstConcreteClafer) js.getModel().getChildren().stream()
                .filter(c -> c.getName().equals("c0_aBike")).findFirst().get();
        AstConcreteClafer total = js.getModel().getAbstracts().stream()
                .filter(c -> c.getName().equals("c0_Bike")).findFirst().get()
                .getChildren().stream().filter(c -> c.getName().equals("c0_total")).findFirst().get();

        ClaferSolver solver = ClaferCompiler.compile(js.getModel(), js.getScope());
        List<Object> totals = new ArrayList<>();
        while (solver.find()) {
            InstanceClafer bike = solver.instance().getTopClafers(aBike)[0];
            totals.add(bike.getChildren(total)[0].getRef());
        }
        assertEquals(3, totals.size());
        assertTrue(totals.containsAll(List.of(d("287.87"), d("310.46"), d("326.85"))));

        ClaferOptimizer optimizer = ClaferCompiler.compile(js.getModel(), js.getScope(), js.getObjectives());
        assertTrue(optimizer.find());
        assertArrayEquals(new int[]{32685}, optimizer.optimalValues());
        assertFalse(optimizer.find());
    }

    /**
     * The Javascript format, as clafer would generate it for
     * <pre>
     * rate -> decimal
     * [ rate = 1.99 ]
     *
     * interest -> decimal = 100 * rate
     * </pre>
     */
    @Test(timeout = 60000)
    public void testJavascript() throws IOException {
        JavascriptFile js = Javascript.readModel(
                "defaultScope(1);\n"
                + "stringLength(16);\n"
                + "decimalScale(2);\n"
                + "c0_rate = Clafer(\"c0_rate\").withCard(1, 1);\n"
                + "c0_interest = Clafer(\"c0_interest\").withCard(1, 1);\n"
                + "c0_rate.refToUnique(Decimal);\n"
                + "Constraint(equal(joinRef(global(c0_rate)), decimal(\"1.99\")));\n"
                + "c0_interest.refToUnique(Decimal);\n"
                + "Constraint(implies(some(global(c0_interest)), equal(joinRef(global(c0_interest)), "
                + "mul(constant(100), joinRef(global(c0_rate))))));\n");
        ClaferSearch solver = ClaferCompiler.compile(js.getModel(), js.getScope());
        assertTrue(solver.find());
        StringBuilder out = new StringBuilder();
        solver.instance().print(out);
        assertEquals("rate -> 1.99\ninterest -> 199.0\n", out.toString());
        assertFalse(solver.find());
    }
}
