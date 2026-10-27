package org.clafer.cli;

import java.io.IOException;
import org.clafer.javascript.Javascript;
import org.clafer.javascript.JavascriptFile;
import org.clafer.scope.Scope;
import static org.junit.Assert.assertEquals;
import org.junit.Test;

/**
 *
 * @author Michał Antkiewicz
 */
public class UtilsTest {

    private static Scope resolveScopes(String model, String... args) throws IOException {
        JavascriptFile javascriptFile = Javascript.readModel(model);
        return Utils.resolveScopes(javascriptFile, Main.optionParser().parse(args));
    }

    @Test
    public void testDecimalScaleOfModel() throws IOException {
        Scope scope = resolveScopes("decimalScale(2);\n");
        assertEquals(2, scope.getDecimalScale());
        assertEquals(Scope.MaxScaledDecimal, scope.getDecimalHigh());
    }

    @Test
    public void testDecimalDigitsOverridesModel() throws IOException {
        Scope scope = resolveScopes("decimalScale(2);\n", "--decimal-digits=4");
        assertEquals(4, scope.getDecimalScale());
        assertEquals(10000, scope.getDecimalOne());
    }

    @Test
    public void testDecimalDigitsKeepsRange() throws IOException {
        // The range of the model is kept, at the new scale.
        Scope scope = resolveScopes("decimalScale(1);\ndecimalRange(\"-2.5\", \"2.5\");\n", "--decimal-digits=3");
        assertEquals(3, scope.getDecimalScale());
        assertEquals(-2500, scope.getDecimalLow());
        assertEquals(2500, scope.getDecimalHigh());
    }

    @Test(expected = IllegalArgumentException.class)
    public void testDecimalDigitsTooLarge() throws IOException {
        resolveScopes("", "--decimal-digits=" + (Scope.MaxDecimalScale + 1));
    }
}
