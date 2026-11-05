package org.clafer.frontend;

import java.io.File;
import java.net.URISyntaxException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;
import org.clafer.ast.AstAbstractClafer;
import org.clafer.ast.AstClafer;
import org.clafer.ast.AstModel;
import org.clafer.ast.AstUtil;
import org.clafer.ast.AstPrinter;
import org.clafer.compiler.ClaferCompiler;
import org.clafer.compiler.ClaferSearch;
import org.clafer.javascript.Javascript;
import org.clafer.javascript.JavascriptFile;
import org.clafer.compiler.ClaferOption;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import static org.junit.Assume.assumeNoException;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Parameterized;
import org.junit.runners.Parameterized.Parameter;
import org.junit.runners.Parameterized.Parameters;

/**
 * The round trip {@code .js} → model → explicit form → {@link ExplicitParser}
 * → model: the parsed model prints the same, and chocosolver solves it the
 * same, i.e., the first instances (optimal instances, counterexamples) are the
 * same in the same order, or both fail with the same error.
 * <p>
 * The models are chocosolver's test models and, if a Clafer checkout is next
 * to chocosolver (or at {@code -Dclafer.dir}), the compiler's committed
 * outputs for Clafer's positive tests ({@code test/regression/*.js.reg}).
 */
@RunWith(Parameterized.class)
public class ExplicitRoundTripTest {

    private static final int Instances = 20;

    @Parameter
    public File jsFile;

    @Parameters(name = "{0}")
    public static List<File[]> testFiles() throws URISyntaxException {
        List<File> files = new ArrayList<>();
        for (String dir : List.of("/solve-positive", "/solve-negative", "/assert-positive", "/assert-negative",
                "/optimization", "/failing")) {
            File[] js = new File(ExplicitRoundTripTest.class.getResource(dir).toURI()).listFiles((d, n) -> n.endsWith(".js"));
            files.addAll(Arrays.asList(js));
        }
        File regression = new File(System.getProperty("clafer.dir", "../clafer"), "test/regression");
        File[] reg = regression.listFiles((d, n) -> n.endsWith(".js.reg"));
        if (reg != null) {
            files.addAll(Arrays.asList(reg));
        }
        files.sort(null);
        List<File[]> parameters = new ArrayList<>();
        for (File file : files) {
            parameters.add(new File[]{file});
        }
        return parameters;
    }

    @Test
    public void testRoundTrip() throws Exception {
        JavascriptFile js;
        try {
            js = Javascript.readModel(jsFile);
        } catch (Exception e) {
            // Some of the failing models do not load: nothing to round-trip.
            assumeNoException(e);
            return;
        }
        String printed = AstPrinter.print(js.getModel(), js.getScope(), js.getObjectives(), js.getAssertions());

        ExplicitParser.Result parsed = ExplicitParser.parse(printed);
        assertEquals(List.of(), parsed.diagnostics());
        assertEquals(printed, AstPrinter.print(parsed.model(), parsed.scope(), parsed.objectives(), parsed.assertions()));

        List<String> loaded = solve(js, js.getModel(), js.getScope(), js.getObjectives(), js.getAssertions());
        List<String> reparsed = solve(js, parsed.model(), parsed.scope(), parsed.objectives(), parsed.assertions());
        if (subs(js.getModel()).equals(subs(parsed.model()))) {
            assertEquals(loaded, reparsed);
        } else {
            // The order of the subs of an abstract clafer numbers its instances.
            // The compiler adds them in the order of the source, which the
            // explicit form keeps; the Clafer compiler of 2015 created all
            // abstract clafers first, an order no source has. The instances
            // are then the same but numbered differently.
            assertEquals(jsFile.getName(), loaded.size(), reparsed.size());
        }
    }

    /**
     * The subs of each abstract clafer, in order, by name.
     */
    private static Map<String, List<String>> subs(AstModel model) {
        Map<String, List<String>> subs = new TreeMap<>();
        for (AstClafer clafer : AstUtil.getClafers(model)) {
            if (clafer instanceof AstAbstractClafer abstractClafer) {
                subs.put(clafer.getName(), abstractClafer.getSubs().stream().map(AstClafer::getName).toList());
            }
        }
        return subs;
    }

    /**
     * The first instances, or the error, as text.
     */
    private static List<String> solve(JavascriptFile js, AstModel model, org.clafer.scope.Scope scope,
            org.clafer.objective.Objective[] objectives, org.clafer.assertion.Assertion[] assertions) {
        List<String> result = new ArrayList<>();
        try {
            ClaferOption option = js.getOption();
            ClaferSearch search = assertions.length > 0
                    ? ClaferCompiler.compile(model, scope, assertions, option)
                    : objectives.length > 0
                    ? ClaferCompiler.compile(model, scope, objectives, option)
                    : ClaferCompiler.compile(model, scope, option);
            while (result.size() < Instances && search.find()) {
                result.add(search.instance().toString());
            }
        } catch (Exception e) {
            // A parsed model has spans, so its type errors start with a position.
            result.add(e.getClass().getName() + ": " + String.valueOf(e.getMessage()).replaceFirst("^\\d+:\\d+: ", ""));
        }
        return result;
    }

    @Test
    public void testSomeModels() throws Exception {
        assertTrue(testFiles().size() > 100);
    }
}
