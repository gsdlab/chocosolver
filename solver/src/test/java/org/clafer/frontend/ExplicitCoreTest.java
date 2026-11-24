package org.clafer.frontend;

import java.io.File;
import java.nio.file.Files;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;
import org.clafer.assertion.Assertion;
import org.clafer.ast.AstAbstractClafer;
import org.clafer.ast.AstClafer;
import org.clafer.ast.AstModel;
import org.clafer.ast.AstUtil;
import org.clafer.ast.ToAst;
import org.clafer.compiler.ClaferCompiler;
import org.clafer.compiler.ClaferOption;
import org.clafer.compiler.ClaferSearch;
import org.clafer.javascript.Javascript;
import org.clafer.javascript.JavascriptFile;
import org.clafer.objective.Objective;
import org.clafer.scope.Scope;
import org.clafer.scope.ScopeFile;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import static org.junit.Assume.assumeTrue;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Parameterized;
import org.junit.runners.Parameterized.Parameter;
import org.junit.runners.Parameterized.Parameters;

/**
 * chocosolver on the explicit core and rich models of Clafer's positive tests
 * ({@code test/regression/*.core.cfr.reg} and {@code *.des.cfr.reg}, in a
 * Clafer checkout next to chocosolver or at {@code -Dclafer.dir}): every
 * static model, the rich ones lowered by {@link ToCore}, builds
 * without errors, and solves as the compiler's Choco output of the same model
 * ({@code *.js.reg}) does: the same first instances (optimal instances,
 * counterexamples) in the same order, or both the same error, with the scopes
 * of the Choco output.
 */
@RunWith(Parameterized.class)
public class ExplicitCoreTest {

    private static final int Instances = 20;

    @Parameter
    public File coreFile;

    static File regressions() {
        return new File(System.getProperty("clafer.dir", "../../clafer"), "test/regression");
    }

    @Parameters(name = "{0}")
    public static List<File[]> files() {
        File[] files = regressions().listFiles((d, n) -> n.endsWith(".core.cfr.reg") || n.endsWith(".des.cfr.reg"));
        List<File[]> parameters = new ArrayList<>();
        if (files != null) {
            Arrays.sort(files);
            for (File f : files) {
                parameters.add(new File[]{f});
            }
        }
        return parameters;
    }

    private File jsFile() {
        return new File(coreFile.getParentFile(), coreFile.getName().replaceFirst("\\.(core|des)\\.cfr\\.reg$", ".js.reg"));
    }

    @Test
    public void testSolvesAsTheChocoOutput() throws Exception {
        String source = Files.readString(coreFile.toPath());
        ExplicitParser.Result tree = ExplicitParser.parse(source);
        assertEquals(List.of(), tree.diagnostics());
        boolean temporal = tree.model().options().traceLength().isPresent();
        boolean reals = source.matches("(?s).*(-> (real|double) |[0-9]e-?[0-9]).*");
        ToAst.Result built = ToAst.toAst(tree.model());
        if (temporal || reals) {
            // chocosolver solves neither
            assertTrue(coreFile.getName(), !built.diagnostics().isEmpty());
            return;
        }
        assertEquals(List.of(), built.diagnostics().stream().map(Diagnostic::toString).toList());
        // the Choco output of the model, if the compiler gives one
        assumeTrue(jsFile().isFile());
        JavascriptFile js = Javascript.readModel(jsFile());
        Scope scope = ScopeFile.parse(ScopeFile.print(js.getModel(), js.getScope()), "model.cfr-scope")
                .applyTo(js.getScope(), built.model());
        List<String> expected = solve(js.getModel(), js.getScope(), js.getObjectives(), js.getAssertions(), js.getOption());
        List<String> actual = solve(built.model(), scope, built.objectives(), built.assertions(), js.getOption());
        if (subs(js.getModel()).equals(subs(built.model()))) {
            assertEquals(expected, actual);
        } else {
            // The order of the subs of an abstract clafer numbers its instances.
            assertEquals(coreFile.getName(), expected.size(), actual.size());
        }
    }

    /**
     * The subs of each abstract clafer that has some, in order, by name.
     */
    static Map<String, List<String>> subs(AstModel model) {
        Map<String, List<String>> subs = new TreeMap<>();
        for (AstClafer clafer : AstUtil.getClafers(model)) {
            if (clafer instanceof AstAbstractClafer abstractClafer && !abstractClafer.getSubs().isEmpty()) {
                subs.put(clafer.getName(), abstractClafer.getSubs().stream().map(AstClafer::getName).toList());
            }
        }
        return subs;
    }

    /**
     * The first instances, or the error, as text.
     */
    static List<String> solve(AstModel model, Scope scope, Objective[] objectives, Assertion[] assertions,
            ClaferOption option) {
        List<String> result = new ArrayList<>();
        try {
            ClaferSearch search = assertions.length > 0
                    ? ClaferCompiler.compile(model, scope, assertions, option)
                    : objectives.length > 0
                    ? ClaferCompiler.compile(model, scope, objectives, option)
                    : ClaferCompiler.compile(model, scope, option);
            while (result.size() < Instances && search.find()) {
                result.add(search.instance().toString());
            }
        } catch (Exception e) {
            // A built model has spans, so its type errors start with a position.
            result.add(e.getClass().getName() + ": " + String.valueOf(e.getMessage()).replaceFirst("^\\d+:\\d+: ", ""));
        }
        return result;
    }
}
