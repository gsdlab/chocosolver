package org.clafer.frontend;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.util.List;
import org.clafer.ast.ToAst;
import org.clafer.javascript.Javascript;
import org.clafer.javascript.JavascriptFile;
import org.clafer.scope.Scope;
import org.clafer.scope.ScopeFile;
import static org.junit.Assert.assertEquals;
import static org.junit.Assume.assumeTrue;
import org.junit.Test;

/**
 * Step 9b end to end: a model in the full syntax whose constraints are
 * explicit ({@code ACCDemo_attributedFeatureModels.cfr}: layout, no braces,
 * no inferred cards or group cards) gives, through {@link ClaferParser},
 * {@link Symbols} and {@link ToIr}, the rich explicit model that the compiler
 * gives for it, {@code test/regression/ACCDemo_attributedFeatureModels.des.cfr.reg}
 * (the compiler gives that file for the rewrite too), and chocosolver solves
 * it as the compiler's Choco output of the original model.
 */
public class FullExplicitModelTest {

    private static final String Name = "ACCDemo_attributedFeatureModels";

    private static String resource(String name) throws IOException {
        try (InputStream in = FullExplicitModelTest.class.getResourceAsStream(name)) {
            return new String(in.readAllBytes(), StandardCharsets.UTF_8);
        }
    }

    @Test
    public void testEndToEnd() throws Exception {
        File regression = new File(ExplicitCoreTest.regressions(), Name + ".des.cfr.reg");
        assumeTrue(regression.isFile());
        ToIr.Result ir = ToIr.toIr(resource(Name + ".cfr"));
        assertEquals(List.of(), ir.diagnostics());
        Ir.Model model = ir.model();
        assertEquals(Files.readString(regression.toPath()), ExplicitPrinter.print(model));

        ToAst.Result built = ToAst.toAst(model);
        assertEquals(List.of(), built.diagnostics());
        JavascriptFile js = Javascript.readModel(new File(ExplicitCoreTest.regressions(), Name + ".js.reg"));
        Scope scope = ScopeFile.parse(ScopeFile.print(js.getModel(), js.getScope()), "model.cfr-scope")
                .applyTo(js.getScope(), built.model());
        List<String> expected = ExplicitCoreTest.solve(js.getModel(), js.getScope(), js.getObjectives(),
                js.getAssertions(), js.getOption());
        List<String> actual = ExplicitCoreTest.solve(built.model(), scope, built.objectives(), built.assertions(),
                js.getOption());
        assertEquals(true, !expected.isEmpty());
        assertEquals(ExplicitCoreTest.subs(js.getModel()), ExplicitCoreTest.subs(built.model()));
        assertEquals(expected, actual);
    }
}
