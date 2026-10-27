package org.clafer.cli;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import org.clafer.ast.AstClafer;
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

    /*
     * abstract Feature
     * Port : Feature 3
     * abstract Component
     *     Cfeatures -> Feature *
     * Device : Component 2
     *     Cfeatures -> Feature *
     */
    private static final String ScopeFileModel
            = "scope({c0_Cfeatures:6, c0_Component:2, c0_Device:2, c0_Feature:3, c0_Port:3, c1_Cfeatures:6});\n"
            + "defaultScope(1);\n"
            + "c0_Feature = Abstract(\"c0_Feature\");\n"
            + "c0_Port = Clafer(\"c0_Port\").withCard(3, 3);\n"
            + "c0_Component = Abstract(\"c0_Component\");\n"
            + "c0_Cfeatures = c0_Component.addChild(\"c0_Cfeatures\");\n"
            + "c0_Device = Clafer(\"c0_Device\").withCard(2, 2);\n"
            + "c1_Cfeatures = c0_Device.addChild(\"c1_Cfeatures\");\n"
            + "c0_Port.extending(c0_Feature);\n"
            + "c0_Cfeatures.refToUnique(c0_Feature);\n"
            + "c0_Device.extending(c0_Component);\n"
            + "c1_Cfeatures.refToUnique(c0_Feature);\n";

    /**
     * The scopes of c0_Cfeatures and c1_Cfeatures after applying the given scope file.
     */
    private static List<Integer> cfeaturesScopes(String scopeFile) throws IOException {
        JavascriptFile javascriptFile = Javascript.readModel(ScopeFileModel);
        Scope scope = Utils.applyScopeFile(javascriptFile.getScope(), javascriptFile.getModel(), scopeFile, "test.cfr-scope");
        return cfeaturesScopes(javascriptFile, scope);
    }

    private static List<Integer> cfeaturesScopes(JavascriptFile javascriptFile, Scope scope) {
        List<AstClafer> clafers = Utils.getAllModelClafers(javascriptFile.getModel());
        return List.of(scope.getScope(Utils.claferNamed(clafers, "c0_Cfeatures").get(0)),
                       scope.getScope(Utils.claferNamed(clafers, "c1_Cfeatures").get(0)));
    }

    @Test
    public void testScopeFileNames() throws IOException {
        // a name sets the scopes of all the clafers whose containment paths end with it
        assertEquals(List.of(2, 2), cfeaturesScopes("[{\"lpqName\": \"Cfeatures\", \"scope\": 2}]"));
        assertEquals(List.of(2, 6), cfeaturesScopes("[{\"lpqName\": \"Component::Cfeatures\", \"scope\": 2}]"));
        assertEquals(List.of(6, 2), cfeaturesScopes("[\n{\n\"lpqName\": \"::Device::Cfeatures\",\n\"scope\": 2\n}\n]"));
        // UIDs, as the REPL saves them, also with the root's empty name
        assertEquals(List.of(6, 2), cfeaturesScopes("[{\"lpqName\": \"\", \"scope\": 1},\n{\"lpqName\": \"c1_Cfeatures\", \"scope\": 2}]"));
        assertEquals(List.of(6, 6), cfeaturesScopes("[]"));
    }

    @Test(expected = IllegalArgumentException.class)
    public void testScopeFileUnknownName() throws IOException {
        cfeaturesScopes("[{\"lpqName\": \"Missing::Cfeatures\", \"scope\": 2}]");
    }

    @Test(expected = IllegalArgumentException.class)
    public void testScopeFileMalformed() throws IOException {
        cfeaturesScopes("[{\"lpqName\": \"Cfeatures\", \"scope\": two}]");
    }

    @Test
    public void testScopeFileOfModel() throws IOException {
        Path dir = Files.createTempDirectory("scope-file");
        try {
            File model = dir.resolve("model.js").toFile();
            Files.writeString(model.toPath(), ScopeFileModel);
            JavascriptFile javascriptFile = Javascript.readModel(model);
            // no scope file
            assertEquals(List.of(6, 6), cfeaturesScopes(javascriptFile, Utils.resolveScopes(model, javascriptFile, Main.optionParser().parse())));
            // model.cfr-scope next to model.js
            Files.writeString(dir.resolve("model.cfr-scope"), "[{\"lpqName\": \"Cfeatures\", \"scope\": 2}]");
            assertEquals(List.of(2, 2), cfeaturesScopes(javascriptFile, Utils.resolveScopes(model, javascriptFile, Main.optionParser().parse())));
            // --scope-file instead
            Files.writeString(dir.resolve("other.cfr-scope"), "[{\"lpqName\": \"Cfeatures\", \"scope\": 4}]");
            assertEquals(List.of(4, 4), cfeaturesScopes(javascriptFile, Utils.resolveScopes(model, javascriptFile,
                    Main.optionParser().parse("--scope-file=" + dir.resolve("other.cfr-scope")))));
        } finally {
            for (File file : dir.toFile().listFiles())
                file.delete();
            dir.toFile().delete();
        }
    }
}
