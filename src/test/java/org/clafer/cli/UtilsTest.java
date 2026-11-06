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
            = "scope({$Component:2, $Component__Cfeatures:6, $Device:2, $Device__Cfeatures:6, $Feature:3, $Port:3});\n"
            + "defaultScope(1);\n"
            + "$Feature = Abstract(\"Feature\");\n"
            + "$Port = Clafer(\"Port\").withCard(3, 3);\n"
            + "$Component = Abstract(\"Component\");\n"
            + "$Component__Cfeatures = $Component.addChild(\"Component__Cfeatures\");\n"
            + "$Device = Clafer(\"Device\").withCard(2, 2);\n"
            + "$Device__Cfeatures = $Device.addChild(\"Device__Cfeatures\");\n"
            + "$Port.extending($Feature);\n"
            + "$Component__Cfeatures.refToUnique($Feature);\n"
            + "$Device.extending($Component);\n"
            + "$Device__Cfeatures.refToUnique($Feature);\n";

    /**
     * The scopes of Component__Cfeatures and Device__Cfeatures after applying the given scope file.
     */
    private static List<Integer> cfeaturesScopes(String scopeFile) throws IOException {
        JavascriptFile javascriptFile = Javascript.readModel(ScopeFileModel);
        Scope scope = Utils.applyScopeFile(javascriptFile.getScope(), javascriptFile.getModel(), scopeFile, "test.cfr-scope");
        return cfeaturesScopes(javascriptFile, scope);
    }

    private static List<Integer> cfeaturesScopes(JavascriptFile javascriptFile, Scope scope) {
        List<AstClafer> clafers = Utils.getAllModelClafers(javascriptFile.getModel());
        return List.of(scope.getScope(Utils.claferNamed(clafers, "Component__Cfeatures").get(0)),
                       scope.getScope(Utils.claferNamed(clafers, "Device__Cfeatures").get(0)));
    }

    @Test
    public void testScopeFileNames() throws IOException {
        // a name sets the scopes of all the clafers whose containment paths end with it
        assertEquals(List.of(2, 2), cfeaturesScopes("[{\"lpqName\": \"Cfeatures\", \"scope\": 2}]"));
        assertEquals(List.of(2, 6), cfeaturesScopes("[{\"lpqName\": \"Component__Cfeatures\", \"scope\": 2}]"));
        assertEquals(List.of(6, 2), cfeaturesScopes("[{\"lpqName\": \"root__Device__Cfeatures\", \"scope\": 2}]"));
        // the older form with ::
        assertEquals(List.of(2, 6), cfeaturesScopes("[{\"lpqName\": \"Component::Cfeatures\", \"scope\": 2}]"));
        assertEquals(List.of(6, 2), cfeaturesScopes("[\n{\n\"lpqName\": \"::Device::Cfeatures\",\n\"scope\": 2\n}\n]"));
        // a qualified name of a clafer whose UID is its name: Port in the root
        assertEquals(1, Utils.claferNamed(Utils.getAllModelClafers(Javascript.readModel(ScopeFileModel).getModel()), "root__Port").size());
        // UIDs, as the REPL saves them, also with the root's empty name
        assertEquals(List.of(6, 2), cfeaturesScopes("[{\"lpqName\": \"\", \"scope\": 1},\n{\"lpqName\": \"Device__Cfeatures\", \"scope\": 2}]"));
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
            // the file's integers replace the default -128..127, and the command line's replace the file's
            Files.writeString(dir.resolve("model.cfr-scope"), "[{\"int\": [-8, 7]}]");
            Scope scope = Utils.resolveScopes(model, javascriptFile, Main.optionParser().parse());
            assertEquals(List.of(-8, 7), List.of(scope.getIntLow(), scope.getIntHigh()));
            scope = Utils.resolveScopes(model, javascriptFile, Main.optionParser().parse("--minint=-2"));
            assertEquals(List.of(-2, 7), List.of(scope.getIntLow(), scope.getIntHigh()));
            scope = Utils.resolveScopes(model, javascriptFile, Main.optionParser().parse("--maxint=3"));
            assertEquals(List.of(-4, 3), List.of(scope.getIntLow(), scope.getIntHigh()));
        } finally {
            for (File file : dir.toFile().listFiles())
                file.delete();
            dir.toFile().delete();
        }
    }
}
