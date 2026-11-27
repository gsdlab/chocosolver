package org.clafer.frontend;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import static org.junit.Assert.assertEquals;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.TemporaryFolder;

/**
 * The scope files and the files of models with imports, on small models, each
 * as the compiler gives them ({@code clafer -m clafer --meta-data}).
 */
public class ScopesTest {

    @Rule
    public TemporaryFolder folder = new TemporaryFolder();

    private static String scopes(String... clafers) {
        return scopes(-128, 127, 16, 0, "-21474835, 21474835", clafers);
    }

    private static String scopes(long low, long high, int stringLength, int digits, String decimal,
            String... clafers) {
        StringBuilder sb = new StringBuilder("[\n");
        for (int i = 0; i < clafers.length; i += 2) {
            sb.append("{\"lpqName\": \"").append(clafers[i]).append("\", \"scope\": ").append(clafers[i + 1])
                    .append("},\n");
        }
        return sb + "{\"defaultScope\": 1},\n{\"int\": [" + low + ", " + high + "]},\n{\"mul\": [" + low + ", " + high
                + "]},\n{\"stringLength\": " + stringLength + "},\n{\"chars\": [32, 126]},\n{\"decimalDigits\": " + digits
                + "},\n{\"decimal\": [" + decimal + "]}\n]\n";
    }

    private static String scopeFile(String model) {
        ToIr.Result result = ToIr.toIr(model);
        assertEquals(List.of(), result.messages());
        return result.scopes().orElseThrow().scopeFile();
    }

    /**
     * The scopes of the clafers and the ranges of the options.
     */
    @Test
    public void testScopeFile() {
        assertEquals(scopes(-5, 20, 16, 0, "-21474835, 21474835", "A", "3", "n", "3"),
                scopeFile("//# OPTIONS --minint=-5 --maxint=20\nA 2..3\n  n -> int\n[sum A.n > 4]\n"));
        assertEquals(scopes(-128, 50, 16, 0, "-21474835, 21474835", "A", "1", "x", "1"),
                scopeFile("//# OPTIONS --maxint 50\nA\n  x -> integer\n"));
        assertEquals(scopes(-128, 127, 16, 3, "-21474.835, 21474.835", "A", "1"),
                scopeFile("//# OPTIONS --decimal-digits=3\nA -> decimal\n[A = 1.5]\n"));
        assertEquals(scopes(-128, 127, 16, 2, "-214748.35, 214748.35", "A", "1"),
                scopeFile("//# OPTIONS --decimal-digits=2\nA\n"));
        String model = "abstract Fe\nGr : Fe 3\n  h 2\n[#Gr = 3]\n";
        assertEquals(scopes("Fe", "3", "Gr", "3", "h", "6"), scopeFile(model));
        assertEquals(scopes("Fe", "1", "Gr", "1", "h", "1"), scopeFile("//# OPTIONS --ss=none\n" + model));
        assertEquals(scopes(-128, 127, 34, 0, "-21474835, 21474835", "name", "1"),
                scopeFile("name -> string\n[name = \"a very long string literal of 30\"]\n"));
    }

    private String write(String name, String text) throws IOException {
        Path p = folder.getRoot().toPath().resolve(name);
        Files.createDirectories(p.getParent());
        Files.writeString(p, text, StandardCharsets.UTF_8);
        return p.toString();
    }

    /**
     * A model with imports, relative to the importing file, with their
     * options combined.
     */
    @Test
    public void testImports() throws IOException {
        String model = write("imp1.cfr", "//# OPTIONS --import=sub/lib.cfr\nCar : Vehicle\n[some Car.wheel]\n");
        write("sub/lib.cfr", "//# OPTIONS --import=base.cfr --maxint=40\nabstract Vehicle\n  wheel 2..4\n");
        write("sub/base.cfr", "//# OPTIONS --minint=-30\nabstract Thing\nb\n  [some nothere]\n");
        ToIr.Result result = ToIr.read(model);
        String base = folder.getRoot() + "/sub/base.cfr";
        assertEquals(List.of(base + ":4:9: Name resolver: 'nothere' not found within paths:b\nin context of '\"b\"'"),
                result.messages());
        write("sub/base.cfr", "//# OPTIONS --minint=-30\nabstract Thing\n");
        result = ToIr.read(model);
        assertEquals(List.of(), result.messages());
        assertEquals(scopes(-30, 40, 16, 0, "-21474835, 21474835", "Car", "1", "Thing", "0", "Vehicle", "1", "wheel", "4"),
                result.scopes().orElseThrow().scopeFile());
    }

    /**
     * The errors of the imports, as the compiler gives them.
     */
    @Test
    public void testImportErrors() throws IOException {
        String dir = folder.getRoot().toString();
        String missing = write("imp2.cfr", "//# OPTIONS --import=missing.cfr\nA\n");
        assertEquals(List.of(missing + ": the file " + dir + "/missing.cfr does not exist"), ToIr.read(missing).messages());
        String text = write("imp3.cfr", "//# OPTIONS --import=notes.txt\nA\n");
        assertEquals(List.of(text + ": the import " + dir + "/notes.txt is not a .cfr file"), ToIr.read(text).messages());
        String cycle = write("cyc1.cfr", "//# OPTIONS --import=cyc2.cfr\nA\n");
        write("cyc2.cfr", "//# OPTIONS --import=cyc1.cfr\nB\n");
        assertEquals(List.of("Import cycle: " + dir + "/cyc1.cfr imports " + dir + "/cyc2.cfr imports " + dir
                + "/cyc1.cfr"), ToIr.read(cycle).messages());
        String ss = write("imp4.cfr", "//# OPTIONS --import=ssn.cfr\nA\n");
        write("ssn.cfr", "//# OPTIONS --ss=none\nB\n");
        assertEquals(List.of("The option --ss must be the same in all files of the model: it differs in " + dir
                + "/ssn.cfr from " + ss), ToIr.read(ss).messages());
        String meta = write("imp5.cfr", "//# OPTIONS --import=meta.cfr\nA\n");
        write("meta.cfr", "//# OPTIONS --meta-data\nB\n");
        assertEquals(List.of(dir + "/meta.cfr: the option --meta-data can only be given in the compiled model or on the"
                + " command line, not in an imported file"), ToIr.read(meta).messages());
    }

    /**
     * A qualified name is its last name, and its errors are there.
     */
    @Test
    public void testQualifiedNames() {
        assertEquals(List.of("5:4: clafer \"b\" is not unique. Available paths:\nA.A__b\nC.C__b\n"),
                ToIr.toIr("A\n  b\nC\n  b\n[A\\b]\n").messages());
        assertEquals(List.of("5:16: Name resolver: 'A' not found within paths:B\nin context of '\"B\"'"),
                ToIr.toIr("A\n  b\n[some A\\b]\nB\n  [this.parent\\A = A]\n").messages());
        assertEquals(List.of(), ToIr.toIr("A\n  b\n[some A\\b]\n[some M\\A]\n").messages());
    }
}
