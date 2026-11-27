package org.clafer.lsp;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import static org.junit.Assume.assumeTrue;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import java.util.stream.Stream;
import org.clafer.frontend.ModelFiles;
import org.clafer.frontend.SourceSpan;
import org.clafer.frontend.Symbols.ClaferSymbol;
import org.clafer.frontend.ToIr;
import org.eclipse.lsp4j.Location;
import org.eclipse.lsp4j.Position;
import org.junit.Test;

/**
 * The table of names on clafer's positive models (at {@code -Dclafer.dir}):
 * each name the table records is written there as the name of its clafer,
 * and the server's go to definition at each name in the model's own file
 * goes to that clafer's name.
 */
public class NamesConformanceTest {

    private static List<Path> models() throws IOException {
        Path dir = Path.of(System.getProperty("clafer.dir", "../../clafer"), "test", "positive");
        assumeTrue("no clafer checkout at " + dir, Files.isDirectory(dir));
        try (Stream<Path> s = Files.list(dir)) {
            return s.filter(p -> p.toString().endsWith(".cfr") && !p.toString().endsWith(".des.cfr")
                    && !p.toString().endsWith(".core.cfr")).sorted().toList();
        }
    }

    private static String textAt(Positions p, SourceSpan s) {
        return p.text(p.toLsp(s));
    }

    @Test
    public void everyNameIsWrittenAsItsClaferAndGoesToIt() throws Exception {
        int models = 0, uses = 0, checked = 0;
        List<String> failures = new ArrayList<>();
        for (Path model : models()) {
            String path = model.toAbsolutePath().normalize().toString();
            ModelFiles files = ModelFiles.read(path, List.of());
            ToIr.Result r = ToIr.toIr(files);
            if (r.names().clafers().isEmpty()) {
                continue;
            }
            models++;
            Positions all = new Positions(files.text());
            String text = Files.readString(model);
            Analysis a = Analysis.of(Path.of(path).toUri().toString(), path, text, p -> Optional.empty());
            // the first line of the model's own file, after its imports
            int start = 1;
            for (ModelFiles.ModelFile f : files.files().subList(0, files.files().size() - 1)) {
                start += (int) f.text().chars().filter(ch -> ch == '\n').count();
            }
            for (var e : r.names().uses().entrySet()) {
                uses++;
                ClaferSymbol c = e.getValue();
                String written = textAt(all, e.getKey());
                if (!written.equals(c.name())) {
                    failures.add(model.getFileName() + " " + e.getKey() + ": '" + written + "' for " + c.uid());
                    continue;
                }
                if (!textAt(all, c.nameSpan()).equals(c.name())) {
                    failures.add(model.getFileName() + " declaration of " + c.uid() + " at " + c.nameSpan());
                }
                SourceSpan s = e.getKey();
                if (s.line() >= start && files.position(s.line()).path().equals(path)) {
                    checked++;
                    Position at = a.positions().toLsp(s.line() - start + 1, s.column());
                    Optional<Location> d = a.index().definition(at);
                    ModelFiles.Position declared = files.position(c.nameSpan().line());
                    boolean ok = d.isPresent() && d.get().getRange().getStart().getLine() == declared.line() - 1
                            && Path.of(java.net.URI.create(d.get().getUri())).toString().equals(declared.path());
                    if (!ok) {
                        failures.add(model.getFileName() + " definition at " + s + " of " + c.uid() + ": " + d);
                    }
                }
            }
        }
        assertEquals(String.join("\n", failures), 0, failures.size());
        assertTrue("models " + models, models > 150);
        assertTrue("uses " + uses, uses > 1000);
        System.out.println("NamesConformanceTest: " + models + " models, " + uses + " names, " + checked
                + " definitions checked");
    }

    /**
     * Renames each clafer of each model without imports, applies the edits,
     * and compiles the result: it has no errors, and its names resolve as
     * before, to clafers of the same new names.
     */
    @Test
    public void renamingEveryClaferKeepsTheModel() throws Exception {
        int renames = 0;
        List<String> failures = new ArrayList<>();
        for (Path model : models()) {
            String path = model.toAbsolutePath().normalize().toString();
            String text = Files.readString(model);
            ModelFiles files = ModelFiles.read(path, List.of());
            if (files.files().size() != 1) {
                continue;
            }
            ToIr.Result r = ToIr.toIr(files);
            if (r.names().clafers().isEmpty() || !r.diagnostics().isEmpty()) {
                continue;
            }
            String uri = Path.of(path).toUri().toString();
            Analysis a = Analysis.of(uri, path, text, p -> Optional.empty());
            java.util.Set<String> renamed = new java.util.HashSet<>();
            for (ClaferSymbol c : r.names().clafers()) {
                // one clafer of each name; a clafer use takes its super's name
                if (!renamed.add(c.name()) || c.declaration() instanceof org.clafer.frontend.Syntax.ClaferUse) {
                    continue;
                }
                Position at = a.positions().toLsp(c.nameSpan().line(), c.nameSpan().column());
                Optional<Location> d = a.index().definition(at);
                if (d.isEmpty()) {
                    failures.add(model.getFileName() + " " + c.uid() + ": no declaration");
                    continue;
                }
                String newName = "zq" + c.name().replace("'", "");
                if (r.names().clafers().stream().anyMatch(x -> x.name().equals(newName))) {
                    continue;
                }
                renames++;
                List<Location> edits = a.index().renameOccurrences(d.get());
                String after = apply(a.positions(), text, edits, newName);
                ToIr.Result r2 = ToIr.toIr(ModelFiles.of(path, after));
                if (!r2.diagnostics().isEmpty()) {
                    failures.add(model.getFileName() + " " + c.uid() + " to " + newName + ": " + r2.diagnostics().get(0));
                    continue;
                }
                // the same names resolve, in the same order, to the clafers of the same names, renamed
                List<String> before = r.names().uses().values().stream()
                        .map(x -> x == c || (x.declaration() instanceof org.clafer.frontend.Syntax.ClaferUse
                                && x.superClafer().orElse(null) == c) ? newName : x.name()).toList();
                List<String> now = r2.names().uses().values().stream().map(ClaferSymbol::name).toList();
                if (!before.equals(now)) {
                    failures.add(model.getFileName() + " " + c.uid() + " to " + newName + ": " + before + " vs " + now);
                }
            }
        }
        assertEquals(String.join("\n", failures), 0, failures.size());
        assertTrue("renames " + renames, renames > 1000);
        System.out.println("NamesConformanceTest: " + renames + " renames");
    }

    private static String apply(Positions p, String text, List<Location> edits, String newName) {
        List<Location> sorted = new ArrayList<>(edits);
        sorted.sort(java.util.Comparator.comparing((Location l) -> l.getRange().getStart().getLine())
                .thenComparing(l -> l.getRange().getStart().getCharacter()).reversed());
        StringBuilder sb = new StringBuilder(text);
        for (Location l : sorted) {
            sb.replace(offset(p, l.getRange().getStart()), offset(p, l.getRange().getEnd()), newName);
        }
        return sb.toString();
    }

    private static int offset(Positions p, Position pos) {
        int o = 0;
        for (int i = 0; i < pos.getLine(); i++) {
            o += p.line(i).length() + 1;
        }
        return o + pos.getCharacter();
    }
}
