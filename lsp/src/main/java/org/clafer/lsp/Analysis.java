package org.clafer.lsp;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import java.util.function.Function;
import org.clafer.frontend.ClaferParser;
import org.clafer.frontend.Diagnostic;
import org.clafer.frontend.ModelFiles;
import org.clafer.frontend.SourceSpan;
import org.clafer.frontend.ToIr;
import org.eclipse.lsp4j.DiagnosticSeverity;
import org.eclipse.lsp4j.DocumentSymbol;
import org.eclipse.lsp4j.Position;
import org.eclipse.lsp4j.Range;

/**
 * What the server knows of one version of a document: its tree and its
 * diagnostics. A document is analyzed again, completely, on every change.
 *
 * @param text the text of the document
 * @param positions the positions in it
 * @param parsed the tree of the document alone, with its comments and
 * parse errors; what was read around the errors
 * @param diagnostics the LSP diagnostics of the model of the document, its
 * imports included
 */
public record Analysis(String text, Positions positions, ClaferParser.Result parsed,
        List<org.eclipse.lsp4j.Diagnostic> diagnostics) {

    /**
     * The name of the server in the diagnostics.
     */
    static final String Source = "clafer";

    /**
     * Analyzes a document.
     *
     * @param path the document's file, absolute, or a name if it has no file
     * @param text the document's text
     * @param open the text of the open documents, by the path that a model
     * names them with; the files that are not open are read from disk
     * @return the analysis
     */
    public static Analysis of(String path, String text, Function<String, Optional<String>> open) {
        Positions positions = new Positions(text);
        ClaferParser.Result parsed = ClaferParser.parse(text);
        Function<String, Optional<String>> withThis = p -> p.equals(path) ? Optional.of(text) : open.apply(p);
        ModelFiles files = ModelFiles.read(path, List.of(), withThis);
        ToIr.Result result;
        try {
            result = ToIr.toIr(files);
        } catch (RuntimeException e) {
            // a bug of the frontend: shown on the first line rather than lost
            result = null;
        }
        List<org.eclipse.lsp4j.Diagnostic> diagnostics = new ArrayList<>();
        if (result == null) {
            diagnostics.add(diagnostic(firstLine(positions), "The Java frontend failed on this model; please report it"));
        } else {
            for (Diagnostic d : result.diagnostics()) {
                diagnostics.add(toLsp(d, files, path, positions));
            }
        }
        return new Analysis(text, positions, parsed, List.copyOf(diagnostics));
    }

    /**
     * A diagnostic of the model at its place in this document: where it is
     * if it is in this document, else on the first line, with its file and
     * line in the message.
     */
    private static org.eclipse.lsp4j.Diagnostic toLsp(Diagnostic d, ModelFiles files, String path,
            Positions positions) {
        SourceSpan s = d.span();
        if (files.files().isEmpty()) {
            return diagnostic(firstLine(positions), d.message());
        }
        ModelFiles.Position start = files.position(s.line());
        if (!start.path().equals(path)) {
            return diagnostic(firstLine(positions), "in " + files.format(d));
        }
        ModelFiles.Position end = files.position(s.endLine());
        SourceSpan local = end.path().equals(path) && end.line() >= start.line()
                ? new SourceSpan(start.line(), s.column(), end.line(),
                        end.line() == start.line() ? Math.max(s.endColumn(), s.column()) : s.endColumn())
                : SourceSpan.at(start.line(), s.column());
        return diagnostic(positions.toVisibleLsp(local), d.message());
    }

    private static Range firstLine(Positions positions) {
        return new Range(new Position(0, 0), new Position(0, positions.line(0).length()));
    }

    private static org.eclipse.lsp4j.Diagnostic diagnostic(Range range, String message) {
        return new org.eclipse.lsp4j.Diagnostic(range, message, DiagnosticSeverity.Error, Source);
    }

    /**
     * @return the outline of the document
     */
    public List<DocumentSymbol> outline() {
        return Outline.of(parsed.model(), positions);
    }
}
