package org.clafer.lsp;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import org.clafer.frontend.SourceSpan;
import org.clafer.frontend.Syntax;
import org.eclipse.lsp4j.DocumentSymbol;
import org.eclipse.lsp4j.Position;
import org.eclipse.lsp4j.Range;
import org.eclipse.lsp4j.SymbolKind;

/**
 * The outline of a document (document symbols), from its syntax tree: the
 * clafers, enumerations, constraints, assertions and objectives, nested as
 * they are written.
 */
final class Outline {

    /**
     * The longest name of a constraint, assertion or objective, which is its
     * first line.
     */
    private static final int MaxName = 60;

    private final Positions positions;

    private Outline(Positions positions) {
        this.positions = positions;
    }

    /**
     * @param model the tree of a document
     * @param positions the positions in the document
     * @return its symbols, in order
     */
    static List<DocumentSymbol> of(Syntax.Model model, Positions positions) {
        Outline o = new Outline(positions);
        List<DocumentSymbol> symbols = new ArrayList<>();
        for (Syntax.Declaration d : model.declarations()) {
            o.declaration(d).ifPresent(symbols::add);
        }
        return symbols;
    }

    private Optional<DocumentSymbol> declaration(Syntax.Declaration d) {
        return switch (d) {
            case Syntax.EnumDecl e -> {
                DocumentSymbol s = symbol(e.name().text(), SymbolKind.Enum, e.span(), e.name().span());
                List<DocumentSymbol> literals = new ArrayList<>();
                for (Syntax.Ident l : e.literals()) {
                    literals.add(symbol(l.text(), SymbolKind.EnumMember, l.span(), l.span()));
                }
                s.setChildren(literals);
                yield Optional.of(s);
            }
            case Syntax.Clafer c -> {
                DocumentSymbol s = symbol(c.name().text(), c.isAbstract() ? SymbolKind.Class : SymbolKind.Field,
                        c.span(), c.name().span());
                String detail = detail(c);
                s.setDetail(detail.isEmpty() ? null : detail);
                s.setChildren(elements(c.elements()));
                yield Optional.of(s);
            }
            case Syntax.ClaferUse u -> {
                DocumentSymbol s = symbol("`" + text(u.name().span()), SymbolKind.Field, u.span(), u.name().span());
                u.card().ifPresent(card -> s.setDetail(text(card.span())));
                s.setChildren(elements(u.elements()));
                yield Optional.of(s);
            }
            case Syntax.Constraint c -> Optional.of(firstLine(SymbolKind.Event, c.span()));
            case Syntax.Assertion a -> Optional.of(firstLine(SymbolKind.Boolean, a.span()));
            case Syntax.Goal g -> Optional.of(firstLine(SymbolKind.Operator, g.span()));
        };
    }

    private List<DocumentSymbol> elements(Optional<Syntax.Elements> elements) {
        List<DocumentSymbol> children = new ArrayList<>();
        elements.ifPresent(es -> es.elements().forEach(e -> declaration(e).ifPresent(children::add)));
        return children;
    }

    /**
     * The super, the reference and the card of a clafer, as written.
     */
    private String detail(Syntax.Clafer c) {
        List<String> parts = new ArrayList<>();
        // the span of a super is its colon; its expression follows
        c.superClafer().ifPresent(s -> parts.add(text(new SourceSpan(s.span().line(), s.span().column(),
                s.exp().span().endLine(), s.exp().span().endColumn()))));
        c.reference().ifPresent(r -> parts.add(text(r.span())));
        c.card().ifPresent(card -> parts.add(text(card.span())));
        return String.join(" ", parts);
    }

    /**
     * A symbol named by the first line of its span.
     */
    private DocumentSymbol firstLine(SymbolKind kind, SourceSpan span) {
        Range range = positions.toLsp(span);
        Position start = range.getStart();
        Position end = range.getEnd().getLine() == start.getLine() ? range.getEnd()
                : new Position(start.getLine(), positions.line(start.getLine()).length());
        Range selection = new Range(start, end);
        String name = positions.text(selection).strip();
        if (name.length() > MaxName) {
            name = name.substring(0, MaxName - 1) + "…";
        }
        return new DocumentSymbol(name.isEmpty() ? "[ ]" : name, kind, range, selection);
    }

    private DocumentSymbol symbol(String name, SymbolKind kind, SourceSpan span, SourceSpan nameSpan) {
        Range range = positions.toLsp(span);
        Range selection = positions.toLsp(nameSpan);
        // the selection must be in the range, which it is unless the parser recovered oddly
        if (!contains(range, selection)) {
            selection = new Range(range.getStart(), range.getStart());
        }
        return new DocumentSymbol(name.isEmpty() ? "?" : name, kind, range, selection);
    }

    private static boolean contains(Range outer, Range inner) {
        return !before(inner.getStart(), outer.getStart()) && !before(outer.getEnd(), inner.getEnd());
    }

    private static boolean before(Position a, Position b) {
        return a.getLine() < b.getLine() || (a.getLine() == b.getLine() && a.getCharacter() < b.getCharacter());
    }

    private String text(SourceSpan span) {
        return positions.text(positions.toLsp(span)).replaceAll("\\s+", " ").strip();
    }
}
