package org.clafer.lsp;

import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.IdentityHashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import org.clafer.frontend.ModelFiles;
import org.clafer.frontend.Names;
import org.clafer.frontend.SourceSpan;
import org.clafer.frontend.Symbols.ClaferSymbol;
import org.clafer.frontend.Symbols.GroupCard;
import org.clafer.frontend.Syntax;
import org.eclipse.lsp4j.Location;
import org.eclipse.lsp4j.Position;
import org.eclipse.lsp4j.Range;

/**
 * The names of a document's model and what they refer to: the clafers, by
 * the resolver's table ({@link Names}), in this document and in its
 * imports, and the locals of the quantifiers and {@code let}s of this
 * document, by their scopes in its tree.
 * <p>
 * The names of a model with errors before the resolver (a syntax error, an
 * error of the symbols) are not known, so the index has only the locals.
 */
final class Index {

    /**
     * What a name refers to: a clafer, or a local of a quantifier or a
     * {@code let} of this document.
     */
    sealed interface Target permits Clafer, Local {
    }

    record Clafer(ClaferSymbol symbol) implements Target {
    }

    /**
     * @param declaration the local where it is declared, in this document
     * @param declaredBy the quantifier or {@code let} that declares it
     */
    record Local(Syntax.Ident declaration, Syntax.Exp declaredBy) implements Target {
    }

    /**
     * A file of the model: its path, its URI, its text's positions, and its
     * first line in the model.
     */
    private record File(String path, String uri, Positions positions, int start) {
    }

    /**
     * A name in a file of the model and what it refers to.
     */
    private record Occurrence(File file, SourceSpan span, Target target, boolean declaration) {
    }

    private final String path;
    private final List<File> files = new ArrayList<>();
    private final File self;
    private final List<Occurrence> occurrences = new ArrayList<>();
    private final Map<ClaferSymbol, Occurrence> declarations = new IdentityHashMap<>();
    private final List<ClaferSymbol> clafers;

    /**
     * @param uri the document's URI
     * @param path the document's path, as the model's files name it
     * @param positions the positions of the document
     * @param tree the document's own tree, for its locals
     * @param model the files of the model, or empty if they could not be read
     * @param names the names of the model
     */
    Index(String uri, String path, Positions positions, Syntax.Model tree, Optional<ModelFiles> model, Names names) {
        this.path = path;
        this.clafers = names.clafers();
        int start = 1;
        File own = null;
        for (ModelFiles.ModelFile f : model.map(ModelFiles::files).orElse(List.of())) {
            boolean isSelf = f.path().equals(path);
            File file = new File(f.path(), isSelf ? uri : uri(f.path()), isSelf ? positions : new Positions(f.text()),
                    start);
            files.add(file);
            if (isSelf) {
                own = file;
            }
            start += (int) f.text().chars().filter(ch -> ch == '\n').count();
        }
        this.self = own != null ? own : new File(path, uri, positions, 1);
        if (own != null) {
            for (ClaferSymbol c : names.clafers()) {
                fileOf(c.nameSpan()).ifPresent(f -> {
                    Occurrence o = new Occurrence(f, local(f, c.nameSpan()), new Clafer(c), true);
                    declarations.put(c, o);
                    occurrences.add(o);
                });
            }
            names.uses().forEach((span, c) -> fileOf(span).ifPresent(
                    f -> occurrences.add(new Occurrence(f, local(f, span), new Clafer(c), false))));
        }
        new Locals().model(tree);
    }

    private static String uri(String path) {
        try {
            return Path.of(path).toUri().toString();
        } catch (RuntimeException e) {
            return path;
        }
    }

    /**
     * @return the file of the model that a span of the model is in
     */
    private Optional<File> fileOf(SourceSpan span) {
        File found = null;
        for (File f : files) {
            if (f.start() <= span.line()) {
                found = f;
            }
        }
        return Optional.ofNullable(found);
    }

    /**
     * @return a span of the model, as a span of its file
     */
    private static SourceSpan local(File f, SourceSpan span) {
        int d = f.start() - 1;
        return new SourceSpan(span.line() - d, span.column(), span.endLine() - d, span.endColumn());
    }

    private Location location(Occurrence o) {
        return new Location(o.file().uri(), o.file().positions().toLsp(o.span()));
    }

    /**
     * @return the name at a position of this document, the name's end
     * included, so that a cursor right after a name is on it
     */
    private Optional<Occurrence> at(Position position) {
        int[] lc = self.positions().fromLsp(position);
        Occurrence found = null;
        for (Occurrence o : occurrences) {
            SourceSpan s = o.span();
            if (o.file() == self && s.line() == lc[0] && s.column() <= lc[1] && lc[1] <= s.endColumn()) {
                // the name of a clafer use, `A, declares a clafer and names its super: it is the super's
                if (found == null || (found.declaration() && !o.declaration())) {
                    found = o;
                }
            }
        }
        return Optional.ofNullable(found);
    }

    /**
     * @return the name at a position and what it refers to
     */
    Optional<Target> targetAt(Position position) {
        return at(position).map(Occurrence::target);
    }

    /**
     * @return the range of the name at a position
     */
    Optional<Range> rangeAt(Position position) {
        return at(position).map(o -> self.positions().toLsp(o.span()));
    }

    /**
     * @return where what the name at a position refers to is declared
     */
    Optional<Location> definition(Position position) {
        return targetAt(position).flatMap(this::declaration).map(this::location);
    }

    private Optional<Occurrence> declaration(Target t) {
        return switch (t) {
            case Clafer c -> Optional.ofNullable(declarations.get(c.symbol()));
            case Local l -> occurrences.stream().filter(o -> o.declaration() && o.target().equals(l)).findFirst();
        };
    }

    /**
     * @return every name in the model that refers to what the name at a
     * position refers to, in file and source order
     */
    List<Location> references(Position position, boolean includeDeclaration) {
        Optional<Target> t = targetAt(position);
        if (t.isEmpty()) {
            return List.of();
        }
        List<Occurrence> found = new ArrayList<>();
        for (Occurrence o : occurrences) {
            if (same(o.target(), t.get()) && (includeDeclaration || !o.declaration())) {
                found.add(o);
            }
        }
        found.sort(Comparator.<Occurrence>comparingInt(o -> files.indexOf(o.file()))
                .thenComparingInt(o -> o.span().line()).thenComparingInt(o -> o.span().column()));
        List<Location> locations = new ArrayList<>();
        Occurrence last = null;
        for (Occurrence o : found) {
            if (last == null || last.file() != o.file() || !last.span().equals(o.span())) {
                locations.add(location(o));
            }
            last = o;
        }
        return locations;
    }

    private static boolean same(Target a, Target b) {
        return a instanceof Clafer ca && b instanceof Clafer cb ? ca.symbol() == cb.symbol() : a.equals(b);
    }

    /**
     * @return the hover text of the name at a position, in Markdown
     */
    Optional<String> hover(Position position) {
        return targetAt(position).map(t -> switch (t) {
            case Clafer c -> hover(c.symbol());
            case Local l -> "```clafer\n" + localText(l) + "\n```\nlocal";
        });
    }

    private String hover(ClaferSymbol c) {
        StringBuilder sb = new StringBuilder("```clafer\n").append(declarationLine(c)).append("\n```\n");
        if (isLiteral(c)) {
            sb.append("literal of the enumeration ").append(c.superClafer().map(ClaferSymbol::uid).orElse(""));
        } else if (c.isEnum()) {
            sb.append("enumeration");
        } else {
            sb.append(c.isAbstract() ? "abstract clafer" : "clafer");
            c.parent().ifPresent(p -> sb.append(" in ").append(p.uid()));
        }
        Optional<Occurrence> d = Optional.ofNullable(declarations.get(c));
        if (d.isPresent() && d.get().file() != self) {
            sb.append(", declared in `").append(d.get().file().path()).append(':').append(d.get().span().line())
                    .append('`');
        }
        return sb.toString();
    }

    /**
     * The clafer's line in the rich explicit model, without its initializer
     * and modifiers: abstract, the group card, the UID, the super, the
     * reference and the card, with the defaults applied.
     */
    private static boolean isLiteral(ClaferSymbol c) {
        return c.declaration() instanceof Syntax.Ident;
    }

    private String declarationLine(ClaferSymbol c) {
        if (isLiteral(c)) {
            return c.uid() + " : " + c.superClafer().map(ClaferSymbol::uid).orElse("?");
        }
        if (c.isEnum()) {
            List<String> literals = new ArrayList<>();
            for (ClaferSymbol l : clafers.stream().filter(x -> x.superClafer().orElse(null) == c
                    && isLiteral(x)).toList()) {
                literals.add(l.uid());
            }
            return "enum " + c.uid() + (literals.isEmpty() ? "" : " = " + String.join(" | ", literals));
        }
        StringBuilder sb = new StringBuilder();
        if (c.isAbstract()) {
            sb.append("abstract ");
        }
        GroupCard g = c.groupCard();
        sb.append(g.interval()).append(' ').append(c.uid());
        c.superClafer().ifPresent(s -> sb.append(" : ").append(s.uid()));
        c.reference().ifPresent(r -> sb.append(r.isSet() ? " -> " : " ->> ")
                .append(r.hasType() ? r.typeName() : written(r.span())));
        sb.append(' ').append(c.card());
        return sb.toString();
    }

    /**
     * @return a span of the model as it is written
     */
    private String written(SourceSpan span) {
        return fileOf(span).map(f -> f.positions().text(f.positions().toLsp(local(f, span)))
                .replaceAll("\\s+", " ").strip()).orElse("?");
    }

    private String localText(Local l) {
        Positions p = self.positions();
        return switch (l.declaredBy()) {
            case Syntax.Quantified q -> l.declaration().text() + " : " + text(p, q.decl().exp().span());
            case Syntax.Let let -> "let " + l.declaration().text() + " = " + text(p, let.value().span());
            default -> l.declaration().text();
        };
    }

    private static String text(Positions p, SourceSpan span) {
        return p.text(p.toLsp(span)).replaceAll("\\s+", " ").strip();
    }

    /**
     * Finds the locals of this document and their uses, by scope: a local of
     * a quantifier is in its body, and a local of a {@code let} in the
     * expression after {@code in}. A local hides a clafer of its name, as in
     * the resolver, where the locals come right after the special names.
     */
    private final class Locals {

        private final List<Local> scope = new ArrayList<>();

        void model(Syntax.Model m) {
            m.declarations().forEach(this::declaration);
        }

        private void declaration(Syntax.Declaration d) {
            switch (d) {
                case Syntax.EnumDecl e -> {
                }
                case Syntax.Clafer c -> {
                    c.superClafer().ifPresent(s -> exp(s.exp()));
                    c.reference().ifPresent(r -> exp(r.exp()));
                    c.init().ifPresent(i -> exp(i.exp()));
                    c.transition().ifPresent(t -> {
                        t.arrow().guard().ifPresent(this::exp);
                        exp(t.exp());
                    });
                    c.elements().ifPresent(es -> es.elements().forEach(this::declaration));
                }
                case Syntax.ClaferUse u -> u.elements().ifPresent(es -> es.elements().forEach(this::declaration));
                case Syntax.Constraint c -> c.exps().forEach(this::exp);
                case Syntax.Assertion a -> a.exps().forEach(this::exp);
                case Syntax.Goal g -> g.exps().forEach(this::exp);
            }
        }

        private void declare(Syntax.Ident ident, Syntax.Exp by) {
            Local l = new Local(ident, by);
            occurrences.add(new Occurrence(self, ident.span(), l, true));
            scope.add(l);
        }

        private void exp(Syntax.Exp e) {
            switch (e) {
                case Syntax.Quantified q -> {
                    exp(q.decl().exp());
                    int mark = scope.size();
                    q.decl().locals().forEach(l -> declare(l, q));
                    exp(q.body());
                    unwind(mark);
                }
                case Syntax.Let l -> {
                    exp(l.value());
                    int mark = scope.size();
                    declare(l.local(), l);
                    exp(l.body());
                    unwind(mark);
                }
                case Syntax.ClaferId id -> {
                    if (id.name().path().size() == 1) {
                        Syntax.Ident name = id.name().path().get(0);
                        for (int i = scope.size() - 1; i >= 0; i--) {
                            if (scope.get(i).declaration().text().equals(name.text())) {
                                // the resolver binds it, so it is no clafer
                                occurrences.removeIf(o -> o.file() == self && !o.declaration()
                                        && o.span().equals(name.span()));
                                occurrences.add(new Occurrence(self, name.span(), scope.get(i), false));
                                break;
                            }
                        }
                    }
                }
                case Syntax.TransitionExp t -> {
                    exp(t.from());
                    t.arrow().guard().ifPresent(this::exp);
                    exp(t.to());
                }
                case Syntax.IfThenElse i -> {
                    exp(i.condition());
                    exp(i.then());
                    exp(i.otherwise());
                }
                case Syntax.Pattern p -> {
                    p.exps().forEach(this::exp);
                    p.scope().ifPresent(s -> s.exps().forEach(this::exp));
                }
                case Syntax.Anchor a -> exp(a.exp());
                case Syntax.Binary b -> {
                    exp(b.left());
                    // after a dot, a name is a child, not a local
                    if (b.op() != Syntax.BinaryOp.Join) {
                        exp(b.right());
                    } else if (!(b.right() instanceof Syntax.ClaferId)) {
                        exp(b.right());
                    }
                }
                case Syntax.Unary u -> exp(u.exp());
                case Syntax.QuantExp q -> exp(q.exp());
                case Syntax.Literal l -> {
                }
            }
        }

        private void unwind(int mark) {
            while (scope.size() > mark) {
                scope.remove(scope.size() - 1);
            }
        }
    }
}
