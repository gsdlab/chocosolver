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
     * Where each local is in scope: the body of its quantifier or let.
     */
    private final Map<Local, SourceSpan> localScopes = new java.util.LinkedHashMap<>();
    /**
     * The clafers of this document's tree, in pre-order.
     */
    private final List<Syntax.Clafer> syntaxClafers = new ArrayList<>();

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
        return sorted(found);
    }

    private List<Location> sorted(List<Occurrence> occurrences) {
        List<Occurrence> found = new ArrayList<>(occurrences);
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

    // ------------------------------------------------------------------
    // Rename
    // ------------------------------------------------------------------

    /**
     * @return whether the names of the model's clafers are known: the model
     * has no errors before the resolver
     */
    boolean hasClafers() {
        return !declarations.isEmpty();
    }

    /**
     * @return the declaration of what the name at a position refers to, and
     * the name as written, if it can be renamed: a clafer or a local
     */
    Optional<Location> renameTarget(Position position) {
        return definition(position);
    }

    /**
     * @return the name at a position, as written
     */
    Optional<String> nameAt(Position position) {
        return at(position).map(o -> o.file().positions().text(o.file().positions().toLsp(o.span())));
    }

    /**
     * @return whether the name at a position is a local
     */
    boolean isLocalAt(Position position) {
        return targetAt(position).filter(t -> t instanceof Local).isPresent();
    }

    /**
     * @param declaration where a clafer or a local is declared, as
     * {@link #definition} gives it
     * @param includeDeclaration whether the declaration is included
     * @return every name in this document's model that refers to it, in
     * file and source order
     */
    List<Location> occurrencesOf(Location declaration, boolean includeDeclaration) {
        for (Occurrence o : occurrences) {
            if (o.declaration() && sameLocation(location(o), declaration)) {
                Target t = o.target();
                List<Occurrence> found = occurrences.stream()
                        .filter(x -> same(x.target(), t) && (includeDeclaration || !x.declaration())).toList();
                return sorted(found);
            }
        }
        return List.of();
    }

    /**
     * @param declaration where a clafer or a local is declared
     * @return the names to change to rename it: its occurrences, and those
     * of the clafers that its uses ({@code `A}) declare, which have its name
     */
    List<Location> renameOccurrences(Location declaration) {
        List<Location> out = new ArrayList<>(occurrencesOf(declaration, true));
        for (Occurrence o : occurrences) {
            if (o.declaration() && sameLocation(location(o), declaration) && o.target() instanceof Clafer c) {
                for (ClaferSymbol u : clafers) {
                    Occurrence d = declarations.get(u);
                    if (u.declaration() instanceof Syntax.ClaferUse && u.superClafer().orElse(null) == c.symbol()
                            && d != null) {
                        for (Location l : occurrencesOf(location(d), true)) {
                            if (out.stream().noneMatch(x -> sameLocation(x, l))) {
                                out.add(l);
                            }
                        }
                    }
                }
                break;
            }
        }
        return out;
    }

    static boolean sameLocation(Location a, Location b) {
        return a.getRange().equals(b.getRange())
                && ClaferLanguageServer.path(a.getUri()).equals(ClaferLanguageServer.path(b.getUri()));
    }

    /**
     * @return whether a clafer of the model, or a local in scope at the
     * declaration of the local at a position, has a name
     */
    boolean nameTaken(Position position, String name) {
        if (clafers.stream().anyMatch(c -> c.name().equals(name))) {
            return true;
        }
        Optional<Target> t = targetAt(position);
        if (t.isPresent() && t.get() instanceof Local l) {
            SourceSpan at = l.declaration().span();
            return localScopes.entrySet().stream().anyMatch(e -> e.getKey().declaration().text().equals(name)
                    && (contains(e.getValue(), at) || e.getKey().declaredBy() == l.declaredBy()));
        }
        return false;
    }

    private static boolean contains(SourceSpan outer, SourceSpan inner) {
        return before(outer.line(), outer.column(), inner.line(), inner.column())
                && before(inner.endLine(), inner.endColumn(), outer.endLine(), outer.endColumn());
    }

    private static boolean before(int l1, int c1, int l2, int c2) {
        return l1 < l2 || (l1 == l2 && c1 <= c2);
    }

    private static boolean at(SourceSpan span, int[] lc) {
        return before(span.line(), span.column(), lc[0], lc[1]) && before(lc[0], lc[1], span.endLine(), span.endColumn());
    }

    // ------------------------------------------------------------------
    // Completion
    // ------------------------------------------------------------------

    /**
     * A name to complete: its label, what it is, its detail, and its rank,
     * from 0 (the most likely) up.
     */
    record Completion(String label, org.eclipse.lsp4j.CompletionItemKind kind, String detail, int rank) {
    }

    private static final java.util.regex.Pattern AfterDot = java.util.regex.Pattern.compile(
            "([A-Za-z_\\u00C0-\\u00FF][A-Za-z0-9_'\\u00C0-\\u00FF]*)\\s*\\.\\s*[A-Za-z0-9_'\\u00C0-\\u00FF]*$");

    /**
     * The words that start or join expressions and declarations, which
     * completion offers outside a path.
     */
    private static final List<String> Keywords = List.of(
            "abstract", "all", "assert", "disj", "else", "enum", "if", "in", "let", "lone", "max", "min",
            "mux", "no", "not", "one", "opt", "or", "product", "some", "sum", "then", "xor");

    /**
     * The names to complete at a position.
     *
     * @param position the position, in this document
     * @param known the index whose clafers are offered: this one, or the
     * latest of this document whose model had them, while this one has a
     * syntax error
     * @return the names, most likely first: after a dot, the children of
     * what is before it; otherwise the locals in scope, the special names,
     * the children of the enclosing clafers, the other clafers, and the
     * keywords
     */
    List<Completion> completion(Position position, Index known) {
        String line = self.positions().line(position.getLine());
        String before = line.substring(0, Math.min(position.getCharacter(), line.length()));
        int[] lc = self.positions().fromLsp(position);
        List<Completion> items = new ArrayList<>();
        java.util.Set<String> seen = new java.util.HashSet<>();
        Optional<ClaferSymbol> enclosing = enclosing(lc, known);
        java.util.regex.Matcher m = AfterDot.matcher(before);
        if (m.find()) {
            String base = m.group(1);
            int baseChar = m.start(1);
            List<ClaferSymbol> of = switch (base) {
                case "this" -> enclosing.map(List::of).orElse(List.of());
                case "parent" -> enclosing.flatMap(ClaferSymbol::parent).map(List::of).orElse(List.of());
                default -> {
                    Position basePosition = new Position(position.getLine(), baseChar);
                    if (targetAt(basePosition).isEmpty()) {
                        // the expression being written is broken, so its locals are not in the tree:
                        // they are in the tree of the text without the dot and what follows it
                        int dot = before.lastIndexOf('.');
                        String patched = patch(position.getLine(), dot, position.getCharacter());
                        Positions pp = new Positions(patched);
                        Index i = new Index(self.uri(), path, pp, org.clafer.frontend.ClaferParser.parse(patched).model(),
                                Optional.empty(), Names.None);
                        yield i.claferOf(basePosition, base, known);
                    }
                    yield claferOf(basePosition, base, known);
                }
            };
            boolean refs = false;
            for (ClaferSymbol c : of) {
                for (ClaferSymbol child : members(c)) {
                    if (seen.add(child.name())) {
                        items.add(new Completion(child.name(), org.eclipse.lsp4j.CompletionItemKind.Field,
                                known.declarationLine(child), 0));
                    }
                }
                refs |= hierarchy(c).stream().anyMatch(h -> h.reference().isPresent());
            }
            if (refs) {
                items.add(new Completion("dref", org.eclipse.lsp4j.CompletionItemKind.Keyword, "the reference's value", 1));
            }
            if (!of.isEmpty()) {
                items.add(new Completion("parent", org.eclipse.lsp4j.CompletionItemKind.Keyword, "the parent", 1));
            }
            return items;
        }
        for (Map.Entry<Local, SourceSpan> e : localScopes.entrySet()) {
            if (at(e.getValue(), lc) && seen.add(e.getKey().declaration().text())) {
                items.add(new Completion(e.getKey().declaration().text(), org.eclipse.lsp4j.CompletionItemKind.Variable,
                        localText(e.getKey()), 0));
            }
        }
        for (String special : List.of("this", "parent", "dref", "root")) {
            if (seen.add(special)) {
                items.add(new Completion(special, org.eclipse.lsp4j.CompletionItemKind.Keyword, null, 3));
            }
        }
        // the children of the enclosing clafer and of its ancestors, then every other clafer
        for (Optional<ClaferSymbol> c = enclosing; c.isPresent(); c = c.get().parent()) {
            for (ClaferSymbol child : members(c.get())) {
                if (seen.add(child.name())) {
                    items.add(new Completion(child.name(), org.eclipse.lsp4j.CompletionItemKind.Field,
                            known.declarationLine(child), 1));
                }
            }
        }
        for (ClaferSymbol c : known.clafers) {
            if (seen.add(c.name())) {
                items.add(new Completion(c.name(), c.isEnum() ? isLiteral(c) ? org.eclipse.lsp4j.CompletionItemKind.EnumMember
                        : org.eclipse.lsp4j.CompletionItemKind.Enum
                        : c.isAbstract() ? org.eclipse.lsp4j.CompletionItemKind.Class
                        : org.eclipse.lsp4j.CompletionItemKind.Field, known.declarationLine(c), 2));
            }
        }
        for (String k : Keywords) {
            if (seen.add(k)) {
                items.add(new Completion(k, org.eclipse.lsp4j.CompletionItemKind.Keyword, null, 4));
            }
        }
        return items;
    }

    /**
     * @return the text of this document without the characters of a line
     * from one character up to another
     */
    private String patch(int line, int from, int to) {
        StringBuilder sb = new StringBuilder();
        for (int i = 0; i < self.positions().lineCount(); i++) {
            String l = self.positions().line(i);
            sb.append(i == line ? l.substring(0, from) + l.substring(Math.min(to, l.length())) : l);
            if (i < self.positions().lineCount() - 1) {
                sb.append('\n');
            }
        }
        return sb.toString();
    }

    /**
     * @return the innermost clafer of this document around a position, as
     * a clafer of the known index: by its declaration if this index knows the
     * clafers, else by its name
     */
    private Optional<ClaferSymbol> enclosing(int[] lc, Index known) {
        ClaferSymbol found = null;
        if (hasClafers()) {
            for (ClaferSymbol c : clafers) {
                Occurrence d = declarations.get(c);
                if (d != null && d.file() == self && c.declaration() instanceof Syntax.Clafer
                        && at(local(self, c.declaration().span()), lc)) {
                    found = c;
                }
            }
            return Optional.ofNullable(found);
        }
        Syntax.Clafer inner = null;
        for (Syntax.Clafer c : syntaxClafers) {
            if (at(c.span(), lc)) {
                inner = c;
            }
        }
        if (inner == null) {
            return Optional.empty();
        }
        String name = inner.name().text();
        return known.clafers.stream().filter(c -> c.name().equals(name)).findFirst();
    }

    /**
     * @return the clafers that a name at a position may refer to: its
     * clafer if this index resolved it, the clafer of a local's declaration,
     * else the known clafers of that name
     */
    private List<ClaferSymbol> claferOf(Position position, String name, Index known) {
        Optional<Target> t = targetAt(position);
        if (t.isPresent() && t.get() instanceof Clafer c && known == this) {
            return List.of(c.symbol());
        }
        if (t.isPresent() && t.get() instanceof Local l && l.declaredBy() instanceof Syntax.Quantified q
                && q.decl().exp() instanceof Syntax.ClaferId id) {
            name = id.name().path().get(id.name().path().size() - 1).text();
        }
        String n = name;
        return known.clafers.stream().filter(c -> c.name().equals(n)).toList();
    }

    /**
     * @return a clafer and its supers, nearest first
     */
    private static List<ClaferSymbol> hierarchy(ClaferSymbol c) {
        List<ClaferSymbol> out = new ArrayList<>();
        for (ClaferSymbol x = c; x != null && !out.contains(x); x = x.superClafer().orElse(null)) {
            out.add(x);
        }
        return out;
    }

    /**
     * @return the children that a path can take from a clafer: its own and
     * its supers', then those of its reference's target, as the resolver's
     * {@code allChildren}
     */
    private static List<ClaferSymbol> members(ClaferSymbol c) {
        List<ClaferSymbol> out = new ArrayList<>();
        java.util.Set<ClaferSymbol> visited = java.util.Collections.newSetFromMap(new IdentityHashMap<>());
        members(c, out, visited);
        return out;
    }

    private static void members(ClaferSymbol c, List<ClaferSymbol> out, java.util.Set<ClaferSymbol> visited) {
        if (!visited.add(c)) {
            return;
        }
        for (ClaferSymbol h : hierarchy(c)) {
            out.addAll(h.children());
        }
        for (ClaferSymbol h : hierarchy(c)) {
            if (h.reference().isPresent()) {
                h.reference().get().clafer().ifPresent(t -> members(t, out, visited));
                break;
            }
        }
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
                    syntaxClafers.add(c);
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

        private void declare(Syntax.Ident ident, Syntax.Exp by, SourceSpan scopeSpan) {
            Local l = new Local(ident, by);
            localScopes.put(l, scopeSpan);
            occurrences.add(new Occurrence(self, ident.span(), l, true));
            scope.add(l);
        }

        private void exp(Syntax.Exp e) {
            switch (e) {
                case Syntax.Quantified q -> {
                    exp(q.decl().exp());
                    int mark = scope.size();
                    q.decl().locals().forEach(l -> declare(l, q, q.body().span()));
                    exp(q.body());
                    unwind(mark);
                }
                case Syntax.Let l -> {
                    exp(l.value());
                    int mark = scope.size();
                    declare(l.local(), l, l.body().span());
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
