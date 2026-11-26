package org.clafer.frontend;

import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Deque;
import java.util.HashMap;
import java.util.HashSet;
import java.util.IdentityHashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;
import org.clafer.frontend.Ir.InitKind;
import org.clafer.frontend.Syntax.Binary;
import org.clafer.frontend.Syntax.BinaryOp;
import org.clafer.frontend.Syntax.CardNum;
import org.clafer.frontend.Syntax.CardSymbol;
import org.clafer.frontend.Syntax.Clafer;
import org.clafer.frontend.Syntax.ClaferId;
import org.clafer.frontend.Syntax.ClaferUse;
import org.clafer.frontend.Syntax.Declaration;
import org.clafer.frontend.Syntax.Element;
import org.clafer.frontend.Syntax.EnumDecl;
import org.clafer.frontend.Syntax.Exp;
import org.clafer.frontend.Syntax.GroupKeyword;
import org.clafer.frontend.Syntax.Ident;
import org.clafer.frontend.Syntax.Literal;
import org.clafer.frontend.Syntax.Model;
import org.clafer.frontend.Syntax.NCard;
import org.clafer.frontend.Syntax.Node;
import org.clafer.frontend.Syntax.TempModifier;
import org.clafer.frontend.Syntax.TempModifierKind;
import org.clafer.frontend.Syntax.Unary;
import org.clafer.frontend.Syntax.UnaryOp;

/**
 * The symbols of a model, the first pass after the parser: a symbol per
 * clafer, with its UID, its parent, its super clafer, the type of its
 * reference, its card and group card with the defaults applied, its temporal
 * modifiers and the clock of its initializer. An enumeration declares a
 * symbol for itself and one for each literal. The tree is not changed: the
 * symbols are found by their declarations.
 *
 * <p>
 * The rules are those of the Clafer compiler, in the order it applies them:
 * the desugarer ({@code Desugarer.hs}: enumerations, clafer uses, a primitive
 * super, a reference to a constant), the UIDs ({@code UIDs.hs}), the supers
 * ({@code ResolverInheritance.resolveNModule}, with the relocation of a
 * top-level abstract clafer that extends a nested one, gsdlab/clafer#67),
 * the references ({@code resolveOModule}), and the defaults of the cards
 * ({@code analyzeModule}). A reference's target is resolved as the compiler
 * resolves it: by a name, a path of names, or a set expression whose type is
 * the primitive of its values or the nearest common super of its clafers.
 * The names inside constraints are the next pass.
 */
public final class Symbols {

    private Symbols() {
    }

    /**
     * An interval of a card or a group card.
     *
     * @param low the lower bound
     * @param high the upper bound, {@link #Unbounded} for {@code *}
     */
    public record Interval(int low, int high) {

        public static final int Unbounded = -1;
        public static final Interval Any = new Interval(0, Unbounded);
        public static final Interval One = new Interval(1, 1);
        public static final Interval Lone = new Interval(0, 1);

        @Override
        public String toString() {
            return low + ".." + (high == Unbounded ? "*" : Integer.toString(high));
        }
    }

    /**
     * A group card.
     *
     * @param keyword whether it was written as {@code xor}, {@code or},
     * {@code mux} or {@code opt}: a child under {@code opt} is optional,
     * under a written {@code 0..*} mandatory
     * @param interval its interval
     */
    public record GroupCard(boolean keyword, Interval interval) {

        /**
         * The group card of a clafer that has none and inherits none.
         */
        public static final GroupCard Default = new GroupCard(false, Interval.Any);
    }

    /**
     * The modifiers of a reference: {@code finalref}, {@code finaltarget},
     * and {@code final}, which is both.
     */
    public enum RefModifier {
        FinalRef("finalref"), FinalTarget("finaltarget"), FinalRefTarget("final");

        private final String keyword;

        RefModifier(String keyword) {
            this.keyword = keyword;
        }

        public String keyword() {
            return keyword;
        }
    }

    /**
     * The reference of a clafer.
     *
     * @param span the target as written, or the constant of {@code -> 1}
     * @param isSet whether it is {@code ->}, rather than {@code ->>}
     * @param target the target as written; for {@code -> 1}, the constant
     * @param clafer the clafer it refers to, if its type is a clafer
     * @param primitive the primitive it refers to, as written
     * ({@code integer} or {@code int}), if its type is a primitive
     */
    public record Reference(SourceSpan span, boolean isSet, Exp target, Optional<ClaferSymbol> clafer,
            Optional<String> primitive) {

        /**
         * @return whether the reference has one type, a clafer or a
         * primitive; a union of unrelated clafers has none
         */
        public boolean hasType() {
            return clafer.isPresent() || primitive.isPresent();
        }

        /**
         * @return the type as the explicit form writes it: a UID, or a
         * primitive with {@code integer} as {@code int}
         */
        public String typeName() {
            if (clafer.isPresent()) {
                return clafer.get().uid();
            }
            String p = primitive.orElseThrow();
            return p.equals("integer") ? "int" : p;
        }
    }

    /**
     * A clafer, or an enumeration or one of its literals.
     */
    public static final class ClaferSymbol {

        private final Node declaration;
        private final Ident name;
        private final ClaferSymbol lexicalParent;
        private final boolean isAbstract;
        private final boolean isEnum;
        private final boolean initial;
        private final boolean isFinal;
        private final List<RefModifier> refModifiers;
        private final Optional<GroupCard> writtenGroupCard;
        private final Optional<Interval> writtenCard;
        private final Optional<Exp> superExp;
        private final Optional<Syntax.Reference> writtenReference;
        private final Optional<InitKind> initializer;
        final List<ClaferSymbol> lexicalChildren = new ArrayList<>();
        // what the pass finds
        String uid;
        ClaferSymbol parent;
        final List<ClaferSymbol> children = new ArrayList<>();
        ClaferSymbol superClafer;
        boolean superResolved;
        boolean inheritsInitial;
        boolean inheritsFinal;
        Reference reference;
        // the types of the values of the reference, as the compiler's getRefIds
        List<String> referenceIds = List.of();
        Interval card;
        GroupCard groupCard;

        ClaferSymbol(Node declaration, Ident name, ClaferSymbol lexicalParent, boolean isAbstract, boolean isEnum,
                boolean initial, boolean isFinal, List<RefModifier> refModifiers,
                Optional<GroupCard> writtenGroupCard, Optional<Interval> writtenCard, Optional<Exp> superExp,
                Optional<Syntax.Reference> writtenReference, Optional<InitKind> initializer) {
            this.declaration = declaration;
            this.name = name;
            this.lexicalParent = lexicalParent;
            this.isAbstract = isAbstract;
            this.isEnum = isEnum;
            this.initial = initial;
            this.isFinal = isFinal;
            this.refModifiers = List.copyOf(refModifiers);
            this.writtenGroupCard = writtenGroupCard;
            this.writtenCard = writtenCard;
            this.superExp = superExp;
            this.writtenReference = writtenReference;
            this.initializer = initializer;
        }

        /**
         * @return the {@link Clafer}, {@link ClaferUse} or {@link EnumDecl}
         * that declares it, or the {@link Ident} of an enumeration's literal
         */
        public Node declaration() {
            return declaration;
        }

        public String name() {
            return name.text();
        }

        public SourceSpan nameSpan() {
            return name.span();
        }

        public String uid() {
            return uid;
        }

        /**
         * @return its parent, empty at the top level; a top-level abstract
         * clafer that extends a nested one is a sibling of its super
         * (gsdlab/clafer#67)
         */
        public Optional<ClaferSymbol> parent() {
            return Optional.ofNullable(parent);
        }

        /**
         * @return the clafer whose block declares it, empty at the top level
         */
        public Optional<ClaferSymbol> lexicalParent() {
            return Optional.ofNullable(lexicalParent);
        }

        /**
         * @return its subclafers, in the order of the model, with the clafers
         * relocated under it last
         */
        public List<ClaferSymbol> children() {
            return Collections.unmodifiableList(children);
        }

        public boolean isAbstract() {
            return isAbstract;
        }

        /**
         * @return whether it is an enumeration or one of its literals
         */
        public boolean isEnum() {
            return isEnum;
        }

        /**
         * @return whether its parent is the root, as the compiler's
         * {@code isTopLevel}
         */
        public boolean isTopLevel() {
            return parent == null;
        }

        /**
         * @return the super as written, if it is a clafer: not a primitive
         * super, which is a reference, nor {@code : clafer}
         */
        Optional<Exp> superExp() {
            return superExp;
        }

        public Optional<ClaferSymbol> superClafer() {
            return Optional.ofNullable(superClafer);
        }

        public Optional<Reference> reference() {
            return Optional.ofNullable(reference);
        }

        /**
         * @return its card, as written or by default
         */
        public Interval card() {
            return card;
        }

        /**
         * @return its group card: as written, or else that of its nearest
         * super with one, or else {@link GroupCard#Default}
         */
        public GroupCard groupCard() {
            return groupCard;
        }

        public Optional<Interval> writtenCard() {
            return writtenCard;
        }

        public Optional<GroupCard> writtenGroupCard() {
            return writtenGroupCard;
        }

        /**
         * @return whether it is initial, as written or as its super is
         * written
         */
        public boolean isInitial() {
            return initial || inheritsInitial;
        }

        /**
         * @return whether it is final, as written or as its super is written
         */
        public boolean isFinal() {
            return isFinal || inheritsFinal;
        }

        /**
         * @return the modifiers of its reference, of its own or of the one it
         * inherits ({@code finalref motor : engine}), as written
         */
        public List<RefModifier> refModifiers() {
            return refModifiers;
        }

        /**
         * @return the clock of its initializer, if it has one: {@code =},
         * {@code := each} or {@code := together}, written or derived from the
         * written card
         */
        public Optional<InitKind> initializer() {
            return initializer;
        }

        @Override
        public String toString() {
            return uid != null ? uid : name.text();
        }
    }

    /**
     * The symbols of a model.
     *
     * @param topLevel the top-level symbols, in the order of the model,
     * enumerations followed by their literals
     * @param all all symbols, in pre-order of the declarations
     * @param byDeclaration the symbol of each declaration, by identity
     * @param diagnostics the errors
     */
    public record Result(List<ClaferSymbol> topLevel, List<ClaferSymbol> all,
            Map<Node, ClaferSymbol> byDeclaration, List<Diagnostic> diagnostics) {

        /**
         * @param declaration a clafer, a clafer use, an enumeration or the
         * name of a literal
         * @return its symbol
         */
        public Optional<ClaferSymbol> of(Node declaration) {
            return Optional.ofNullable(byDeclaration.get(declaration));
        }
    }

    static final Set<String> Primitives = Set.of("string", "int", "integer", "decimal", "double", "real");
    private static final Set<String> Specials = Set.of("this", "parent", "dref", "root", "children", "noclafer");

    /**
     * @param model a parsed model
     * @return its symbols
     */
    public static Result analyze(Model model) {
        return new Analysis().run(model);
    }

    private static final class Analysis {

        private final List<ClaferSymbol> topLevel = new ArrayList<>();
        private final List<ClaferSymbol> all = new ArrayList<>();
        private final Map<Node, ClaferSymbol> byDeclaration = new IdentityHashMap<>();
        private final List<Diagnostic> diagnostics = new ArrayList<>();
        private final Set<ClaferSymbol> resolving = Collections.newSetFromMap(new IdentityHashMap<>());

        Result run(Model model) {
            for (Declaration d : model.declarations()) {
                declare(d);
            }
            assignUids();
            for (ClaferSymbol c : all) {
                resolveSuper(c);
            }
            checkCycles();
            relocate();
            for (ClaferSymbol c : all) {
                resolveReference(c);
            }
            for (ClaferSymbol c : roots()) {
                analyzeCards(c, null);
            }
            checkDuplicateSiblings(roots());
            for (ClaferSymbol c : all) {
                checkRedefinition(c);
                checkGroup(c);
            }
            diagnostics.sort((a, b) -> a.span().line() != b.span().line()
                    ? Integer.compare(a.span().line(), b.span().line())
                    : Integer.compare(a.span().column(), b.span().column()));
            return new Result(List.copyOf(roots()), List.copyOf(all), Collections.unmodifiableMap(byDeclaration),
                    List.copyOf(diagnostics));
        }

        private List<ClaferSymbol> roots() {
            return topLevel;
        }

        private void error(SourceSpan span, String message) {
            diagnostics.add(new Diagnostic(span, message));
        }

        // --------------------------------------------------------------
        // Declarations, as the desugarer makes them
        // --------------------------------------------------------------
        private void declare(Declaration d) {
            switch (d) {
                case EnumDecl e -> {
                    // an abstract clafer, and a clafer extending it for each literal
                    ClaferSymbol en = add(new ClaferSymbol(e, e.name(), null, true, true, false, false, List.of(),
                            Optional.empty(), Optional.empty(), Optional.empty(), Optional.empty(), Optional.empty()),
                            null, e);
                    for (Ident literal : e.literals()) {
                        ClaferSymbol l = add(new ClaferSymbol(literal, literal, null, false, true, false, false,
                                List.of(), Optional.empty(), Optional.of(Interval.One), Optional.empty(),
                                Optional.empty(), Optional.empty()), null, literal);
                        l.superClafer = en;
                        l.superResolved = true;
                    }
                }
                case Element e -> declare(e, null);
            }
        }

        private void declare(Element e, ClaferSymbol parent) {
            switch (e) {
                case Clafer c -> declareClafer(c, parent);
                case ClaferUse u -> {
                    Ident name = u.name().path().get(u.name().path().size() - 1);
                    ClaferSymbol s = add(new ClaferSymbol(u, name, parent, false, false, false, false, List.of(),
                            Optional.empty(), u.card().flatMap(this::card), Optional.of(new ClaferId(u.name().span(), u.name())),
                            Optional.empty(), Optional.empty()), parent, u);
                    u.elements().ifPresent(es -> es.elements().forEach(x -> declare(x, s)));
                }
                default -> {
                }
            }
        }

        private void declareClafer(Clafer c, ClaferSymbol parent) {
            Optional<Interval> writtenCard = c.card().flatMap(this::card);
            Optional<Syntax.Reference> reference = c.reference();
            Optional<Exp> superExp = c.superClafer().map(Syntax.Super::exp);
            Optional<InitKind> init = c.init().map(i -> switch (i.how()) {
                case Constant -> InitKind.FIXED;
                case Each -> InitKind.EACH;
                case Together -> InitKind.TOGETHER;
                // more than one instance under a parent: together; by the written card
                case Default -> writtenCard.map(w -> w.high() != 0 && w.high() != 1).orElse(false)
                        ? InitKind.TOGETHER : InitKind.EACH;
            });
            // a reference to a constant, `-> 1`, is a reference to its type with the value `= 1`
            if (c.init().isEmpty() && reference.isPresent() && constantType(reference.get().exp()).isPresent()) {
                init = Optional.of(InitKind.FIXED);
            }
            // a primitive super, `: integer`, is a reference
            if (superExp.isPresent() && superName(superExp.get()).filter(Primitives::contains).isPresent()) {
                if (reference.isPresent()) {
                    error(superExp.get().span(), "A clafer with a reference cannot have a primitive type as its super: "
                            + superName(superExp.get()).get());
                } else {
                    reference = Optional.of(new Syntax.Reference(c.superClafer().get().span(), false, superExp.get()));
                }
                superExp = Optional.empty();
            }
            // `: clafer` is no super
            if (superExp.isPresent() && superExp.get() instanceof ClaferId id && id.name().path().size() == 1
                    && id.name().path().get(0).text().equals("clafer")) {
                superExp = Optional.empty();
            }
            boolean initial = false;
            boolean isFinal = false;
            List<RefModifier> refModifiers = new ArrayList<>();
            for (TempModifier m : c.modifiers()) {
                switch (m.kind()) {
                    case Initial -> initial = true;
                    case Final -> {
                        isFinal = true;
                        if (c.reference().isPresent()) {
                            addOnce(refModifiers, RefModifier.FinalRefTarget);
                        }
                    }
                    case FinalRef -> addOnce(refModifiers, RefModifier.FinalRef);
                    case FinalTarget -> addOnce(refModifiers, RefModifier.FinalTarget);
                }
            }
            ClaferSymbol s = add(new ClaferSymbol(c, c.name(), parent, c.isAbstract(), false, initial, isFinal,
                    refModifiers, c.groupCard().flatMap(this::groupCard), writtenCard, superExp, reference, init),
                    parent, c);
            c.elements().ifPresent(es -> es.elements().forEach(x -> declare(x, s)));
        }

        private static <T> void addOnce(List<T> list, T t) {
            if (!list.contains(t)) {
                list.add(t);
            }
        }

        private ClaferSymbol add(ClaferSymbol s, ClaferSymbol parent, Node declaration) {
            all.add(s);
            byDeclaration.put(declaration, s);
            if (parent == null) {
                topLevel.add(s);
            } else {
                parent.lexicalChildren.add(s);
                parent.children.add(s);
            }
            s.parent = parent;
            return s;
        }

        private Optional<Interval> card(Syntax.Card card) {
            return switch (card) {
                case CardSymbol s -> Optional.of(switch (s.symbol()) {
                    case "?" -> Interval.Lone;
                    case "+" -> new Interval(1, Interval.Unbounded);
                    default -> Interval.Any;
                });
                case CardNum n -> number(n.span(), n.number()).map(k -> new Interval(k, k));
                case NCard n -> interval(n);
            };
        }

        private Optional<GroupCard> groupCard(Syntax.GCard card) {
            return switch (card) {
                case GroupKeyword k -> Optional.of(new GroupCard(true, switch (k.keyword()) {
                    case "xor" -> Interval.One;
                    case "or" -> new Interval(1, Interval.Unbounded);
                    case "mux" -> Interval.Lone;
                    default -> Interval.Any;
                }));
                case NCard n -> interval(n).map(i -> new GroupCard(false, i));
            };
        }

        private Optional<Interval> interval(NCard n) {
            Optional<Integer> low = number(n.span(), n.low());
            Optional<Integer> high = n.high().isEmpty() ? Optional.of(Interval.Unbounded) : number(n.span(), n.high().get());
            return low.isPresent() && high.isPresent() ? Optional.of(new Interval(low.get(), high.get())) : Optional.empty();
        }

        private Optional<Integer> number(SourceSpan span, String text) {
            try {
                return Optional.of(Integer.parseInt(text));
            } catch (NumberFormatException e) {
                error(span, "The bound " + text + " is too large");
                return Optional.empty();
            }
        }

        /**
         * The type of a constant as a reference's target, as the desugarer's
         * {@code constantType}: a real literal, {@code 3.14}, is a decimal.
         */
        private static Optional<String> constantType(Exp exp) {
            Exp e = exp instanceof Unary u && u.op() == UnaryOp.Minus ? u.exp() : exp;
            if (e instanceof Literal l) {
                return Optional.of(switch (l.kind()) {
                    case Int -> "integer";
                    case Double -> "double";
                    case Real -> "decimal";
                    case Str -> "string";
                });
            }
            return Optional.empty();
        }

        /**
         * The last name of a name or a path, as the desugarer's
         * {@code getPExpClaferIdent}.
         */
        private static Optional<String> superName(Exp exp) {
            return switch (exp) {
                case ClaferId id -> Optional.of(last(id));
                case Binary b when b.op() == BinaryOp.Join -> superName(b.right());
                default -> Optional.empty();
            };
        }

        private static String last(ClaferId id) {
            return id.name().path().get(id.name().path().size() - 1).text();
        }

        // --------------------------------------------------------------
        // UIDs, as UIDs.assignUIDs
        // --------------------------------------------------------------
        private void assignUids() {
            List<List<String>> reversed = new ArrayList<>();
            for (ClaferSymbol s : all) {
                List<String> path = new ArrayList<>();
                for (ClaferSymbol c = s; c != null; c = c.lexicalParent) {
                    path.add(c.name());
                }
                reversed.add(path);
            }
            Map<List<String>, Integer> suffixCounts = new HashMap<>();
            for (List<String> rp : reversed) {
                Set<List<String>> suffixes = new HashSet<>();
                for (int k = 1; k <= rp.size(); k++) {
                    suffixes.add(rp.subList(0, k));
                }
                for (List<String> s : suffixes) {
                    suffixCounts.merge(s, 1, Integer::sum);
                }
            }
            Set<String> used = new HashSet<>();
            for (int i = 0; i < all.size(); i++) {
                List<String> rp = reversed.get(i);
                List<String> chosen = null;
                for (int k = 1; k <= rp.size() && chosen == null; k++) {
                    if (suffixCounts.get(rp.subList(0, k)) == 1) {
                        chosen = rp.subList(0, k);
                    }
                }
                if (chosen == null) {
                    chosen = new ArrayList<>(rp);
                    chosen.add("root");
                }
                List<String> names = new ArrayList<>(chosen);
                Collections.reverse(names);
                String uid = names.stream().map(n -> n.replace("'", "_prime")).collect(Collectors.joining("__"));
                String fresh = uid;
                for (int n = 2; used.contains(fresh); n++) {
                    fresh = uid + "__" + n;
                }
                used.add(fresh);
                all.get(i).uid = fresh;
            }
        }

        // --------------------------------------------------------------
        // Supers, as ResolverInheritance.resolveNSuper
        // --------------------------------------------------------------
        private List<ClaferSymbol> abstractClafers() {
            return all.stream().filter(c -> c.isAbstract).toList();
        }

        /**
         * The supers of the clafer, nearest first, without a cycle.
         */
        private List<ClaferSymbol> superChain(ClaferSymbol c) {
            List<ClaferSymbol> chain = new ArrayList<>();
            Set<ClaferSymbol> visited = Collections.newSetFromMap(new IdentityHashMap<>());
            visited.add(c);
            for (ClaferSymbol s = resolveSuper(c); s != null && visited.add(s); s = resolveSuper(s)) {
                chain.add(s);
            }
            return chain;
        }

        private ClaferSymbol resolveSuper(ClaferSymbol c) {
            if (c.superResolved || c.superExp.isEmpty()) {
                return c.superClafer;
            }
            if (!resolving.add(c)) {
                // a cycle, which checkCycles reports
                return null;
            }
            try {
                Exp exp = c.superExp.get();
                c.superClafer = switch (exp) {
                    case ClaferId id -> superByName(c, id);
                    case Binary b when b.op() == BinaryOp.Join -> superByPath(c, b);
                    default -> {
                        error(exp.span(), "A super must be the name of an abstract clafer or a path to it, e.g., Person.Head");
                        yield null;
                    }
                };
                c.superResolved = true;
                if (c.superClafer != null) {
                    c.inheritsInitial = c.superClafer.initial;
                    c.inheritsFinal = c.superClafer.isFinal;
                }
                return c.superClafer;
            } finally {
                resolving.remove(c);
            }
        }

        private ClaferSymbol superByName(ClaferSymbol c, ClaferId id) {
            String name = last(id);
            if (Primitives.contains(name)) {
                error(id.span(), "Primitive types are not allowed as super types: " + name);
                return null;
            }
            List<ClaferSymbol> abstracts = abstractClafers().stream().filter(a -> a != c).toList();
            // a clafer cannot extend itself, unless it redefines another clafer with its name
            if (c.isAbstract && name.equals(c.name()) && abstracts.stream().noneMatch(a -> a.name().equals(name))) {
                error(c.declaration.span(), "clafer " + c.uid + " inherits from itself");
                return null;
            }
            // the nearest abstract clafer with the name among the children of the parent and
            // those the parent inherits
            if (c.lexicalParent != null) {
                List<ClaferSymbol> scopes = new ArrayList<>();
                scopes.add(c.lexicalParent);
                scopes.addAll(superChain(c.lexicalParent));
                for (ClaferSymbol s : scopes) {
                    for (ClaferSymbol child : s.lexicalChildren) {
                        if (child.isAbstract && child != c && child.name().equals(name)) {
                            return child;
                        }
                    }
                }
            }
            List<ClaferSymbol> candidates = abstracts.stream().filter(a -> a.name().equals(name)).toList();
            if (candidates.isEmpty()) {
                error(id.span(), "No superclafer found: " + name);
                return null;
            }
            if (candidates.size() > 1) {
                error(id.span(), "clafer \"" + name + "\" is not unique. Available paths: "
                        + candidates.stream().map(Analysis::path).collect(Collectors.joining(", ")));
                return null;
            }
            return candidates.get(0);
        }

        private ClaferSymbol superByPath(ClaferSymbol c, Binary path) {
            List<ClaferId> names = new ArrayList<>();
            if (!pathNames(path, names)) {
                error(path.span(), "A super must be the name of an abstract clafer or a path to it, e.g., Person.Head");
                return null;
            }
            String text = names.stream().map(Analysis::last).collect(Collectors.joining("."));
            String first = last(names.get(0));
            ClaferSymbol s = null;
            for (ClaferSymbol a = c.lexicalParent; a != null && s == null; a = a.lexicalParent) {
                if (a.name().equals(first)) {
                    s = a;
                }
            }
            if (s == null) {
                List<ClaferSymbol> named = all.stream().filter(x -> x.name().equals(first)).toList();
                List<ClaferSymbol> top = named.stream().filter(x -> x.lexicalParent == null).toList();
                if (named.isEmpty()) {
                    error(path.span(), "No superclafer found: no clafer " + first + " for " + text);
                    return null;
                } else if (named.size() == 1) {
                    s = named.get(0);
                } else if (top.size() == 1) {
                    s = top.get(0);
                } else {
                    error(path.span(), "No superclafer found: the clafer " + first + " in " + text + " is not unique");
                    return null;
                }
            }
            for (ClaferId id : names.subList(1, names.size())) {
                String n = last(id);
                ClaferSymbol child = null;
                List<ClaferSymbol> scopes = new ArrayList<>();
                scopes.add(s);
                scopes.addAll(superChain(s));
                for (ClaferSymbol scope : scopes) {
                    for (ClaferSymbol x : scope.lexicalChildren) {
                        if (child == null && x.name().equals(n)) {
                            child = x;
                        }
                    }
                }
                if (child == null) {
                    error(id.span(), "No superclafer found: " + s.name() + " has no child " + n
                            + ", neither its own nor an inherited one, for " + text);
                    return null;
                }
                s = child;
            }
            if (s == c) {
                error(path.span(), "clafer " + c.uid + " inherits from itself");
                return null;
            }
            if (!s.isAbstract) {
                error(path.span(), "No superclafer found: " + text + " is not abstract, and only abstract clafers can be supers");
                return null;
            }
            return s;
        }

        private static boolean pathNames(Exp exp, List<ClaferId> names) {
            return switch (exp) {
                case ClaferId id -> names.add(id);
                case Binary b when b.op() == BinaryOp.Join -> pathNames(b.left(), names) && pathNames(b.right(), names);
                default -> false;
            };
        }

        private static String path(ClaferSymbol c) {
            List<String> names = new ArrayList<>();
            for (ClaferSymbol a = c; a != null; a = a.lexicalParent) {
                names.add(0, a.name());
            }
            return String.join(".", names);
        }

        private void checkCycles() {
            for (ClaferSymbol c : all) {
                Set<ClaferSymbol> visited = Collections.newSetFromMap(new IdentityHashMap<>());
                for (ClaferSymbol s = c.superClafer; s != null && visited.add(s); s = s.superClafer) {
                    if (s == c) {
                        error(c.declaration.span(), "clafer " + c.uid + " inherits from itself");
                        break;
                    }
                }
            }
        }

        /**
         * A top-level abstract clafer that extends a nested abstract clafer
         * becomes a sibling of its super (gsdlab/clafer#67): it goes after
         * the children of its super's parent.
         */
        private void relocate() {
            List<ClaferSymbol> moved = new ArrayList<>();
            for (ClaferSymbol c : topLevel) {
                if (c.isAbstract && c.superClafer != null && c.superClafer.lexicalParent != null) {
                    moved.add(c);
                }
            }
            for (ClaferSymbol c : moved) {
                topLevel.remove(c);
                c.parent = c.superClafer.lexicalParent;
                c.parent.children.add(c);
            }
        }

        // --------------------------------------------------------------
        // References, as ResolverInheritance.resolveOReference
        // --------------------------------------------------------------
        private void resolveReference(ClaferSymbol c) {
            if (c.writtenReference.isEmpty()) {
                return;
            }
            Syntax.Reference r = c.writtenReference.get();
            Exp target = r.exp();
            boolean isSet = !r.bag();
            Optional<String> constant = constantType(target);
            if (constant.isPresent()) {
                c.reference = new Reference(target.span(), isSet, target, Optional.empty(), constant);
                c.referenceIds = List.of(constant.get());
                return;
            }
            List<Type> leaves = new ArrayList<>();
            boolean setExp = target instanceof Binary b && isSetOp(b.op());
            if (!typesOf(c, target, leaves)) {
                c.reference = new Reference(target.span(), isSet, target, Optional.empty(), Optional.empty());
                return;
            }
            c.referenceIds = leaves.stream().map(t -> t.clafer().map(ClaferSymbol::uid).orElseGet(() -> t.primitive().get()))
                    .toList();
            Optional<Type> type = setExp ? setExpType(leaves) : leaves.size() == 1 ? Optional.of(leaves.get(0)) : Optional.empty();
            c.reference = new Reference(target.span(), isSet, target,
                    type.flatMap(Type::clafer), type.flatMap(Type::primitive));
        }

        private record Type(Optional<ClaferSymbol> clafer, Optional<String> primitive) {

            static Type of(ClaferSymbol c) {
                return new Type(Optional.of(c), Optional.empty());
            }

            static Type of(String primitive) {
                return new Type(Optional.empty(), Optional.of(primitive));
            }
        }

        private static boolean isSetOp(BinaryOp op) {
            return op == BinaryOp.Union || op == BinaryOp.UnionCom || op == BinaryOp.Difference
                    || op == BinaryOp.Intersection || op == BinaryOp.IntersectionDeprecated;
        }

        /**
         * The types of the values of a target, as the compiler's
         * {@code getRefIds}: of each operand of a set operator, and of the
         * last name of a path.
         */
        private boolean typesOf(ClaferSymbol c, Exp exp, List<Type> types) {
            Optional<String> constant = constantType(exp);
            if (constant.isPresent()) {
                types.add(Type.of(constant.get()));
                return true;
            }
            switch (exp) {
                case Binary b when isSetOp(b.op()) -> {
                    return typesOf(c, b.left(), types) && typesOf(c, b.right(), types);
                }
                case Binary b when b.op() == BinaryOp.Join -> {
                    List<ClaferId> names = new ArrayList<>();
                    if (!pathNames(b, names)) {
                        error(exp.span(), "A reference's target must be a name, a path or a set expression");
                        return false;
                    }
                    Optional<Type> t = resolveFirst(c, names.get(0));
                    for (ClaferId id : names.subList(1, names.size())) {
                        if (t.isEmpty() || t.get().clafer().isEmpty()) {
                            if (t.isPresent()) {
                                error(id.span(), "Name resolver: '" + last(id) + "' not found");
                            }
                            return false;
                        }
                        t = resolveChild(t.get().clafer().get(), id);
                    }
                    t.ifPresent(types::add);
                    return t.isPresent();
                }
                case ClaferId id -> {
                    Optional<Type> t = resolveFirst(c, id);
                    t.ifPresent(types::add);
                    return t.isPresent();
                }
                default -> {
                    error(exp.span(), "A reference's target must be a name, a path or a set expression");
                    return false;
                }
            }
        }

        /**
         * The first name of a target: a primitive, or the unique clafer with
         * the name in the trees of the top-level abstract clafers, or else of
         * the concrete ones, inherited children included, as the compiler's
         * {@code resolveTopLevel}.
         */
        private Optional<Type> resolveFirst(ClaferSymbol c, ClaferId id) {
            String name = last(id);
            if (Primitives.contains(name)) {
                return Optional.of(Type.of(name));
            }
            if (Specials.contains(name)) {
                error(id.span(), "A reference's target cannot be '" + name + "'");
                return Optional.empty();
            }
            for (boolean abstractTrees : new boolean[]{true, false}) {
                List<List<ClaferSymbol>> found = new ArrayList<>();
                for (List<ClaferSymbol> p : bfs(abstractTrees)) {
                    if (p.get(0).name().equals(name)) {
                        found.add(p);
                    }
                }
                if (found.size() == 1) {
                    return Optional.of(Type.of(found.get(0).get(0)));
                }
                if (found.size() > 1) {
                    error(id.span(), "clafer \"" + name + "\" is not unique. Available paths: "
                            + found.stream().map(Analysis::showPath).distinct().collect(Collectors.joining(", ")));
                    return Optional.empty();
                }
            }
            error(id.span(), "Name resolver: '" + name + "' not found");
            return Optional.empty();
        }

        private static String showPath(List<ClaferSymbol> path) {
            List<String> uids = path.stream().map(ClaferSymbol::uid).collect(Collectors.toList());
            Collections.reverse(uids);
            return String.join(".", uids);
        }

        /**
         * The paths, nearest clafer first, of the clafers in the trees of the
         * top-level abstract or concrete clafers, breadth first, through the
         * children and the inherited children; a path stops at a clafer that
         * it reaches a second time.
         */
        private List<List<ClaferSymbol>> bfs(boolean abstractTrees) {
            List<List<ClaferSymbol>> out = new ArrayList<>();
            Deque<List<ClaferSymbol>> queue = new ArrayDeque<>();
            for (ClaferSymbol t : topLevel) {
                if (t.isAbstract == abstractTrees) {
                    queue.add(List.of(t));
                }
            }
            Set<List<ClaferSymbol>> seen = new HashSet<>();
            while (!queue.isEmpty()) {
                List<ClaferSymbol> p = queue.poll();
                if (seen.add(p)) {
                    out.add(p);
                }
                ClaferSymbol c = p.get(0);
                if (Collections.frequency(p, c) > 1) {
                    continue;
                }
                for (ClaferSymbol child : inheritedChildren(c)) {
                    List<ClaferSymbol> q = new ArrayList<>();
                    q.add(child);
                    q.addAll(p);
                    queue.add(q);
                }
            }
            return out;
        }

        /**
         * The children of the clafer and of its supers.
         */
        private List<ClaferSymbol> inheritedChildren(ClaferSymbol c) {
            List<ClaferSymbol> out = new ArrayList<>(c.children);
            for (ClaferSymbol s : superChain(c)) {
                out.addAll(s.children);
            }
            return out;
        }

        /**
         * The next name of a path: a child of the clafer, its own or an
         * inherited one, or else a child of its reference's target.
         */
        private Optional<Type> resolveChild(ClaferSymbol c, ClaferId id) {
            String name = last(id);
            Set<ClaferSymbol> own = new LinkedHashSet<>();
            for (ClaferSymbol x : inheritedChildren(c)) {
                if (x.name().equals(name)) {
                    own.add(x);
                }
            }
            if (own.isEmpty()) {
                Set<ClaferSymbol> visited = Collections.newSetFromMap(new IdentityHashMap<>());
                for (ClaferSymbol t = c; t != null && visited.add(t); ) {
                    if (t.writtenReference.isPresent() && t.reference == null && t != c) {
                        resolveReference(t);
                    }
                    ClaferSymbol target = null;
                    for (ClaferSymbol s : withSupers(t)) {
                        if (s.reference != null && s.reference.clafer().isPresent()) {
                            target = s.reference.clafer().get();
                            break;
                        }
                    }
                    t = target;
                    if (t != null) {
                        for (ClaferSymbol x : inheritedChildren(t)) {
                            if (x.name().equals(name)) {
                                own.add(x);
                            }
                        }
                    }
                }
            }
            if (own.size() == 1) {
                return Optional.of(Type.of(own.iterator().next()));
            }
            error(id.span(), own.isEmpty()
                    ? "Name resolver: '" + name + "' not found within " + c.uid
                    : "clafer \"" + name + "\" is not unique in " + c.uid);
            return Optional.empty();
        }

        private List<ClaferSymbol> withSupers(ClaferSymbol c) {
            List<ClaferSymbol> out = new ArrayList<>();
            out.add(c);
            out.addAll(superChain(c));
            return out;
        }

        /**
         * The type of a set expression, as the compiler's
         * {@code setExpType}: the primitive of its values, with integers
         * among decimals as decimals, or the nearest common super of its
         * clafers.
         */
        private Optional<Type> setExpType(List<Type> leaves) {
            if (leaves.stream().allMatch(t -> t.primitive().isPresent())) {
                Set<String> types = new java.util.TreeSet<>();
                for (Type t : leaves) {
                    types.add(t.primitive().get().equals("int") ? "integer" : t.primitive().get());
                }
                if (types.size() == 1) {
                    return Optional.of(Type.of(types.iterator().next()));
                }
                if (types.equals(Set.of("decimal", "integer"))) {
                    return Optional.of(Type.of("decimal"));
                }
                return Optional.empty();
            }
            if (leaves.stream().allMatch(t -> t.clafer().isPresent())) {
                List<List<ClaferSymbol>> supers = leaves.stream().map(t -> withSupers(t.clafer().get())).toList();
                for (ClaferSymbol u : supers.get(0)) {
                    if (supers.stream().allMatch(s -> s.contains(u))) {
                        return Optional.of(Type.of(u));
                    }
                }
            }
            return Optional.empty();
        }

        // --------------------------------------------------------------
        // Cards, as ResolverInheritance.analyzeModule
        // --------------------------------------------------------------
        private void analyzeCards(ClaferSymbol c, ClaferSymbol parent) {
            c.groupCard = c.writtenGroupCard.orElse(null);
            if (c.groupCard == null && c.superClafer != null) {
                for (ClaferSymbol s : superChain(c)) {
                    if (s.writtenGroupCard.isPresent()) {
                        c.groupCard = s.writtenGroupCard.get();
                        break;
                    }
                }
            }
            if (c.groupCard == null) {
                c.groupCard = GroupCard.Default;
            }
            if (c.writtenCard.isPresent()) {
                c.card = c.writtenCard.get();
            } else if (c.isAbstract) {
                c.card = Interval.Any;
            } else if (parent == null || parent.groupCard.equals(GroupCard.Default)) {
                // mandatory at the top level and under the default group; optional under a
                // keyword group (opt included) or a numeric one
                c.card = Interval.One;
            } else {
                c.card = Interval.Lone;
            }
            for (ClaferSymbol child : c.children) {
                analyzeCards(child, c);
            }
        }

        /**
         * The UID of the parent as the compiler's {@code _parentUID}: the
         * root of a top-level clafer is {@code root}, and that of a
         * top-level abstract one {@code clafer}.
         */
        private static String parentUid(ClaferSymbol c) {
            return c.parent != null ? c.parent.uid : c.isAbstract ? "clafer" : "root";
        }

        /**
         * A primitive and the primitives it converts to, as the compiler's
         * hierarchy of primitives: an integer is a decimal, a decimal and a
         * double are reals.
         */
        private static List<String> primitiveSupers(String p) {
            return switch (p) {
                case "integer", "int" -> List.of(p, "decimal", "real");
                case "decimal", "double" -> List.of(p, "real");
                default -> List.of(p);
            };
        }

        private List<String> supersOfId(String id) {
            if (Primitives.contains(id)) {
                return primitiveSupers(id);
            }
            return all.stream().filter(s -> s.uid.equals(id)).findFirst()
                    .map(s -> withSupers(s).stream().map(x -> x.uid).toList()).orElse(List.of(id));
        }

        /**
         * The nesting and the redefinition of a clafer with a super, as the
         * compiler's {@code resolveRedefinition}: a super that is nested
         * belongs to the parent, or to a super of the parent's super; a
         * concrete super's card is narrowed; a set reference stays a set; the
         * target is a subtype of the super's target.
         */
        private void checkRedefinition(ClaferSymbol c) {
            ClaferSymbol s = c.superClafer;
            if (s == null || c.isEnum) {
                return;
            }
            String superParent = parentUid(s);
            boolean properNesting = (s.parent == null && s.isAbstract)
                    || parentUid(c).equals(superParent)
                    || (c.parent != null && c.parent.superClafer != null
                    && withSupers(c.parent.superClafer).stream().anyMatch(x -> x.uid.equals(superParent)));
            String problem = null;
            if (!properNesting) {
                problem = "Improperly nested clafer";
            } else if (!s.isAbstract && !(c.card.low() >= s.card.low()
                    && (c.card.high() <= s.card.high() || s.card.high() == Interval.Unbounded))) {
                problem = "Improper cardinality refinement for clafer";
            } else if (c.reference != null && s.reference != null && s.reference.isSet() && !c.reference.isSet()) {
                problem = "Improper bag to set refinement for clafer";
            } else if (c.referenceIds.size() == 1 && s.referenceIds.size() == 1) {
                String target = c.referenceIds.get(0);
                String superTarget = s.referenceIds.get(0);
                if (!supersOfId(target).contains(superTarget)
                        && !((target.equals("integer") || target.equals("int")) && superTarget.equals("double"))) {
                    problem = "Improper target subtyping for clafer";
                }
            }
            if (problem != null) {
                error(c.declaration.span(), problem + " '" + c.name() + "'");
            }
        }

        /**
         * A concrete clafer's group card can be satisfied: its children, the
         * inherited ones included, can have as many instances as its lower
         * bound, as the compiler's {@code unsatisfiableGroups}, e.g.,
         * {@code xor} over no children.
         */
        private void checkGroup(ClaferSymbol c) {
            int low = c.groupCard.interval().low();
            if (c.isAbstract || low <= 0) {
                return;
            }
            long most = 0;
            for (ClaferSymbol child : inheritedChildren(c)) {
                if (child.card.high() == Interval.Unbounded) {
                    return;
                }
                most += child.card.high();
            }
            if (most < low) {
                error(c.declaration.span(), "The group cardinality " + c.groupCard.interval() + " of " + c.name()
                        + " cannot be satisfied: its children, including the inherited ones, can have at most "
                        + most + " instances");
            }
        }

        private void checkDuplicateSiblings(List<ClaferSymbol> siblings) {
            // at the first clafer with the name, as the compiler reports it
            Map<String, ClaferSymbol> seen = new HashMap<>();
            Set<ClaferSymbol> reported = Collections.newSetFromMap(new IdentityHashMap<>());
            for (ClaferSymbol s : siblings) {
                ClaferSymbol first = seen.putIfAbsent(s.name(), s);
                if (first != null && reported.add(first)) {
                    error(first.declaration.span(), "Duplicate name: " + s.name());
                }
                checkDuplicateSiblings(s.children);
            }
        }
    }

    // ------------------------------------------------------------------
    // The outline
    // ------------------------------------------------------------------
    /**
     * The clafers as the explicit form declares them, without their
     * constraints and the values of their initializers: one line per clafer,
     * {@code [abstract] [modifiers] gcard uid [: super] [-> type] card [init]},
     * its subclafers indented under it, and an enumeration as
     * {@code enum E = a | b}. It is what the symbols say about a model, and
     * {@link #outline(Ir.Model)} gives the same for an explicit model.
     *
     * @param symbols the symbols of a model
     * @return its outline
     */
    public static String outline(Result symbols) {
        StringBuilder out = new StringBuilder();
        for (ClaferSymbol c : symbols.topLevel()) {
            if (c.isEnum()) {
                if (c.isAbstract()) {
                    out.append("enum ").append(c.uid()).append(" = ").append(symbols.topLevel().stream()
                            .filter(l -> l.isEnum() && l.superClafer().orElse(null) == c)
                            .map(ClaferSymbol::uid).collect(Collectors.joining(" | "))).append('\n');
                }
            } else {
                outline(c, "", out);
            }
        }
        return out.toString();
    }

    private static void outline(ClaferSymbol c, String indent, StringBuilder out) {
        out.append(indent);
        if (c.isAbstract()) {
            out.append("abstract ");
        }
        if (c.isInitial()) {
            out.append("initial ");
        }
        if (c.isFinal()) {
            out.append("final ");
        } else {
            for (RefModifier m : c.refModifiers()) {
                out.append(m.keyword()).append(' ');
            }
        }
        out.append(printedGroupCard(c)).append(' ').append(c.uid());
        c.superClafer().ifPresent(s -> out.append(" : ").append(s.uid()));
        c.reference().filter(Reference::hasType)
                .ifPresent(r -> out.append(r.isSet() ? " -> " : " ->> ").append(r.typeName()));
        out.append(' ').append(c.card());
        c.initializer().ifPresent(i -> out.append(' ').append(i.syntax()));
        out.append('\n');
        for (ClaferSymbol child : c.children()) {
            outline(child, indent + "  ", out);
        }
    }

    /**
     * The group card as the explicit form prints it: the clafer's, unless it
     * is {@code 0..*}, and then its super's.
     */
    static Interval printedGroupCard(ClaferSymbol c) {
        Set<ClaferSymbol> visited = Collections.newSetFromMap(new IdentityHashMap<>());
        for (ClaferSymbol s = c; s != null && visited.add(s); s = s.superClafer().orElse(null)) {
            if (!s.groupCard().interval().equals(Interval.Any)) {
                return s.groupCard().interval();
            }
        }
        return Interval.Any;
    }

    /**
     * @param model an explicit model
     * @return its outline, as {@link #outline(Result)}
     */
    public static String outline(Ir.Model model) {
        StringBuilder out = new StringBuilder();
        for (Ir.Element e : model.elements()) {
            switch (e) {
                case Ir.EnumDecl en -> out.append("enum ").append(en.name().text()).append(" = ")
                        .append(en.literals().stream().map(Ir.Name::text).collect(Collectors.joining(" | ")))
                        .append('\n');
                case Ir.ClaferDecl c -> outline(c, "", out);
                default -> {
                }
            }
        }
        return out.toString();
    }

    private static void outline(Ir.ClaferDecl c, String indent, StringBuilder out) {
        out.append(indent);
        if (c.isAbstract()) {
            out.append("abstract ");
        }
        for (Ir.Modifier m : c.modifiers()) {
            out.append(m.kind().keyword()).append(' ');
        }
        out.append(c.groupCard()).append(' ').append(c.name().text());
        c.superName().ifPresent(s -> out.append(" : ").append(s.text()));
        c.ref().ifPresent(r -> out.append(r.isSet() ? " -> " : " ->> ").append(r.target().text()));
        out.append(' ').append(c.abstractMultiplicity().map(Ir.AbstractMultiplicity::card).orElse(c.card()));
        c.initializer().ifPresent(i -> out.append(' ').append(i.kind().syntax()));
        out.append('\n');
        for (Ir.Element e : c.elements()) {
            if (e instanceof Ir.ClaferDecl child) {
                outline(child, indent + "  ", out);
            }
        }
    }
}
