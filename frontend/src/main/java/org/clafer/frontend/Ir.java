package org.clafer.frontend;

import java.util.List;
import java.util.Optional;
import java.util.OptionalInt;

/**
 * The explicit IR: one tree for the rich and the core explicit language of
 * Clafer, as the Clafer compiler prints them ({@code clafer -m clafer}, with
 * {@code --core} for the core). The core is a strict subset of the rich
 * language, so a core model is a rich model without rich nodes. Whether a node
 * is rich is a property of its type ({@link Node#isRich()}), never of a field
 * value: every rich part of a declaration is a node of its own, e.g., a
 * {@link Modifier} or an {@link Initializer} of a {@link ClaferDecl}, which is
 * core. {@link Core#richNodes} lists the rich nodes of a tree.
 * <p>
 * The tree is the text: names are not resolved, and every declaration and
 * constraint stays where it was written, so that {@link ExplicitPrinter}
 * prints it back as it was read. Resolving the names is the job of what reads
 * the tree, e.g., chocosolver's {@code ToAst}.
 * <p>
 * The records are immutable; their lists are unmodifiable copies.
 */
public final class Ir {

    private Ir() {
    }

    /**
     * A node of the tree.
     */
    public sealed interface Node permits Model, Element, Exp, Name, Card, RefDecl, Modifier, Initializer,
            AbstractMultiplicity, Objective, Assertion, Decl {

        /**
         * @return where the node is in the source
         */
        SourceSpan span();

        /**
         * @return whether nodes of this type are outside the core, a constant
         * of the type
         */
        default boolean isRich() {
            return false;
        }
    }

    // ------------------------------------------------------------------
    // The model
    // ------------------------------------------------------------------
    /**
     * The options of the first line, {@code //# OPTIONS --explicit}, as the
     * compiler writes them.
     *
     * @param core whether the model is an explicit core model,
     * {@code --core}
     * @param traceLength the trace length of a temporal model,
     * {@code --tl=N}
     */
    public record Options(boolean core, OptionalInt traceLength) {

        /**
         * The options of a static rich explicit model.
         */
        public static final Options Rich = new Options(false, OptionalInt.empty());
    }

    /**
     * A model: its declarations and constraints in the order they were
     * written, then its objectives and assertions, which the compiler prints
     * at the end.
     *
     * @param span the whole source
     * @param options the options of the first line
     * @param elements the top-level clafers, enumerations and constraints
     * @param objectives the objectives
     * @param assertions the assertions, quantified at the top level
     */
    public record Model(SourceSpan span, Options options, List<Element> elements, List<Objective> objectives,
            List<Assertion> assertions) implements Node {

        public Model {
            elements = List.copyOf(elements);
            objectives = List.copyOf(objectives);
            assertions = List.copyOf(assertions);
        }
    }

    /**
     * Where a clafer or constraint of the core comes from, as
     * {@code --record-provenance} writes it: the rich construct that a
     * lowering made it from, and the position of that construct, e.g.,
     * {@code // final, model.cfr:56:5}. Not a node: it is read from a
     * comment, and no backend needs it.
     *
     * @param construct the rich construct, e.g., {@code final} or
     * {@code never between}
     * @param position its position, if the compiler knew one
     */
    public record Origin(String construct, Optional<Position> position) {
    }

    /**
     * A position in a source file, as in the compiler's messages.
     *
     * @param file the file name
     * @param line the line
     * @param column the column
     */
    public record Position(String file, int line, int column) {

        @Override
        public String toString() {
            return file + ":" + line + ":" + column;
        }
    }

    /**
     * A name as written: of a clafer, a variable, or a type.
     *
     * @param span where it is
     * @param text the name
     */
    public record Name(SourceSpan span, String text) implements Node {
    }

    /**
     * A cardinality, {@code low..high}, or {@code low..*}.
     *
     * @param span where it is
     * @param low the lower bound
     * @param high the upper bound, or {@link #Unbounded}
     */
    public record Card(SourceSpan span, int low, int high) implements Node {

        /**
         * The upper bound {@code *}.
         */
        public static final int Unbounded = -1;

        public boolean isUnbounded() {
            return high == Unbounded;
        }

        /**
         * @return whether this is {@code 0..*}
         */
        public boolean isAny() {
            return low == 0 && high == Unbounded;
        }

        @Override
        public String toString() {
            return low + ".." + (high == Unbounded ? "*" : Integer.toString(high));
        }
    }

    // ------------------------------------------------------------------
    // Elements
    // ------------------------------------------------------------------
    /**
     * A declaration or constraint of a model or of a clafer.
     */
    public sealed interface Element extends Node permits ClaferDecl, Constraint, EnumDecl, RootBlock {
    }

    /**
     * A clafer:
     * {@code [abstract] [modifiers] gcard name [: super] [-> | ->> target] card [initializer] { elements }}.
     * The core parts are its fields; the rich ones are nodes of their own.
     *
     * @param span from its first token to its closing brace
     * @param isAbstract whether it is abstract
     * @param modifiers its temporal modifiers, rich
     * @param groupCard its group card, as written
     * @param name its name, the compiler's UID
     * @param superName its super clafer
     * @param ref its own reference
     * @param card its multiplicity as the core has it: {@code 0..*} for a
     * top-level abstract clafer, whose written bound is its
     * {@code abstractMultiplicity}
     * @param abstractMultiplicity the written bound of a top-level abstract
     * clafer, {@code abstract Tools 3..6}, rich
     * @param initializer its initializer, rich
     * @param elements its constraints and subclafers, in the order written
     * @param origin the rich construct it was lowered from
     */
    public record ClaferDecl(SourceSpan span, boolean isAbstract, List<Modifier> modifiers, Card groupCard,
            Name name, Optional<Name> superName, Optional<RefDecl> ref, Card card,
            Optional<AbstractMultiplicity> abstractMultiplicity, Optional<Initializer> initializer,
            List<Element> elements, Optional<Origin> origin) implements Element {

        public ClaferDecl {
            modifiers = List.copyOf(modifiers);
            elements = List.copyOf(elements);
        }
    }

    /**
     * A reference: {@code -> target}, a set, or {@code ->> target}, a bag.
     *
     * @param span from the arrow to the target
     * @param isSet whether the reference is {@code ->}, whose targets are
     * distinct
     * @param target the target: a clafer, or {@code int}, {@code string},
     * {@code real}, {@code double}, {@code decimal}
     */
    public record RefDecl(SourceSpan span, boolean isSet, Name target) implements Node {
    }

    /**
     * A temporal modifier of a clafer, rich.
     *
     * @param span where it is
     * @param kind which one
     */
    public record Modifier(SourceSpan span, ModifierKind kind) implements Node {

        @Override
        public boolean isRich() {
            return true;
        }
    }

    /**
     * The temporal modifiers, by their keywords.
     */
    public enum ModifierKind {
        INITIAL("initial"), FINAL("final"), FINALREF("finalref"), FINALTARGET("finaltarget");

        private final String keyword;

        ModifierKind(String keyword) {
            this.keyword = keyword;
        }

        public String keyword() {
            return keyword;
        }
    }

    /**
     * An initializer, rich: {@code = value}, or {@code := each value} or
     * {@code := together value}, with its clock written out
     * (kbak/clafer-semantics#194).
     *
     * @param span from the operator to the end of the value
     * @param kind the operator and clock
     * @param value the value
     */
    public record Initializer(SourceSpan span, InitKind kind, Exp value) implements Node {

        @Override
        public boolean isRich() {
            return true;
        }
    }

    /**
     * The initializers, by their syntax.
     */
    public enum InitKind {
        /**
         * {@code =}: the value in every state.
         */
        FIXED("="),
        /**
         * {@code := each}: the value when each instance is born.
         */
        EACH(":= each"),
        /**
         * {@code := together}: the value of the set when the parent is born.
         */
        TOGETHER(":= together");

        private final String syntax;

        InitKind(String syntax) {
            this.syntax = syntax;
        }

        public String syntax() {
            return syntax;
        }
    }

    /**
     * The written bound of a top-level abstract clafer, which counts all its
     * instances, rich: {@code abstract Tools 3..6}. A nested abstract
     * multiplicity is core, the clafer's {@code card}.
     *
     * @param span where the bound is
     * @param card the bound
     */
    public record AbstractMultiplicity(SourceSpan span, Card card) implements Node {

        @Override
        public boolean isRich() {
            return true;
        }
    }

    /**
     * A constraint, {@code [ exp ]}.
     *
     * @param span from the bracket to the bracket
     * @param exp the formula
     * @param origin the rich construct it was lowered from
     */
    public record Constraint(SourceSpan span, Exp exp, Optional<Origin> origin) implements Element {
    }

    /**
     * An enumeration, rich: {@code enum E = a | b}, at the top level.
     *
     * @param span the declaration
     * @param name the enumeration
     * @param literals its literals, in order
     */
    public record EnumDecl(SourceSpan span, Name name, List<Name> literals) implements Element {

        public EnumDecl {
            literals = List.copyOf(literals);
        }

        @Override
        public boolean isRich() {
            return true;
        }
    }

    /**
     * Constraints directly under the root, {@code root { [ exp ] }}, as
     * chocosolver's {@code AstPrinter} prints a model of its Java API that
     * has them; Clafer has none.
     *
     * @param span the block
     * @param constraints the constraints
     */
    public record RootBlock(SourceSpan span, List<Constraint> constraints) implements Element {

        public RootBlock {
            constraints = List.copyOf(constraints);
        }
    }

    /**
     * An objective, {@code << max exp >>} or {@code << min exp >>}.
     *
     * @param span the objective
     * @param maximize whether it maximizes
     * @param exp what it optimizes
     */
    public record Objective(SourceSpan span, boolean maximize, Exp exp) implements Node {
    }

    /**
     * An assertion, {@code assert [ exp ]}.
     *
     * @param span the assertion
     * @param exp the formula
     * @param origin the rich construct it was lowered from
     */
    public record Assertion(SourceSpan span, Exp exp, Optional<Origin> origin) implements Node {
    }

    // ------------------------------------------------------------------
    // Expressions
    // ------------------------------------------------------------------
    /**
     * An expression. The explicit languages parenthesize every operand that is
     * not an atom or a navigation, so the tree has no precedence to keep.
     */
    public sealed interface Exp extends Node permits This, NameExp, IntLit, DecimalLit, DoubleLit, StringLit,
            NoClafer, Join, Dref, ParentOf, Unary, Binary, IfThenElse, Quantified, QuantifiedSet, Call, RelationOf, Cast,
            Connected, ConstantSet, Temporal, Anchor, Pattern, Transition {
    }

    /**
     * {@code this}.
     */
    public record This(SourceSpan span) implements Exp {
    }

    /**
     * A name in an expression: a clafer, all its instances or, on the right
     * of a navigation, its relation, or a variable.
     *
     * @param span where it is
     * @param name the name
     */
    public record NameExp(SourceSpan span, String name) implements Exp {
    }

    /**
     * An integer, {@code -3} with its sign.
     */
    public record IntLit(SourceSpan span, long value) implements Exp {
    }

    /**
     * A decimal as written, e.g., {@code 19.99} or {@code -0.5}.
     */
    public record DecimalLit(SourceSpan span, String text) implements Exp {
    }

    /**
     * A double as written, with its exponent, e.g., {@code 1.2e2}.
     */
    public record DoubleLit(SourceSpan span, String text) implements Exp {
    }

    /**
     * A string literal as written, with its quotes and escapes.
     */
    public record StringLit(SourceSpan span, String text) implements Exp {
    }

    /**
     * {@code noclafer}, the empty set.
     */
    public record NoClafer(SourceSpan span) implements Exp {
    }

    /**
     * A navigation, {@code left.right}: the right is a name (a child
     * relation), or in chocosolver's printing a relation or a parenthesized
     * set.
     */
    public record Join(SourceSpan span, Exp left, Exp right) implements Exp {
    }

    /**
     * {@code left.dref}.
     */
    public record Dref(SourceSpan span, Exp left) implements Exp {
    }

    /**
     * {@code left.parent}.
     */
    public record ParentOf(SourceSpan span, Exp left) implements Exp {
    }

    /**
     * A prefix operator.
     */
    public record Unary(SourceSpan span, UnaryOp op, Exp arg) implements Exp {
    }

    /**
     * The prefix operators. {@code sum} and {@code product} are printed with
     * the {@code .dref} they take from each instance, which is not in the
     * tree: {@code sum x.dref} is {@code Unary(SUM, x)}.
     */
    public enum UnaryOp {
        NOT("!"), MINUS("-"), CARD("#"), SUM("sum "), PRODUCT("product "), MAX("max "), MIN("min "),
        LENGTH("length "), INVERSE("~"), CLOSURE("^"), REFLEXIVE_CLOSURE("*");

        private final String syntax;

        UnaryOp(String syntax) {
            this.syntax = syntax;
        }

        /**
         * @return the operator as printed before its operand, with its space
         */
        public String syntax() {
            return syntax;
        }
    }

    /**
     * A chain of one binary operator: {@code a op b op c}. The compiler
     * prints two operands; chocosolver's printer chains the associative
     * ones.
     */
    public record Binary(SourceSpan span, BinaryOp op, List<Exp> operands) implements Exp {

        public Binary {
            operands = List.copyOf(operands);
        }
    }

    /**
     * The binary operators, by their syntax.
     */
    public enum BinaryOp {
        ADD("+"), SUB("-"), MUL("*"), DIV("/"), DECIMAL_MUL("*."), DECIMAL_DIV("/."), MOD("%"),
        AND("&&"), OR("||"), IMPLIES("=>"), IFF("<=>"), XOR("xor"),
        LT("<"), LE("<="), GT(">"), GE(">="), EQ("="), NE("!="), IN("in"), NOT_IN("not in"),
        UNION("++"), DIFF("--"), INTER("**"), DOMAIN("<:"), RANGE(":>");

        private final String syntax;

        BinaryOp(String syntax) {
            this.syntax = syntax;
        }

        public String syntax() {
            return syntax;
        }
    }

    /**
     * {@code if c then a else b}, a formula or a set.
     */
    public record IfThenElse(SourceSpan span, Exp condition, Exp then, Exp otherwise) implements Exp {
    }

    /**
     * {@code q decls | body}.
     */
    public record Quantified(SourceSpan span, Quantifier quantifier, List<Decl> decls, Exp body) implements Exp {

        public Quantified {
            decls = List.copyOf(decls);
        }
    }

    /**
     * The quantifiers.
     */
    public enum Quantifier {
        ALL("all"), SOME("some"), NO("no"), LONE("lone"), ONE("one");

        private final String keyword;

        Quantifier(String keyword) {
            this.keyword = keyword;
        }

        public String keyword() {
            return keyword;
        }
    }

    /**
     * The declarations of a quantifier, {@code [disj] x; y : set}.
     */
    public record Decl(SourceSpan span, boolean disjoint, List<Name> locals, Exp set) implements Node {

        public Decl {
            locals = List.copyOf(locals);
        }
    }

    /**
     * A quantifier over a set, without variables: {@code some x}, {@code no x},
     * {@code lone x}, {@code one x}, {@code all x}. It is a formula on the set
     * as a whole, e.g., {@code some x} holds if x has an element, which needs
     * no count.
     */
    public record QuantifiedSet(SourceSpan span, Quantifier quantifier, Exp set) implements Exp {
    }

    /**
     * A function of strings, {@code concat(a, b)}, {@code prefix(a, b)} or
     * {@code suffix(a, b)}.
     */
    public record Call(SourceSpan span, String function, List<Exp> args) implements Exp {

        public Call {
            args = List.copyOf(args);
        }
    }

    /**
     * {@code dref(c)} or {@code parent(c)}, a relation of a clafer, as
     * chocosolver's printer writes it.
     */
    public record RelationOf(SourceSpan span, boolean isDref, Name clafer) implements Exp {
    }

    /**
     * {@code upcast(e, T)} or {@code downcast(e, T)}, as chocosolver's
     * printer writes the casts of its analysis.
     */
    public record Cast(SourceSpan span, boolean up, Exp base, List<Name> type) implements Exp {

        public Cast {
            type = List.copyOf(type);
        }
    }

    /**
     * {@code connected(nodes, relation, directed)}.
     */
    public record Connected(SourceSpan span, Exp nodes, Exp relation, boolean directed) implements Exp {
    }

    /**
     * {@code {[1, 2], [3, 4]} :: T * U}, a constant set of chocosolver's
     * printer: one row per column of the type.
     */
    public record ConstantSet(SourceSpan span, List<List<Integer>> rows, List<Name> type) implements Exp {

        public ConstantSet {
            rows = rows.stream().map(List::copyOf).toList();
            type = List.copyOf(type);
        }
    }

    /**
     * A temporal operator of the core: {@code X}, {@code F}, {@code G},
     * {@code previous}, {@code once}, {@code historically} on one formula,
     * {@code U}, {@code W}, {@code since}, {@code triggered} between two.
     */
    public record Temporal(SourceSpan span, TemporalOp op, List<Exp> args) implements Exp {

        public Temporal {
            args = List.copyOf(args);
        }
    }

    /**
     * The temporal operators of the core.
     */
    public enum TemporalOp {
        X("X", 1), F("F", 1), G("G", 1), PREVIOUS("previous", 1), ONCE("once", 1), HISTORICALLY("historically", 1),
        U("U", 2), W("W", 2), SINCE("since", 2), TRIGGERED("triggered", 2);

        private final String keyword;
        private final int arity;

        TemporalOp(String keyword, int arity) {
            this.keyword = keyword;
            this.arity = arity;
        }

        public String keyword() {
            return keyword;
        }

        public int arity() {
            return arity;
        }
    }

    /**
     * An anchor, rich: {@code initially p}, at the birth of the context, or
     * {@code finally p}, at its death.
     */
    public record Anchor(SourceSpan span, boolean initially, Exp body) implements Exp {

        @Override
        public boolean isRich() {
            return true;
        }
    }

    /**
     * A behavioral pattern of Dwyer et al.'s catalogue, rich, e.g.,
     * {@code never p between q and r} or {@code s must precede p}.
     *
     * @param span the pattern
     * @param kind the pattern
     * @param scope where it holds
     * @param args the formulas of the pattern ({@code p}, or {@code s} and
     * {@code p}), then those of the scope
     */
    public record Pattern(SourceSpan span, PatternKind kind, PatternScope scope, List<Exp> args) implements Exp {

        public Pattern {
            args = List.copyOf(args);
        }

        @Override
        public boolean isRich() {
            return true;
        }
    }

    /**
     * The patterns.
     */
    public enum PatternKind {
        NEVER("never", 1), SOMETIME("sometime", 1), LONCE("lonce", 1), ALWAYS("always", 1),
        PRECEDE("precede", 2), FOLLOW("follow", 2);

        private final String keyword;
        private final int arity;

        PatternKind(String keyword, int arity) {
            this.keyword = keyword;
            this.arity = arity;
        }

        public String keyword() {
            return keyword;
        }

        /**
         * @return the number of formulas of the pattern itself
         */
        public int arity() {
            return arity;
        }
    }

    /**
     * The scopes of a pattern.
     */
    public enum PatternScope {
        GLOBALLY(0), BEFORE(1), AFTER(1), BETWEEN(2), AFTER_UNTIL(2);

        private final int arity;

        PatternScope(int arity) {
            this.arity = arity;
        }

        /**
         * @return the number of formulas of the scope
         */
        public int arity() {
            return arity;
        }
    }

    /**
     * A transition, rich: {@code a --> b}, {@code a -[g]-> b},
     * {@code a -->> b} or {@code a -[g]->> b}, whose target may be a chain
     * of transitions.
     */
    public record Transition(SourceSpan span, boolean sync, Exp from, Optional<Exp> guard, Exp to) implements Exp {

        @Override
        public boolean isRich() {
            return true;
        }
    }
}
