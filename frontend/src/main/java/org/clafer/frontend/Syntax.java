package org.clafer.frontend;

import java.util.List;
import java.util.Optional;

/**
 * The syntax tree of a model in the full Clafer language, as written: the
 * tree of the compiler's {@code clafer.cf} ({@code AbsClafer}), with a span
 * for each node. {@link ClaferParser} builds it, and {@link SyntaxPrinter}
 * prints it as the compiler's {@code printTree} does.
 * <p>
 * The sugar stays as written, e.g., a card {@code ?} or the implicit
 * {@code this} of a name; the later passes read it from the tree. Where the
 * parser recovered from an error, the tree has what it read around it.
 * <p>
 * The records are immutable; their lists are unmodifiable copies.
 */
public final class Syntax {

    private Syntax() {
    }

    /**
     * A node of the tree.
     */
    public sealed interface Node permits Model, Declaration, Ident, Name, TempModifier, GCard, Card, Super,
            Reference, Init, ClaferTransition, Elements, TransArrow, PatternScope, Decl, Exp {

        /**
         * @return where the node is in the source
         */
        SourceSpan span();
    }

    /**
     * A name as written, e.g., a clafer's name or a local variable.
     *
     * @param span where it is
     * @param text the name
     */
    public record Ident(SourceSpan span, String text) implements Node {
    }

    /**
     * A model: its declarations in the order they were written.
     *
     * @param span the whole source
     * @param declarations the enumerations and the top-level elements
     */
    public record Model(SourceSpan span, List<Declaration> declarations) implements Node {

        public Model {
            declarations = List.copyOf(declarations);
        }
    }

    /**
     * A declaration at the top level.
     */
    public sealed interface Declaration extends Node permits EnumDecl, Element {
    }

    /**
     * An enumeration, {@code enum E = a | b}.
     *
     * @param span where it is
     * @param name the name of the enumeration
     * @param literals its literals
     */
    public record EnumDecl(SourceSpan span, Ident name, List<Ident> literals) implements Declaration {

        public EnumDecl {
            literals = List.copyOf(literals);
        }
    }

    /**
     * An element: a clafer, a use of a clafer, a constraint, an objective
     * or an assertion.
     */
    public sealed interface Element extends Declaration permits Clafer, ClaferUse, Constraint, Goal, Assertion {
    }

    /**
     * A clafer, e.g., {@code abstract xor A : B -> int ? = 3 { ... }}.
     *
     * @param span where it is
     * @param isAbstract whether it is written {@code abstract}
     * @param modifiers its temporal modifiers
     * @param groupCard its group cardinality, if written
     * @param name its name
     * @param superClafer its super clafer, if written
     * @param reference its reference, if written
     * @param card its cardinality, if written
     * @param init its initializer, if written
     * @param transition its transition, if written
     * @param elements its elements, if written, in braces or indented
     */
    public record Clafer(SourceSpan span, boolean isAbstract, List<TempModifier> modifiers, Optional<GCard> groupCard,
            Ident name, Optional<Super> superClafer, Optional<Reference> reference, Optional<Card> card,
            Optional<Init> init, Optional<ClaferTransition> transition, Optional<Elements> elements)
            implements Element {

        public Clafer {
            modifiers = List.copyOf(modifiers);
        }
    }

    /**
     * A use of a clafer, {@code `A ? { ... }}.
     *
     * @param span where it is
     * @param name the clafer
     * @param card its cardinality, if written
     * @param elements its elements, if written
     */
    public record ClaferUse(SourceSpan span, Name name, Optional<Card> card, Optional<Elements> elements)
            implements Element {
    }

    /**
     * A constraint, {@code [ e1 e2 ]}: the conjunction of its expressions.
     *
     * @param span where it is
     * @param exps its expressions
     */
    public record Constraint(SourceSpan span, List<Exp> exps) implements Element {

        public Constraint {
            exps = List.copyOf(exps);
        }
    }

    /**
     * An assertion, {@code assert [ e ]}.
     *
     * @param span where it is
     * @param exps its expressions
     */
    public record Assertion(SourceSpan span, List<Exp> exps) implements Element {

        public Assertion {
            exps = List.copyOf(exps);
        }
    }

    /**
     * An objective, {@code << minimize e >>}.
     *
     * @param span where it is
     * @param kind how it is written
     * @param exps its expressions
     */
    public record Goal(SourceSpan span, GoalKind kind, List<Exp> exps) implements Element {

        public Goal {
            exps = List.copyOf(exps);
        }
    }

    /**
     * How an objective is written.
     */
    public enum GoalKind {
        MinDeprecated("min"), MaxDeprecated("max"), Minimize("minimize"), Maximize("maximize");

        private final String syntax;

        GoalKind(String syntax) {
            this.syntax = syntax;
        }

        /**
         * @return the keyword after {@code <<}
         */
        public String syntax() {
            return syntax;
        }
    }

    /**
     * A path of names, {@code a\b}.
     *
     * @param span where it is
     * @param path the names
     */
    public record Name(SourceSpan span, List<Ident> path) implements Node {

        public Name {
            path = List.copyOf(path);
        }
    }

    /**
     * A temporal modifier of a clafer.
     *
     * @param span where it is
     * @param kind which one
     */
    public record TempModifier(SourceSpan span, TempModifierKind kind) implements Node {
    }

    /**
     * The temporal modifiers.
     */
    public enum TempModifierKind {
        Initial("initial"), Final("final"), FinalRef("finalref"), FinalTarget("finaltarget");

        private final String syntax;

        TempModifierKind(String syntax) {
            this.syntax = syntax;
        }

        /**
         * @return the keyword
         */
        public String syntax() {
            return syntax;
        }
    }

    /**
     * A group cardinality: a keyword or an interval.
     */
    public sealed interface GCard extends Node permits GroupKeyword, NCard {
    }

    /**
     * A group cardinality written as a keyword: {@code xor}, {@code or},
     * {@code mux} or {@code opt}.
     *
     * @param span where it is
     * @param keyword the keyword
     */
    public record GroupKeyword(SourceSpan span, String keyword) implements GCard {
    }

    /**
     * A cardinality: {@code ?}, {@code +}, {@code *}, a number or an
     * interval.
     */
    public sealed interface Card extends Node permits CardSymbol, CardNum, NCard {
    }

    /**
     * A cardinality written as {@code ?}, {@code +} or {@code *}.
     *
     * @param span where it is
     * @param symbol the symbol
     */
    public record CardSymbol(SourceSpan span, String symbol) implements Card {
    }

    /**
     * A cardinality written as a number, {@code 2}.
     *
     * @param span where it is
     * @param number the number as written
     */
    public record CardNum(SourceSpan span, String number) implements Card {
    }

    /**
     * An interval, {@code 2..3} or {@code 2..*}, of a cardinality or of a
     * group cardinality.
     *
     * @param span where it is
     * @param low the lower bound as written
     * @param high the upper bound as written, empty for {@code *}
     */
    public record NCard(SourceSpan span, String low, Optional<String> high) implements Card, GCard {
    }

    /**
     * The super clafer, {@code : e}.
     *
     * @param span where it is
     * @param exp the super clafer
     */
    public record Super(SourceSpan span, Exp exp) implements Node {
    }

    /**
     * A reference, {@code -> e} to a set or {@code ->> e} to a bag.
     *
     * @param span where it is
     * @param bag whether it is to a bag
     * @param exp the target
     */
    public record Reference(SourceSpan span, boolean bag, Exp exp) implements Node {
    }

    /**
     * An initializer, {@code = e} or {@code := e}.
     *
     * @param span where it is
     * @param how how it is written
     * @param exp the value
     */
    public record Init(SourceSpan span, InitHow how, Exp exp) implements Node {
    }

    /**
     * How an initializer is written.
     */
    public enum InitHow {
        Constant("="), Default(":="), Each(":= each"), Together(":= together");

        private final String syntax;

        InitHow(String syntax) {
            this.syntax = syntax;
        }

        /**
         * @return the symbol, with its clock if written
         */
        public String syntax() {
            return syntax;
        }
    }

    /**
     * The transition of a clafer, {@code --> e}.
     *
     * @param span where it is
     * @param arrow the arrow
     * @param exp the expression after it
     */
    public record ClaferTransition(SourceSpan span, TransArrow arrow, Exp exp) implements Node {
    }

    /**
     * The elements of a clafer, in braces or indented.
     *
     * @param span where they are
     * @param elements the elements
     */
    public record Elements(SourceSpan span, List<Element> elements) implements Node {

        public Elements {
            elements = List.copyOf(elements);
        }
    }

    /**
     * A transition arrow: {@code -->}, {@code -->>} (synchronous), or with a
     * guard, {@code -[ g ]->} and {@code -[ g ]->>}.
     *
     * @param span where it is
     * @param sync whether it is synchronous
     * @param guard the guard, if written
     */
    public record TransArrow(SourceSpan span, boolean sync, Optional<Exp> guard) implements Node {
    }

    /**
     * The scope of a pattern: {@code before e}, {@code after e},
     * {@code between e and e}, {@code after e until e}.
     *
     * @param span where it is
     * @param kind which one
     * @param exps its expressions
     */
    public record PatternScope(SourceSpan span, ScopeKind kind, List<Exp> exps) implements Node {

        public PatternScope {
            exps = List.copyOf(exps);
        }
    }

    /**
     * The scopes of a pattern.
     */
    public enum ScopeKind {
        Before, After, BetweenAnd, AfterUntil
    }

    /**
     * The declaration of a quantifier, {@code x; y : e}.
     *
     * @param span where it is
     * @param locals the local variables
     * @param exp the set they range over
     */
    public record Decl(SourceSpan span, List<Ident> locals, Exp exp) implements Node {

        public Decl {
            locals = List.copyOf(locals);
        }
    }

    // ------------------------------------------------------------------
    // Expressions
    // ------------------------------------------------------------------
    /**
     * An expression.
     */
    public sealed interface Exp extends Node permits TransitionExp, Quantified, IfThenElse, Let, Pattern, Anchor,
            Binary, Unary, QuantExp, ClaferId, Literal {

        /**
         * @return the level of the expression in {@code clafer.cf}, from 0
         * (a transition) to 27 (a name or a literal)
         */
        int level();
    }

    /**
     * A transition, {@code e1 --> e2}.
     *
     * @param span where it is
     * @param from the expression before the arrow
     * @param arrow the arrow
     * @param to the expression after it
     */
    public record TransitionExp(SourceSpan span, Exp from, TransArrow arrow, Exp to) implements Exp {

        @Override
        public int level() {
            return 0;
        }
    }

    /**
     * The quantifiers.
     */
    public enum Quant {
        All("all"), No("no"), Not("not"), Lone("lone"), One("one"), Some("some");

        private final String syntax;

        Quant(String syntax) {
            this.syntax = syntax;
        }

        /**
         * @return the keyword
         */
        public String syntax() {
            return syntax;
        }
    }

    /**
     * A quantified expression with a declaration,
     * {@code some disj x : e | body}.
     *
     * @param span where it is
     * @param quant the quantifier
     * @param disj whether it is written {@code disj}
     * @param decl the declaration
     * @param body the body
     */
    public record Quantified(SourceSpan span, Quant quant, boolean disj, Decl decl, Exp body) implements Exp {

        @Override
        public int level() {
            return 1;
        }
    }

    /**
     * {@code if c then t else e}.
     *
     * @param span where it is
     * @param condition the condition
     * @param then the expression if it holds
     * @param otherwise the expression if it does not
     */
    public record IfThenElse(SourceSpan span, Exp condition, Exp then, Exp otherwise) implements Exp {

        @Override
        public int level() {
            return 1;
        }
    }

    /**
     * {@code let x = v in body}.
     *
     * @param span where it is
     * @param local the local variable
     * @param value its value
     * @param body the body
     */
    public record Let(SourceSpan span, Ident local, Exp value, Exp body) implements Exp {

        @Override
        public int level() {
            return 1;
        }
    }

    /**
     * The behavioral patterns.
     */
    public enum PatternKind {
        Never, Sometime, LessOrOnce, Always, Precede, Follow
    }

    /**
     * A behavioral pattern, e.g., {@code never e after s}, or
     * {@code a must precede b}.
     *
     * @param span where it is
     * @param kind which one
     * @param exps the expression, or the two of precede and follow
     * @param scope its scope, if written
     */
    public record Pattern(SourceSpan span, PatternKind kind, List<Exp> exps, Optional<PatternScope> scope)
            implements Exp {

        public Pattern {
            exps = List.copyOf(exps);
        }

        @Override
        public int level() {
            return 2;
        }
    }

    /**
     * {@code initially e} or {@code finally e}.
     *
     * @param span where it is
     * @param initially whether it is {@code initially}
     * @param exp the expression
     */
    public record Anchor(SourceSpan span, boolean initially, Exp exp) implements Exp {

        @Override
        public int level() {
            return 2;
        }
    }

    /**
     * The binary operators, with their symbols and levels. Each is left
     * associative: its right operand is one level higher.
     */
    public enum BinaryOp {
        Iff("<=>", 3), Implies("=>", 4), Or("||", 5), Xor("xor", 6), And("&&", 7),
        LtlU("U", 8), Until("until", 8), Since("since", 8), Triggered("triggered", 8),
        LtlW("W", 9), WeakUntil("weakuntil", 9),
        Lt("<", 15), Gt(">", 15), Eq("=", 15), Lte("<=", 15), Gte(">=", 15), Neq("!=", 15), In("in", 15),
        NotIn("not in", 15),
        Add("+", 17), Sub("-", 17), Mul("*", 18), Div("/", 18), Rem("%", 18),
        Domain("<:", 21), Range(":>", 22), Union("++", 23), UnionCom(",", 23), Difference("--", 24),
        Intersection("**", 25), IntersectionDeprecated("&", 26), Join(".", 26);

        private final String syntax;
        private final int level;

        BinaryOp(String syntax, int level) {
            this.syntax = syntax;
            this.level = level;
        }

        /**
         * @return the operator as written
         */
        public String syntax() {
            return syntax;
        }

        /**
         * @return the level of {@code clafer.cf}
         */
        public int level() {
            return level;
        }
    }

    /**
     * A binary expression.
     *
     * @param span where it is
     * @param op the operator
     * @param left the left operand
     * @param right the right operand
     */
    public record Binary(SourceSpan span, BinaryOp op, Exp left, Exp right) implements Exp {

        @Override
        public int level() {
            return op.level();
        }
    }

    /**
     * The prefix operators, with their symbols, levels and the levels of
     * their operands.
     */
    public enum UnaryOp {
        LtlF("F", 10, 10), Eventually("eventually", 10, 10), LtlG("G", 10, 10), Globally("globally", 10, 10),
        LtlX("X", 10, 10), Next("next", 10, 10), Previous("previous", 10, 10), Once("once", 10, 10),
        Historically("historically", 10, 10),
        Neg("!", 11, 11),
        GMax("max", 19, 20), GMin("min", 19, 20),
        Sum("sum", 20, 21), Product("product", 20, 21), Card("#", 20, 21), Minus("-", 20, 21);

        private final String syntax;
        private final int level;
        private final int operandLevel;

        UnaryOp(String syntax, int level, int operandLevel) {
            this.syntax = syntax;
            this.level = level;
            this.operandLevel = operandLevel;
        }

        /**
         * @return the operator as written
         */
        public String syntax() {
            return syntax;
        }

        /**
         * @return the level of {@code clafer.cf}
         */
        public int level() {
            return level;
        }

        /**
         * @return the level of the operand
         */
        public int operandLevel() {
            return operandLevel;
        }
    }

    /**
     * A prefix expression.
     *
     * @param span where it is
     * @param op the operator
     * @param exp the operand
     */
    public record Unary(SourceSpan span, UnaryOp op, Exp exp) implements Exp {

        @Override
        public int level() {
            return op.level();
        }
    }

    /**
     * A quantified expression without a declaration, {@code some e}.
     *
     * @param span where it is
     * @param quant the quantifier, not {@link Quant#All}
     * @param exp the expression
     */
    public record QuantExp(SourceSpan span, Quant quant, Exp exp) implements Exp {

        @Override
        public int level() {
            return 16;
        }
    }

    /**
     * A name in an expression.
     *
     * @param span where it is
     * @param name the name
     */
    public record ClaferId(SourceSpan span, Name name) implements Exp {

        @Override
        public int level() {
            return 27;
        }
    }

    /**
     * The kinds of literals.
     */
    public enum LiteralKind {
        Int, Double, Real, Str
    }

    /**
     * A literal: an integer, a double, a real or a string, as written.
     *
     * @param span where it is
     * @param kind its kind
     * @param text the literal as written, a string with its quotes and
     * escapes
     */
    public record Literal(SourceSpan span, LiteralKind kind, String text) implements Exp {

        @Override
        public int level() {
            return 27;
        }
    }
}
