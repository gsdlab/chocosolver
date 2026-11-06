package org.clafer.ast;

import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Deque;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;
import org.clafer.assertion.Assertion;
import org.clafer.objective.Objective;

/**
 * Prints a model in a Clafer-like syntax with nothing left implicit: the
 * clafers by their names (the compiler's UIDs), every card and group card,
 * every {@code this}, {@code parent} and {@code dref}. The output is a Clafer
 * model, which the Clafer compiler reads with {@code -f} (the names are already
 * UIDs) and {@code -l} (no layout). Two models print the same when they have
 * the same clafers, constraints, objectives and assertions, so the output can
 * be compared, e.g., between two compilers of the same Clafer model. The
 * scopes, all of them, go to the model's {@code .cfr-scope} file
 * ({@link org.clafer.scope.ScopeFile}). The solver options (e.g., branching
 * priorities) are not printed.
 * <p>
 * The clafers, their children and their constraints are printed in the order
 * they were added, which is the order of the source and of the instances,
 * abstract and concrete clafers interleaved (see {@link #sourceOrder}).
 * Source spans are not printed.
 * <p>
 * Nesting is in braces, as in Clafer without layout: every clafer has its
 * block, {@code {}} if it is empty. The indentation is only for reading;
 * whitespace and line breaks carry no meaning.
 * <p>
 * Values are printed as they apply, declarations where they are declared. A
 * group card and a multiplicity are values: a clafer without its own group
 * card has its super clafer's, as chocosolver solves it, and {@code 0..*} if
 * no super clafer has one; an abstract clafer's multiplicity is {@code 0..*}.
 * Children, constraints and references are declarations: they stay under the
 * clafer that declares them. A reference is printed only where it is declared
 * or redefined, since in Clafer a reference written on a sub clafer redefines
 * the inherited one, even with the same target.
 * <p>
 * Expressions are fully parenthesized, except for navigation ({@code .}),
 * which binds tightest. {@code sum} and {@code product} take a set of clafers
 * and add (multiply) the {@code dref} of each instance, so two instances with
 * the same value count twice; this is printed as in Clafer, e.g.,
 * {@code sum c0_Phone . c0_cost . dref}. An explicit dereference inside would
 * print as {@code . dref . dref}.
 */
public class AstPrinter {

    private static final String Indent = "  ";

    private AstPrinter() {
    }

    /**
     * Prints a model with its objectives and assertions. Its scopes go to a
     * separate file, see {@link org.clafer.scope.ScopeFile#print}.
     *
     * @param model the model
     * @param objectives the objectives
     * @param assertions the assertions
     * @return the model as text, one line per clafer, constraint, objective
     * and assertion
     */
    public static String print(AstModel model, Objective[] objectives, Assertion[] assertions) {
        StringBuilder out = new StringBuilder();
        out.append(print(model));
        if (objectives.length > 0 || assertions.length > 0) {
            out.append('\n');
        }
        for (Objective objective : objectives) {
            out.append("<< ").append(objective.isMaximize() ? "max " : "min ")
                    .append(print(objective.getExpr())).append(" >>\n");
        }
        for (Assertion assertion : assertions) {
            out.append("assert [").append(print(assertion.getExpr())).append("]\n");
        }
        return out.toString();
    }

    /**
     * Prints the clafers and constraints of a model.
     *
     * @param model the model
     * @return the model as text
     */
    public static String print(AstModel model) {
        StringBuilder out = new StringBuilder();
        AstAbstractClafer top = model.getAbstractRoot();
        for (AstConstraint constraint : top.getConstraints()) {
            printConstraint(constraint, "", out);
        }
        List<AstAbstractClafer> abstracts = top.getAbstractChildren().stream()
                .filter(c -> c != model.getTypeRoot()).collect(Collectors.toList());
        for (AstClafer clafer : sourceOrder(abstracts, top.getChildren())) {
            printClafer(clafer, model, "", out);
        }
        // Constraints directly under root, as opposed to the top level, are
        // possible in the Java API.
        if (model.getRoot().hasConstraints()) {
            out.append("root {\n");
            for (AstConstraint constraint : model.getRoot().getConstraints()) {
                printConstraint(constraint, Indent, out);
            }
            out.append("}\n");
        }
        return out.toString();
    }

    private static void printClafer(AstClafer clafer, AstModel model, String indent, StringBuilder out) {
        out.append(indent);
        if (clafer instanceof AstAbstractClafer) {
            out.append("abstract ");
        }
        Card groupCard = AstUtil.getInheritedGroupCard(clafer);
        out.append(groupCard == null ? "0..*" : card(groupCard)).append(' ');
        out.append(clafer.getName());
        if (clafer.hasSuperClafer() && clafer.getSuperClafer() != model.getTypeRoot()) {
            out.append(" : ").append(clafer.getSuperClafer().getName());
        }
        // Only the clafer's own reference: an inherited one written here would redefine it.
        AstRef ref = clafer.getRef();
        if (ref != null) {
            out.append(ref.isUnique() ? " -> " : " ->> ").append(ref.getTargetType().getName());
        }
        // An abstract clafer has no card of its own: its instances are those
        // of the clafers that extend it.
        out.append(' ').append(clafer instanceof AstConcreteClafer concrete ? card(concrete.getCard()) : "0..*");
        if (!clafer.hasConstraints() && !clafer.hasAbstractChildren() && !clafer.hasChildren()) {
            out.append(" {}\n");
            return;
        }
        out.append(" {\n");
        String inner = indent + Indent;
        for (AstConstraint constraint : clafer.getConstraints()) {
            printConstraint(constraint, inner, out);
        }
        for (AstClafer child : sourceOrder(clafer.getAbstractChildren(), clafer.getChildren())) {
            printClafer(child, model, inner, out);
        }
        out.append(indent).append("}\n");
    }

    /**
     * The abstract and concrete children of a clafer, which the model keeps
     * apart, merged in the order of the source model. The order matters to
     * the solver: a sub clafer's place among the subs of its super clafer
     * numbers its instances. The model built from the source adds the subs in
     * the order of the source, and a clafer without a super clafer stays among
     * the subs of {@code #clafer#} in the order of creation; so a list of subs
     * that holds clafers from the subtrees of two children tells which child
     * came first. Without one, the order is not seen by the solver, and the
     * abstract child comes first.
     */
    private static List<AstClafer> sourceOrder(List<AstAbstractClafer> abstracts, List<AstConcreteClafer> concretes) {
        if (abstracts.isEmpty() || concretes.isEmpty()) {
            List<AstClafer> one = new ArrayList<>(abstracts);
            one.addAll(concretes);
            return one;
        }
        // A topological sort of the siblings: each list keeps its order, and
        // a list of subs orders an abstract and a concrete child. Without an
        // order between two, the abstract child comes first.
        List<AstClafer> siblings = new ArrayList<>(abstracts);
        siblings.addAll(concretes);
        int n = siblings.size();
        List<List<Integer>> after = new ArrayList<>();
        int[] before = new int[n];
        for (int k = 0; k < n; k++) {
            after.add(new ArrayList<>());
        }
        int a = abstracts.size();
        for (int k = 0; k + 1 < n; k++) {
            if (k + 1 != a) {
                after.get(k).add(k + 1);
                before[k + 1]++;
            }
        }
        for (int i = 0; i < a; i++) {
            for (int j = a; j < n; j++) {
                Boolean abstractFirst = before(siblings.get(i), siblings.get(j));
                if (abstractFirst != null) {
                    int from = abstractFirst ? i : j, to = abstractFirst ? j : i;
                    after.get(from).add(to);
                    before[to]++;
                }
            }
        }
        List<AstClafer> sorted = new ArrayList<>(n);
        boolean[] placed = new boolean[n];
        while (sorted.size() < n) {
            // The first sibling, in the default order, that nothing must
            // precede; if the orders contradict each other, the first left.
            int next = -1;
            for (int k = 0; k < n && next < 0; k++) {
                if (!placed[k] && before[k] == 0) {
                    next = k;
                }
            }
            for (int k = 0; k < n && next < 0; k++) {
                if (!placed[k]) {
                    next = k;
                }
            }
            placed[next] = true;
            sorted.add(siblings.get(next));
            for (int k : after.get(next)) {
                before[k]--;
            }
        }
        return sorted;
    }

    /**
     * @return whether the subtree of a comes before the subtree of b in a list
     * of subs that has clafers of both, or null if no list has
     */
    private static Boolean before(AstClafer a, AstClafer b) {
        Map<AstClafer, Integer> firstOfB = new HashMap<>();
        for (AstClafer y : subtree(b)) {
            if (y.hasSuperClafer()) {
                firstOfB.merge(y.getSuperClafer(), y.getSuperClafer().getSubs().indexOf(y), Math::min);
            }
        }
        for (AstClafer x : subtree(a)) {
            if (x.hasSuperClafer()) {
                Integer other = firstOfB.get(x.getSuperClafer());
                if (other != null) {
                    return x.getSuperClafer().getSubs().indexOf(x) < other;
                }
            }
        }
        return null;
    }

    private static List<AstClafer> subtree(AstClafer clafer) {
        List<AstClafer> subtree = new ArrayList<>();
        Deque<AstClafer> todo = new ArrayDeque<>();
        todo.push(clafer);
        while (!todo.isEmpty()) {
            AstClafer c = todo.pop();
            subtree.add(c);
            c.getAbstractChildren().forEach(todo::push);
            c.getChildren().forEach(todo::push);
        }
        return subtree;
    }

    private static void printConstraint(AstConstraint constraint, String indent, StringBuilder out) {
        String expr = print(constraint.getExpr());
        out.append(indent).append(constraint.isHard() ? "[" + expr + "]" : "(" + expr + ")").append('\n');
    }

    private static String card(Card card) {
        return card.getLow() + ".." + (card.hasHigh() ? Integer.toString(card.getHigh()) : "*");
    }

    /**
     * Prints an expression.
     *
     * @param expr the expression
     * @return the expression as text
     */
    public static String print(AstExpr expr) {
        return switch (expr) {
            case AstThis e -> "this";
            case AstGlobal e -> e.type().getName();
            case AstLocal e -> e.getName();
            case AstChildRelation e -> e.childType().getName();
            case AstParentRelation e -> "parent(" + e.parentRelation().getName() + ")";
            case AstRefRelation e -> "dref(" + e.ref().getSourceType().getName() + ")";
            case AstConstant e -> constant(e);
            case AstDecimalConstant e -> e.value().toPlainString();
            case AstStringConstant e -> string(e.value());
            case AstJoin e -> operand(e.left()) + " . " + step(e.right());
            case AstJoinParent e -> operand(e.children()) + " . parent";
            case AstJoinRef e -> operand(e.deref()) + " . dref";
            case AstNot e -> "!" + operand(e.expr());
            // -1 is a constant, so the negation of a number is -(1), and of
            // -1 is -(-1) rather than --1, which reads as the difference --.
            case AstMinus e -> "-" + (e.expr() instanceof AstConstant || e.expr() instanceof AstDecimalConstant
                    ? "(" + print(e.expr()) + ")" : operand(e.expr()));
            case AstCard e -> "#" + operand(e.set());
            case AstSum e -> "sum " + operand(e.set()) + " . dref";
            case AstProduct e -> "product " + operand(e.set()) + " . dref";
            case AstMax e -> "max " + operand(e.set());
            case AstMin e -> "min " + operand(e.set());
            case AstLength e -> "length " + operand(e.string());
            case AstInverse e -> "~" + operand(e.relation());
            case AstTransitiveClosure e -> (e.reflexive() ? "*" : "^") + operand(e.relation());
            case AstCompare e -> binary(e.left(), e.op().getSyntax(), e.right());
            case AstSetTest e -> binary(e.left(), e.op().getSyntax(), e.right());
            case AstMembership e -> binary(e.member(), e.op().getSyntax(), e.set());
            case AstMod e -> binary(e.dividend(), "%", e.divisor());
            case AstUnion e -> binary(e.left(), "++", e.right());
            case AstDifference e -> binary(e.left(), "--", e.right());
            case AstIntersection e -> binary(e.left(), "**", e.right());
            case AstDomainRestriction e -> binary(e.domain(), "<:", e.relation());
            case AstRangeRestriction e -> binary(e.relation(), ":>", e.range());
            case AstArithm e -> nary(e.operands(), e.op() == AstArithm.Op.DecimalMul ? "*."
                    : e.op() == AstArithm.Op.DecimalDiv ? "/." : e.op().getSyntax());
            case AstBoolArithm e -> nary(e.operands(), e.op().getSyntax());
            case AstConcat e -> "concat(" + print(e.left()) + ", " + print(e.right()) + ")";
            case AstPrefix e -> "prefix(" + print(e.prefix()) + ", " + print(e.word()) + ")";
            case AstSuffix e -> "suffix(" + print(e.suffix()) + ", " + print(e.word()) + ")";
            case AstConnected e -> "connected(" + print(e.nodes()) + ", " + print(e.relation())
                    + (e.directed() ? ", directed)" : ", undirected)");
            case AstUpcast e -> "upcast(" + print(e.base()) + ", " + e.target() + ")";
            case AstDowncast e -> "downcast(" + print(e.base()) + ", " + e.target() + ")";
            case AstTernary e -> "if " + operand(e.antecedent()) + " then " + operand(e.consequent())
                    + " else " + operand(e.alternative());
            case AstIfThenElse e -> "if " + operand(e.antecedent()) + " then " + operand(e.consequent())
                    + " else " + operand(e.alternative());
            case AstQuantify e -> e.quantifier().getSyntax() + " "
                    + Arrays.stream(e.decls()).map(AstPrinter::decl).collect(Collectors.joining(", "))
                    + " | " + print(e.body());
        };
    }

    private static String decl(AstDecl decl) {
        return (decl.isDisjoint() ? "disj " : "")
                + Arrays.stream(decl.getLocals()).map(AstLocal::getName).collect(Collectors.joining("; "))
                + " : " + print(decl.getBody());
    }

    private static boolean isAtom(AstExpr expr) {
        return switch (expr) {
            case AstThis e -> true;
            case AstGlobal e -> true;
            case AstLocal e -> true;
            case AstChildRelation e -> true;
            case AstParentRelation e -> true;
            case AstRefRelation e -> true;
            case AstConstant e -> true;
            case AstDecimalConstant e -> true;
            case AstStringConstant e -> true;
            case AstConcat e -> true;
            case AstPrefix e -> true;
            case AstSuffix e -> true;
            case AstConnected e -> true;
            case AstUpcast e -> true;
            case AstDowncast e -> true;
            default -> false;
        };
    }

    private static boolean isNavigation(AstExpr expr) {
        return expr instanceof AstJoin || expr instanceof AstJoinParent || expr instanceof AstJoinRef;
    }

    /**
     * An operand of an operator: navigation binds tighter than any operator,
     * so only the other compound expressions are parenthesized.
     */
    private static String operand(AstExpr expr) {
        return isAtom(expr) || isNavigation(expr) ? print(expr) : "(" + print(expr) + ")";
    }

    /**
     * The right of a navigation, parenthesized unless it is an atom.
     */
    private static String step(AstExpr expr) {
        return isAtom(expr) ? print(expr) : "(" + print(expr) + ")";
    }

    private static String binary(AstExpr left, String op, AstExpr right) {
        return operand(left) + " " + op + " " + operand(right);
    }

    private static String nary(AstExpr[] operands, String op) {
        return Arrays.stream(operands).map(AstPrinter::operand).collect(Collectors.joining(" " + op + " "));
    }

    private static String constant(AstConstant constant) {
        int[][] value = constant.value();
        if (constant.type().isInt() && value.length == 1 && value[0].length == 1) {
            return Integer.toString(value[0][0]);
        }
        String tuples = Arrays.stream(value).map(Arrays::toString).collect(Collectors.joining(", "));
        return "{" + tuples + "} :: " + constant.type();
    }

    /**
     * A string from Clafer is its literal, quotes and escapes included, e.g.,
     * {@code "John"}, and chocosolver solves and prints it so; it is printed
     * as it is. Other strings, from the Java API, are quoted and escaped.
     */
    private static String string(String value) {
        if (value.length() >= 2 && value.startsWith("\"") && value.endsWith("\"")) {
            return value;
        }
        StringBuilder out = new StringBuilder("\"");
        for (char c : value.toCharArray()) {
            switch (c) {
                case '"' -> out.append("\\\"");
                case '\\' -> out.append("\\\\");
                case '\n' -> out.append("\\n");
                case '\t' -> out.append("\\t");
                default -> {
                    if (c < ' ' || c > '~') {
                        out.append(String.format("\\u%04x", (int) c));
                    } else {
                        out.append(c);
                    }
                }
            }
        }
        return out.append('"').toString();
    }
}
