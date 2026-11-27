package org.clafer.frontend;

import java.util.ArrayList;
import java.util.IdentityHashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import org.clafer.frontend.Ir.*;

/**
 * The core of the explicit IR: a model is core if and only if it has no rich
 * node ({@link Node#isRich()}).
 */
public final class Core {

    private Core() {
    }

    /**
     * @param model a model
     * @return its rich nodes, outermost first, in the order of the source
     */
    public static List<Node> richNodes(Model model) {
        List<Node> rich = new ArrayList<>();
        walk(model, rich);
        return rich;
    }

    /**
     * @param model a model
     * @return the diagnostics of a model that declares itself the core
     * ({@code --core}) but has rich constructs, one at each
     */
    public static List<Diagnostic> richInCore(Model model) {
        List<Diagnostic> diagnostics = new ArrayList<>();
        if (model.options().core()) {
            Map<Node, ClaferDecl> owners = new IdentityHashMap<>();
            ownersOf(model, owners);
            for (Node node : richNodes(model)) {
                diagnostics.add(new Diagnostic(positionOf(node, owners), "The core has no " + describe(node)
                        + ": the model declares itself the core (--core)"));
            }
        }
        return diagnostics;
    }

    /**
     * The position of a rich construct, as the compiler gives it: a
     * multiplicity of an abstract clafer at its clafer, a pattern or a
     * transition at its first operand, as in the provenance comments.
     */
    private static SourceSpan positionOf(Node node, Map<Node, ClaferDecl> owners) {
        return switch (node) {
            case AbstractMultiplicity a -> owners.get(a).span();
            case Pattern p -> ToCore.position(p);
            case Transition t -> ToCore.position(t);
            default -> node.span();
        };
    }

    /**
     * The clafer of each abstract multiplicity.
     */
    private static void ownersOf(Node node, Map<Node, ClaferDecl> owners) {
        if (node instanceof ClaferDecl d) {
            d.abstractMultiplicity().ifPresent(a -> owners.put(a, d));
        }
        for (Node child : children(node)) {
            ownersOf(child, owners);
        }
    }

    /**
     * @param node a node
     * @return what it is, e.g., "enumeration"
     */
    public static String describe(Node node) {
        return switch (node) {
            case EnumDecl e -> "enumeration";
            case Modifier m -> "temporal modifier " + m.kind().keyword();
            case Initializer i -> "initializer " + i.kind().syntax();
            case AbstractMultiplicity a -> "multiplicity of a top-level abstract clafer";
            case Anchor a -> "anchor " + (a.initially() ? "initially" : "finally");
            case Pattern p -> "behavioral pattern " + ToCore.patternOp(p);
            case Transition t -> "transition";
            default -> node.getClass().getSimpleName();
        };
    }

    private static void walk(Node node, List<Node> rich) {
        if (node.isRich()) {
            rich.add(node);
        }
        for (Node child : children(node)) {
            walk(child, rich);
        }
    }

    /**
     * @param node a node
     * @return its children, in the order of the source
     */
    public static List<Node> children(Node node) {
        List<Node> c = new ArrayList<>();
        switch (node) {
            case Model m -> {
                c.addAll(m.elements());
                c.addAll(m.objectives());
                c.addAll(m.assertions());
            }
            case ClaferDecl d -> {
                c.addAll(d.modifiers());
                c.add(d.groupCard());
                c.add(d.name());
                d.superName().ifPresent(c::add);
                d.ref().ifPresent(c::add);
                c.add(d.card());
                d.abstractMultiplicity().ifPresent(c::add);
                d.initializer().ifPresent(c::add);
                c.addAll(d.elements());
            }
            case RefDecl r -> c.add(r.target());
            case Initializer i -> c.add(i.value());
            case AbstractMultiplicity a -> c.add(a.card());
            case Constraint k -> c.add(k.exp());
            case EnumDecl e -> {
                c.add(e.name());
                c.addAll(e.literals());
            }
            case RootBlock r -> c.addAll(r.constraints());
            case Objective o -> c.add(o.exp());
            case Assertion a -> c.add(a.exp());
            case Decl d -> {
                c.addAll(d.locals());
                c.add(d.set());
            }
            case Join e -> {
                c.add(e.left());
                c.add(e.right());
            }
            case Dref e -> c.add(e.left());
            case ParentOf e -> c.add(e.left());
            case Unary e -> c.add(e.arg());
            case Binary e -> c.addAll(e.operands());
            case IfThenElse e -> {
                c.add(e.condition());
                c.add(e.then());
                c.add(e.otherwise());
            }
            case Quantified e -> {
                c.addAll(e.decls());
                c.add(e.body());
            }
            case QuantifiedSet e -> c.add(e.set());
            case Call e -> c.addAll(e.args());
            case RelationOf e -> c.add(e.clafer());
            case Cast e -> {
                c.add(e.base());
                c.addAll(e.type());
            }
            case Connected e -> {
                c.add(e.nodes());
                c.add(e.relation());
            }
            case ConstantSet e -> c.addAll(e.type());
            case Temporal e -> c.addAll(e.args());
            case Anchor e -> c.add(e.body());
            case Pattern e -> c.addAll(e.args());
            case Transition e -> {
                c.add(e.from());
                e.guard().ifPresent(c::add);
                c.add(e.to());
            }
            case This e -> {
            }
            case NameExp e -> {
            }
            case IntLit e -> {
            }
            case DecimalLit e -> {
            }
            case DoubleLit e -> {
            }
            case StringLit e -> {
            }
            case NoClafer e -> {
            }
            case Name e -> {
            }
            case Card e -> {
            }
            case Modifier e -> {
            }
        }
        return c;
    }

    /**
     * @param node a node
     * @return the first temporal node of a tree, core or rich, if any: a
     * temporal operator, modifier, anchor, pattern or transition
     */
    public static Optional<Node> firstTemporal(Node node) {
        if (node instanceof Temporal || node instanceof Modifier || node instanceof Anchor
                || node instanceof Pattern || node instanceof Transition) {
            return Optional.of(node);
        }
        for (Node child : children(node)) {
            Optional<Node> t = firstTemporal(child);
            if (t.isPresent()) {
                return t;
            }
        }
        return Optional.empty();
    }
}
