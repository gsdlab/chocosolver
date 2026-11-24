package org.clafer.frontend;

import java.util.HashMap;
import java.util.Map;
import java.util.stream.Collectors;

/**
 * Prints an explicit model in the full syntax, as a modeler would write it,
 * with everything left out that {@link Symbols} infers: layout instead of
 * braces, no group card equal to the inherited one, and no card equal to the
 * default one. The constraints stay explicit, so {@link ToIr} reads the
 * result back as the same model, and so does the compiler.
 * <ul>
 * <li>A group card is written as an interval, never as a keyword: it is
 * left out if it is that of the super clafer, or {@code 0..*} without
 * one.</li>
 * <li>A card is left out if it is the default: {@code 0..*} for an abstract
 * clafer, {@code 1..1} at the top level or under the group card
 * {@code 0..*}, otherwise {@code 0..1}.</li>
 * <li>The first line keeps the options that are not {@code --explicit}:
 * {@code --core} and the trace length.</li>
 * </ul>
 */
final class FullSyntaxPrinter {

    private static final Ir.Card Any = new Ir.Card(new SourceSpan(1, 1, 1, 1), 0, Ir.Card.Unbounded);

    private final Map<String, Ir.ClaferDecl> byUid = new HashMap<>();
    private final StringBuilder out = new StringBuilder();

    private FullSyntaxPrinter() {
    }

    static String print(Ir.Model model) {
        FullSyntaxPrinter p = new FullSyntaxPrinter();
        for (Ir.Element e : model.elements()) {
            p.collect(e);
        }
        Ir.Options options = model.options();
        if (options.core() || options.traceLength().isPresent()) {
            p.out.append("//# OPTIONS").append(options.core() ? " --core" : "");
            options.traceLength().ifPresent(tl -> p.out.append(" --tl=").append(tl));
            p.out.append('\n');
        }
        for (Ir.Element e : model.elements()) {
            p.element(e, "", null);
        }
        for (Ir.Objective o : model.objectives()) {
            p.out.append("<< ").append(o.maximize() ? "max " : "min ").append(ExplicitPrinter.print(o.exp()))
                    .append(" >>\n");
        }
        for (Ir.Assertion a : model.assertions()) {
            p.out.append("assert [").append(ExplicitPrinter.print(a.exp())).append("]\n");
        }
        return p.out.toString();
    }

    private void collect(Ir.Element e) {
        if (e instanceof Ir.ClaferDecl c) {
            byUid.put(c.name().text(), c);
            c.elements().forEach(this::collect);
        }
    }

    private void element(Ir.Element e, String indent, Ir.ClaferDecl parent) {
        switch (e) {
            case Ir.Constraint c -> out.append(indent).append('[').append(ExplicitPrinter.print(c.exp())).append("]\n");
            case Ir.EnumDecl en -> out.append(indent).append("enum ").append(en.name().text()).append(" = ")
                    .append(en.literals().stream().map(Ir.Name::text).collect(Collectors.joining(" | "))).append('\n');
            case Ir.RootBlock r -> throw new IllegalArgumentException("A root block has no full syntax");
            case Ir.ClaferDecl c -> clafer(c, indent, parent);
        }
    }

    private Ir.Card inheritedGroupCard(Ir.ClaferDecl c) {
        Ir.ClaferDecl s = c.superName().map(n -> byUid.get(n.text())).orElse(null);
        return s == null ? Any : s.groupCard();
    }

    private static boolean same(Ir.Card a, Ir.Card b) {
        return a.low() == b.low() && a.high() == b.high();
    }

    private void clafer(Ir.ClaferDecl c, String indent, Ir.ClaferDecl parent) {
        out.append(indent);
        if (c.isAbstract()) {
            out.append("abstract ");
        }
        for (Ir.Modifier m : c.modifiers()) {
            out.append(m.kind().keyword()).append(' ');
        }
        if (!same(c.groupCard(), inheritedGroupCard(c))) {
            out.append(c.groupCard()).append(' ');
        }
        out.append(c.name().text());
        c.superName().ifPresent(s -> out.append(" : ").append(s.text()));
        c.ref().ifPresent(r -> out.append(r.isSet() ? " -> " : " ->> ").append(r.target().text()));
        Ir.Card card = c.abstractMultiplicity().map(Ir.AbstractMultiplicity::card).orElse(c.card());
        Ir.Card byDefault = c.isAbstract() ? Any
                : parent == null || same(parent.groupCard(), Any) ? new Ir.Card(card.span(), 1, 1)
                : new Ir.Card(card.span(), 0, 1);
        if (!same(card, byDefault)) {
            out.append(' ').append(card);
        }
        c.initializer().ifPresent(i -> out.append(' ').append(i.kind().syntax()).append(' ')
                .append(ExplicitPrinter.print(i.value())));
        out.append('\n');
        for (Ir.Element e : c.elements()) {
            element(e, indent + "  ", c);
        }
    }
}
