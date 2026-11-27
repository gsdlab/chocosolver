package org.clafer.frontend;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import org.clafer.frontend.Symbols.ClaferSymbol;
import org.clafer.frontend.Symbols.Interval;

/**
 * The statistics that the compiler prints for a model ({@code showStats},
 * {@code statsModule}): the numbers of clafers, abstract, concrete and with a
 * reference, of constraints and of goals, the global scope (the largest
 * global cardinality of a clafer, the product of the cards from the top), and
 * whether the names of the clafers are their UIDs.
 */
final class Statistics {

    private Statistics() {
    }

    /**
     * @param result a model without errors
     * @return the lines, each ending with a newline
     */
    static String of(ToIr.Result result) {
        List<ClaferSymbol> clafers = result.names().clafers();
        int abstracts = 0;
        int concretes = 0;
        int references = 0;
        long low = 1;
        long high = 1;
        Map<ClaferSymbol, long[]> global = new HashMap<>();
        Map<String, Integer> counts = new HashMap<>();
        boolean unique = true;
        for (ClaferSymbol c : clafers) {
            if (c.isAbstract()) {
                abstracts++;
            } else {
                concretes++;
            }
            if (c.reference().isPresent()) {
                references++;
            }
            long[] g = globalCard(c, global);
            low = Math.max(low, g[0]);
            high = high == -1 || g[1] == -1 ? -1 : Math.max(high, g[1]);
            counts.merge(c.name(), 1, Integer::sum);
            unique &= !c.name().contains("'");
        }
        unique &= counts.values().stream().allMatch(n -> n == 1);
        int[] constraints = {0};
        countConstraints(result.model().elements(), constraints);
        constraints[0] += result.model().assertions().size();
        return "All clafers: " + (abstracts + concretes) + " | Abstract: " + abstracts + " | Concrete: " + concretes
                + " | Reference: " + references + "\n"
                + "Constraints: " + constraints[0] + "\n"
                + "Goals: " + result.model().objectives().size() + "\n"
                + "Global scope: " + low + ".." + (high == -1 ? "*" : Long.toString(high)) + "\n"
                + "Can skip name resolver: " + (unique ? "yes" : "no") + "\n";
    }

    private static long[] globalCard(ClaferSymbol c, Map<ClaferSymbol, long[]> global) {
        long[] known = global.get(c);
        if (known != null) {
            return known;
        }
        Optional<ClaferSymbol> parent = c.parent();
        long[] p = parent.isPresent() ? globalCard(parent.get(), global) : new long[]{1, 1};
        Interval card = c.card();
        long m = card.low();
        long n = card.high() == Interval.Unbounded ? -1 : card.high();
        long[] g = {m * p[0], n == 0 || p[1] == 0 ? 0 : n == -1 || p[1] == -1 ? -1 : n * p[1]};
        global.put(c, g);
        return g;
    }

    private static void countConstraints(List<Ir.Element> elements, int[] count) {
        for (Ir.Element e : elements) {
            switch (e) {
                case Ir.Constraint c -> count[0]++;
                case Ir.ClaferDecl c -> {
                    if (c.initializer().isPresent()) {
                        count[0]++;
                    }
                    // the constraints of a reference to a set expression, which the compiler adds when it
                    // prints the model, are not counted
                    List<Ir.Element> own = c.ref().isEmpty() ? c.elements() : c.elements().stream()
                            .filter(x -> !(x instanceof Ir.Constraint k && k.span().equals(c.ref().get().span())))
                            .toList();
                    countConstraints(own, count);
                }
                case Ir.RootBlock r -> count[0] += r.constraints().size();
                case Ir.EnumDecl en -> {
                }
            }
        }
    }
}
