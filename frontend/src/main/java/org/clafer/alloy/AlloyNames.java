package org.clafer.alloy;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

/**
 * The names of the clafers in the Alloy output: a port of the compiler's
 * {@code Generator/AlloyNames.hs}. A clafer's Alloy name is its UID, unless
 * the UID cannot be one: an Alloy keyword, a function of {@code util/integer},
 * or a name that the generator writes, or one whose derived names
 * ({@code r_u}, {@code u_ref}, {@code alive_u}, {@code p_u}) another clafer
 * uses. Such a clafer gets {@code u_1}, {@code u_2}, ..., the first that is
 * free.
 */
final class AlloyNames {

    private AlloyNames() {
    }

    /**
     * {@code alloyNames}.
     *
     * @param uids the UIDs of the clafers, in the order of the model
     * @return the Alloy names of the clafers that are renamed, by UID
     */
    static Map<String, String> alloyNames(List<String> uids) {
        Set<String> allUids = new HashSet<>(uids);
        Set<String> used = new HashSet<>();
        List<String> toRename = new ArrayList<>();
        for (String u : uids) {
            if (alloyReserved.contains(u) || looksGenerated(u) || derived(u).stream().anyMatch(used::contains)) {
                toRename.add(u);
            } else {
                used.addAll(derived(u));
            }
        }
        Map<String, String> renamed = new LinkedHashMap<>();
        for (String u : toRename) {
            String name = null;
            for (int n = 1; name == null; n++) {
                String candidate = u + "_" + n;
                if (!allUids.contains(candidate) && derived(candidate).stream().noneMatch(used::contains)) {
                    name = candidate;
                }
            }
            used.addAll(derived(name));
            renamed.put(u, name);
        }
        return renamed;
    }

    static List<String> derived(String u) {
        return List.of(u, "r_" + u, u + "_ref", "alive_" + u, "p_" + u);
    }

    /**
     * A name that the generator writes for an assertion, e.g.,
     * {@code assertAt_3_5}.
     */
    static boolean looksGenerated(String n) {
        if (!n.startsWith("assertAt_")) {
            return false;
        }
        String rest = n.substring("assertAt_".length());
        int u = rest.indexOf('_');
        if (u < 0) {
            return false;
        }
        String line = rest.substring(0, u);
        String column = rest.substring(u + 1);
        return !line.isEmpty() && line.chars().allMatch(Character::isDigit)
                && !column.isEmpty() && column.chars().allMatch(Character::isDigit);
    }

    static final Set<String> alloyReserved = Set.of(
            "abstract", "all", "and", "as", "assert", "but", "check", "disj", "else", "enum", "exactly",
            "expect", "extends", "fact", "for", "fun", "iden", "iff", "implies", "in", "Int", "int", "let",
            "lone", "module", "no", "none", "not", "one", "open", "or", "pred", "private", "run", "seq",
            "set", "sig", "some", "String", "sum", "this", "univ", "var", "steps", "always", "eventually",
            "after", "before", "historically", "once", "since", "until", "releases", "triggered",
            "add", "plus", "sub", "minus", "mul", "div", "rem", "negate", "eq", "gt", "lt", "gte", "lte",
            "zero", "pos", "neg", "nonpos", "nonneg", "signum", "int2elem", "elem2int", "max", "min",
            "next", "prev", "nexts", "prevs", "larger", "smaller",
            "show", "ref", "self_", "children", "integer", "util");
}
