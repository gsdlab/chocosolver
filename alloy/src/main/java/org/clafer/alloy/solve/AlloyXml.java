package org.clafer.alloy.solve;

import java.io.IOException;
import java.io.StringReader;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;
import java.util.function.UnaryOperator;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import org.clafer.ig.ClaferModel;
import org.clafer.ig.ClaferModel.AliasValue;
import org.clafer.ig.ClaferModel.Clafer;
import org.clafer.ig.ClaferModel.Id;
import org.clafer.ig.ClaferModel.IntValue;
import org.clafer.ig.ClaferModel.StringValue;
import org.clafer.ig.ClaferModel.Value;
import org.clafer.ig.Solution;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.xml.sax.InputSource;
import org.xml.sax.SAXException;

/**
 * A solution from Alloy's XML, as claferIG reads it: {@code Solution.hs}
 * (the sigs and fields of each state), {@code ClaferModel.hs} (the tree of
 * the clafers) and {@code Sugarer.hs} (the names of the instances).
 */
public final class AlloyXml {

    /**
     * What the names of the instances need of a clafer, by its Alloy name.
     *
     * @param ident its name
     * @param superName the Alloy name of its super clafer, or null
     * @param reference the primitive type of its own reference, e.g.,
     * {@code string}, or null
     */
    public record ClaferInfo(String ident, String superName, String reference) {
    }

    /**
     * How the instances are named.
     *
     * @param clafers the clafers by their Alloy names
     * @param toUid the UID of a clafer by its Alloy name
     * @param useUids the instances are named by the UIDs of their clafers
     * @param addTypes each instance has the super clafer of its clafer
     * @param strings the strings by their integers
     */
    public record Naming(Map<String, ClaferInfo> clafers, UnaryOperator<String> toUid, boolean useUids,
            boolean addTypes, Map<Integer, String> strings) {
    }

    private AlloyXml() {
    }

    // ----------------------------------------------------------------------
    // Solution.hs
    // ----------------------------------------------------------------------
    private record Sig(String label, int id, List<String> atoms) {
    }

    private record Field(String label, int toType, List<String[]> tuples) {
    }

    private record Instance(List<Sig> sigs, List<Field> fields) {
    }

    private static Element parse(String xml) {
        try {
            DocumentBuilderFactory f = DocumentBuilderFactory.newInstance();
            f.setExpandEntityReferences(false);
            f.setFeature("http://apache.org/xml/features/disallow-doctype-decl", true);
            return f.newDocumentBuilder().parse(new InputSource(new StringReader(xml))).getDocumentElement();
        } catch (ParserConfigurationException | SAXException | IOException e) {
            throw new IllegalArgumentException("Not Alloy's XML of a solution: " + e.getMessage(), e);
        }
    }

    private static List<Element> children(Element e, String tag) {
        List<Element> out = new ArrayList<>();
        for (Node n = e.getFirstChild(); n != null; n = n.getNextSibling()) {
            if (n instanceof Element c && c.getTagName().equals(tag)) {
                out.add(c);
            }
        }
        return out;
    }

    private static Instance parseInstance(Element instance) {
        List<Sig> sigs = new ArrayList<>();
        for (Element s : children(instance, "sig")) {
            List<String> atoms = children(s, "atom").stream().map(a -> a.getAttribute("label")).toList();
            sigs.add(new Sig(s.getAttribute("label"), Integer.parseInt(s.getAttribute("ID")), atoms));
        }
        List<Field> fields = new ArrayList<>();
        for (Element f : children(instance, "field")) {
            List<Element> types = children(f, "types").stream().flatMap(t -> children(t, "type").stream()).toList();
            int toType = Integer.parseInt(types.get(1).getAttribute("ID"));
            List<String[]> tuples = new ArrayList<>();
            for (Element t : children(f, "tuple")) {
                List<Element> atoms = children(t, "atom");
                tuples.add(new String[]{atoms.get(0).getAttribute("label"), atoms.get(1).getAttribute("label")});
            }
            fields.add(new Field(f.getAttribute("label"), toType, tuples));
        }
        return new Instance(sigs, fields);
    }

    // ----------------------------------------------------------------------
    // ClaferModel.hs
    // ----------------------------------------------------------------------
    private sealed interface TreeNode {

        Id id();
    }

    private record ClaferNode(Id id, int type) implements TreeNode {
    }

    private record ValueNode(Id id, int type) implements TreeNode {
    }

    private static final class FamilyTree {

        final TreeMap<Id, TreeNode> roots = new TreeMap<>();
        final Map<Id, List<TreeNode>> descendants = new HashMap<>();

        List<TreeNode> children(Id parent) {
            return descendants.getOrDefault(parent, List.of());
        }
    }

    private static Id labelAsId(String label) {
        int i = label.indexOf('$');
        return i < 0 ? new Id(label, 0) : new Id(label.substring(0, i), Integer.parseInt(label.substring(i + 1)));
    }

    private static FamilyTree buildFamilyTree(Instance solution, List<Field> fields) {
        FamilyTree tree = new FamilyTree();
        for (Sig s : solution.sigs()) {
            for (String a : s.atoms()) {
                Id id = labelAsId(a);
                tree.roots.put(id, new ClaferNode(id, s.id()));
            }
        }
        // the fields and their tuples in order: foldr of the prepending addChild
        for (Field f : fields) {
            boolean value = f.label().endsWith("_ref");
            for (String[] t : f.tuples()) {
                Id from = labelAsId(t[0]);
                Id to = labelAsId(t[1]);
                TreeNode child = value ? new ValueNode(to, f.toType()) : new ClaferNode(to, f.toType());
                if (child instanceof ClaferNode) {
                    tree.roots.remove(to);
                }
                tree.descendants.computeIfAbsent(from, k -> new ArrayList<>()).add(child);
            }
        }
        return tree;
    }

    /**
     * The instance in a state of a trace of a dynamic model. The sigs of a
     * dynamic model are pools of atoms, which exist in every state; the clafers
     * that exist in the state are those reachable from the root through their
     * containment relations ({@code r_c0_name}). The parent relations
     * ({@code p_c0_name}) of the atoms are static, and do not belong to the
     * tree.
     */
    private static ClaferModel buildStateModel(Instance solution) {
        List<Field> fields = solution.fields().stream()
                .filter(f -> f.label().startsWith("r_") || f.label().endsWith("_ref")).toList();
        FamilyTree tree = buildFamilyTree(solution, fields);
        return buildClafers(solution, tree, tree.children(new Id("root", 0)));
    }

    private static ClaferModel buildClaferModel(Instance solution) {
        FamilyTree tree = buildFamilyTree(solution, solution.fields());
        return buildClafers(solution, tree, new ArrayList<>(tree.roots.values()));
    }

    private static ClaferModel buildClafers(Instance solution, FamilyTree tree, List<TreeNode> nodes) {
        int intType = solution.sigs().stream().filter(s -> s.label().equals("Int")).findFirst()
                .orElseThrow(() -> new IllegalStateException("Missing Int sig")).id();
        List<Clafer> clafers = new ArrayList<>();
        for (TreeNode n : nodes) {
            if (n instanceof ClaferNode c) {
                clafers.add(buildClafer(c, tree, intType));
            }
        }
        return new ClaferModel(removeDups(clafers));
    }

    private static Clafer buildClafer(ClaferNode node, FamilyTree tree, int intType) {
        List<Clafer> claferChildren = new ArrayList<>();
        LinkedHashSet<Value> values = new LinkedHashSet<>();
        for (TreeNode child : tree.children(node.id())) {
            switch (child) {
                case ClaferNode c -> claferChildren.add(buildClafer(c, tree, intType));
                case ValueNode v -> values.add(v.type() == intType
                        ? new IntValue(Integer.parseInt(v.id().name())) : new AliasValue(v.id()));
            }
        }
        if (values.size() > 1) {
            throw new IllegalStateException("Received more than one value " + values);
        }
        return new Clafer(node.id(), node.id().name(), values.isEmpty() ? null : values.iterator().next(),
                claferChildren);
    }

    // removeDups: a clafer that appears again later is left out here; the result is reversed, as the
    // compiler's accumulator gives it
    private static List<Clafer> removeDups(List<Clafer> ms) {
        List<Clafer> acc = new ArrayList<>();
        for (int i = 0; i < ms.size(); i++) {
            Clafer m = ms.get(i);
            if (ms.subList(i + 1, ms.size()).contains(m)) {
                continue;
            }
            acc.add(0, new Clafer(m.id(), m.uid(), m.value(), removeDups(m.children())));
        }
        return acc;
    }

    // ----------------------------------------------------------------------
    // Sugarer.hs
    // ----------------------------------------------------------------------
    /**
     * The simple name of each instance and its number among the instances of
     * that name, and how many instances each name has.
     */
    private static final class Census {

        final Map<Id, Object[]> sample = new HashMap<>();
        final Map<String, Integer> counts = new HashMap<>();

        void poll(Naming naming, Id id) {
            String name = claferName(naming, id.name());
            int ordinal = counts.merge(name, 1, Integer::sum);
            sample.put(id, new Object[]{ordinal, name});
        }
    }

    private static String claferName(Naming naming, String alloyName) {
        ClaferInfo info = naming.clafers().get(alloyName);
        return info == null ? alloyName : info.ident();
    }

    /**
     * Count the number of each clafer in the states of a trace: an instance
     * that exists in several states is counted once, so it has the same name
     * in each of them.
     */
    private static Census statesCensus(Naming naming, List<ClaferModel> states) {
        Census census = new Census();
        for (ClaferModel state : states) {
            state.topLevel().forEach(c -> census(naming, c, census));
        }
        return census;
    }

    private static void census(Naming naming, Clafer c, Census census) {
        c.children().forEach(x -> census(naming, x, census));
        if (!census.sample.containsKey(c.id())) {
            census.poll(naming, c.id());
        }
    }

    private static ClaferModel sugar(Census census, Naming naming, ClaferModel model) {
        return new ClaferModel(model.topLevel().stream().map(c -> sugarClafer(census, naming, c)).toList());
    }

    private static Clafer sugarClafer(Census census, Naming naming, Clafer c) {
        return new Clafer(sugarId(census, naming, true, c.id()), c.uid(), sugarValue(census, naming, c),
                c.children().stream().map(x -> sugarClafer(census, naming, x)).toList());
    }

    private static Value sugarValue(Census census, Naming naming, Clafer c) {
        if (c.value() instanceof AliasValue a) {
            return new AliasValue(sugarId(census, naming, false, a.alias()));
        }
        if (c.value() == null) {
            return null;
        }
        ClaferInfo info = naming.clafers().get(c.id().name());
        if (info == null) {
            throw new IllegalStateException("IClafer not found:" + c.id().name());
        }
        if ("string".equals(info.reference()) && c.value() instanceof IntValue i) {
            String s = naming.strings().get(i.value());
            return new StringValue(s == null ? "\"<text " + i.value() + ">\"" : s);
        }
        return c.value();
    }

    private static Id sugarId(Census census, Naming naming, boolean addRefDecl, Id id) {
        Object[] sample = census.sample.get(id);
        if (sample == null) {
            // an atom that is in no state of the trace, e.g., the literal of an enumeration
            return new Id(naming.useUids() ? naming.toUid().apply(id.name()) : claferName(naming, id.name()), 0);
        }
        int ordinal = (Integer) sample[0];
        String simpleName = (String) sample[1];
        int count = census.counts.get(simpleName);
        String ordinalDisplay = naming.useUids() || count > 1 ? "$" + ordinal : "";
        String finalName = naming.useUids() ? naming.toUid().apply(id.name()) : simpleName;
        String refDecl = "";
        if (naming.addTypes() && addRefDecl) {
            ClaferInfo info = naming.clafers().get(id.name());
            if (info == null) {
                throw new IllegalStateException("IClafer not found: " + id.name());
            }
            if (info.superName() != null) {
                refDecl = " : " + (naming.useUids() ? naming.toUid().apply(info.superName())
                        : claferName(naming, info.superName()));
            }
        }
        return new Id(finalName + ordinalDisplay + refDecl, 0);
    }

    // ----------------------------------------------------------------------
    // ClaferIG.xmlToSolution
    // ----------------------------------------------------------------------
    /**
     * The instance of a static model, or the trace of a dynamic model.
     */
    public static Solution toSolution(String xml, boolean dynamic, Naming naming) {
        Element alloy = parse(xml);
        List<Element> instances = children(alloy, "instance");
        if (!dynamic) {
            ClaferModel model = buildClaferModel(parseInstance(instances.get(0)));
            Census census = new Census();
            model.topLevel().forEach(c -> census(naming, c, census));
            return new Solution.Instance(sugar(census, naming, model));
        }
        // the state to which its last state loops back: Alloy gives the length of the trace and of its loop
        int loopState = 0;
        if (!instances.isEmpty() && instances.get(0).hasAttribute("tracelength")
                && instances.get(0).hasAttribute("looplength")) {
            loopState = Integer.parseInt(instances.get(0).getAttribute("tracelength"))
                    - Integer.parseInt(instances.get(0).getAttribute("looplength"));
        }
        List<ClaferModel> states = instances.stream().map(i -> buildStateModel(parseInstance(i))).toList();
        Census census = statesCensus(naming, states);
        return new Solution.Trace(states.stream().map(s -> sugar(census, naming, s)).toList(), loopState);
    }
}
