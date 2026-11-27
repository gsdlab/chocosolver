package org.clafer.alloy.solve;

import java.io.IOException;
import java.io.StringReader;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import org.clafer.ig.ClaferModel;
import org.clafer.ig.ClaferModel.AliasValue;
import org.clafer.ig.ClaferModel.Clafer;
import org.clafer.ig.ClaferModel.Id;
import org.clafer.ig.ClaferModel.IntValue;
import org.clafer.ig.ClaferModel.Value;
import org.clafer.ig.Solution;
import org.clafer.ig.Sugarer;
import org.clafer.ig.Sugarer.Naming;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.xml.sax.InputSource;
import org.xml.sax.SAXException;

/**
 * A solution from Alloy's XML, as claferIG reads it: {@code Solution.hs}
 * (the sigs and fields of each state), {@code ClaferModel.hs} (the tree of
 * the clafers), named by {@link Sugarer}.
 */
public final class AlloyXml {

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
            return new Solution.Instance(Sugarer.sugarInstance(naming, model));
        }
        // the state to which its last state loops back: Alloy gives the length of the trace and of its loop
        int loopState = 0;
        if (!instances.isEmpty() && instances.get(0).hasAttribute("tracelength")
                && instances.get(0).hasAttribute("looplength")) {
            loopState = Integer.parseInt(instances.get(0).getAttribute("tracelength"))
                    - Integer.parseInt(instances.get(0).getAttribute("looplength"));
        }
        List<ClaferModel> states = instances.stream().map(i -> buildStateModel(parseInstance(i))).toList();
        return new Solution.Trace(Sugarer.sugarStates(naming, states), loopState);
    }
}
