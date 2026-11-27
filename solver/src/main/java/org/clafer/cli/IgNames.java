package org.clafer.cli;

import java.io.File;
import java.math.BigDecimal;
import java.nio.file.Files;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import joptsimple.OptionSet;
import org.clafer.ast.AstAbstractClafer;
import org.clafer.ast.AstClafer;
import org.clafer.ast.AstConcreteClafer;
import org.clafer.ast.AstModel;
import org.clafer.ast.AstRef;
import org.clafer.ast.Card;
import org.clafer.frontend.ModelFiles;
import org.clafer.frontend.ToIr;
import org.clafer.ig.ClaferModel;
import org.clafer.ig.JsonGenerator;
import org.clafer.ig.Sugarer;
import org.clafer.instance.InstanceClafer;
import org.clafer.instance.InstanceModel;
import org.clafer.javascript.JavascriptFile;

/**
 * The instances of a static model named as claferIG names them
 * ({@link Sugarer}), for claferIG's {@code --useuids}, {@code --json} and
 * {@code --all}: each instance by its clafer's name, or by its UID with
 * {@code --useuids} (and {@code --json}), and its number among the instances
 * of that name. The clafers are known by their UIDs, which name them in the
 * Choco model.
 */
final class IgNames {

    private final Sugarer.Naming naming;
    private final Map<String, JsonGenerator.Info> infos;

    private IgNames(Sugarer.Naming naming, Map<String, JsonGenerator.Info> infos) {
        this.naming = naming;
        this.infos = infos;
    }

    /**
     * @return whether the options ask for claferIG's names
     */
    static boolean wanted(OptionSet options) {
        return options.has("useuids") || options.has("json") || options.has("all");
    }

    /**
     * The names of the model's instances, with the clafers as the IR has
     * them when the Java frontend reads the model, otherwise as the Choco
     * model has them.
     */
    static IgNames of(File file, JavascriptFile javascriptFile, OptionSet options) {
        Map<String, JsonGenerator.Info> infos = ir(file, options)
                .map(ir -> JsonGenerator.infos(ir.names().clafers()))
                .orElseGet(() -> fromAst(javascriptFile.getModel()));
        Map<String, Sugarer.ClaferInfo> clafers = new HashMap<>();
        infos.forEach((uid, i) -> clafers.put(uid, new Sugarer.ClaferInfo(i.ident(), i.superUid(),
                i.reference())));
        // --json names the instances by their UIDs, as claferIG's
        boolean useUids = options.has("useuids") || options.has("json");
        return new IgNames(new Sugarer.Naming(clafers, uid -> uid, useUids, options.has("addtypes"), Map.of()),
                infos);
    }

    /**
     * The model as the Java frontend reads it, if it reads it without errors.
     */
    static Optional<ToIr.Result> ir(File file, OptionSet options) {
        if (!file.getName().endsWith(".cfr") || options.has("haskell-frontend")) {
            return Optional.empty();
        }
        try {
            if (Utils.isExplicitModel(file, options) || Files.readString(file.toPath()).contains("[choco|")) {
                return Optional.empty();
            }
            List<String> imports = new ArrayList<>();
            for (Object i : options.valuesOf("import")) {
                imports.add(i.toString());
            }
            ToIr.Result ir = ToIr.toIr(ModelFiles.read(file.getPath(), imports, p -> Optional.empty(),
                    Frontend.restrictions(options)));
            return ir.messages().isEmpty() ? Optional.of(ir) : Optional.empty();
        } catch (Exception e) {
            return Optional.empty();
        }
    }

    // the clafers of a model that the Java frontend does not read (an explicit or .js model, or one
    // that the Haskell compiler compiles): the name is the UID without the compiler's prefix
    private static Map<String, JsonGenerator.Info> fromAst(AstModel model) {
        Map<String, JsonGenerator.Info> out = new HashMap<>();
        for (AstAbstractClafer a : model.getAbstracts()) {
            add(a, out);
        }
        for (AstConcreteClafer c : model.getChildren()) {
            add(c, out);
        }
        return out;
    }

    private static void add(AstClafer c, Map<String, JsonGenerator.Info> out) {
        if (c.getName().startsWith("#")) {
            c.getChildren().forEach(x -> add(x, out));
            return;
        }
        String uid = c.getName();
        String ident = uid.contains("__") ? uid.substring(uid.lastIndexOf("__") + 2)
                : uid.replaceFirst("^c[0-9]+_", "");
        String superUid = c.hasSuperClafer() && !c.getSuperClafer().getName().startsWith("#")
                ? c.getSuperClafer().getName() : null;
        String reference = null;
        if (c.hasRef()) {
            AstRef ref = c.getRef();
            String target = ref.getTargetType().getName();
            reference = target.equals("int") ? "integer" : target;
        }
        int low = 1;
        int high = 1;
        if (c instanceof AstConcreteClafer cc) {
            Card card = cc.getCard();
            low = card.getLow();
            high = card.hasHigh() ? card.getHigh() : -1;
        } else {
            low = 0;
            high = -1;
        }
        out.put(uid, new JsonGenerator.Info(ident, superUid, reference, low, high));
        for (AstConcreteClafer child : c.getChildren()) {
            add(child, out);
        }
    }

    /**
     * @return what the JSON has of a clafer, by its UID
     */
    JsonGenerator.Info info(String uid) {
        return infos.get(uid);
    }

    /**
     * The instance with claferIG's names.
     */
    ClaferModel name(InstanceModel instance) {
        List<ClaferModel.Clafer> top = new ArrayList<>();
        for (InstanceClafer c : instance.getTopClafers()) {
            top.add(raw(c));
        }
        return Sugarer.sugarInstance(naming, new ClaferModel(top));
    }

    // each instance by the UID of its clafer and its number among the instances of that clafer
    private static ClaferModel.Id id(InstanceClafer c) {
        return new ClaferModel.Id(c.getType().getName(), c.getId());
    }

    private static ClaferModel.Clafer raw(InstanceClafer c) {
        ClaferModel.Value value = null;
        if (c.hasRef()) {
            Object ref = c.getRef();
            value = switch (ref) {
                case InstanceClafer r -> new ClaferModel.AliasValue(id(r));
                case Integer i -> new ClaferModel.IntValue(i);
                case BigDecimal d -> new ClaferModel.StringValue(d.toPlainString());
                default -> new ClaferModel.StringValue(ref.toString());
            };
        }
        List<ClaferModel.Clafer> children = new ArrayList<>();
        for (InstanceClafer child : c.getChildren()) {
            children.add(raw(child));
        }
        return new ClaferModel.Clafer(id(c), c.getType().getName(), value, children);
    }
}
