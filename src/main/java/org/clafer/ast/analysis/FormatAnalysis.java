package org.clafer.ast.analysis;

import java.util.HashMap;
import java.util.Map;
import org.clafer.ast.scope.Scope;
import org.clafer.ast.AstAbstractClafer;
import org.clafer.ast.AstClafer;
import org.clafer.ast.AstConcreteClafer;
import org.clafer.ast.AstModel;

/**
 *
 * @author jimmy
 */
public class FormatAnalysis {

    private FormatAnalysis() {
    }

    public static Map<AstClafer, Format> analyze(AstModel model, Scope scope) {
        Map<AstClafer, Format> formats = new HashMap<AstClafer, Format>();
        formats.put(model, Format.LowGroup);
        for (AstAbstractClafer abstractClafer : model.getAbstractClafers()) {
            analyze(abstractClafer, scope, formats);
        }
        for (AstConcreteClafer child : model.getChildren()) {
            analyze(child, scope, formats);
        }
        return formats;
    }

    private static void analyze(AstAbstractClafer clafer, Scope scope, Map<AstClafer, Format> formats) {
        formats.put(clafer, Format.ParentGroup);
        for (AstConcreteClafer child : clafer.getChildren()) {
            analyze(child, scope, formats);
        }
    }

    private static void analyze(AstConcreteClafer clafer, Scope scope, Map<AstClafer, Format> formats) {
        formats.put(clafer,
                clafer.getCard().isExact()
                && scope.getScope(clafer) >= clafer.getCard().getHigh() * scope.getScope(clafer.getParent())
                ? Format.ParentGroup : Format.LowGroup);
        for (AstConcreteClafer child : clafer.getChildren()) {
            analyze(child, scope, formats);
        }
    }
}
