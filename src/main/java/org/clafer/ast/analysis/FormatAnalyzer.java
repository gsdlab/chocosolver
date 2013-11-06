package org.clafer.ast.analysis;

import java.util.HashMap;
import java.util.Map;
import org.clafer.ast.AstAbstractClafer;
import org.clafer.ast.AstClafer;
import org.clafer.ast.AstConcreteClafer;

/**
 *
 * @author jimmy
 */
public class FormatAnalyzer implements Analyzer {

    @Override
    public Analysis analyze(Analysis analysis) {
        Map<AstClafer, Format> formatMap = new HashMap<>();
        formatMap.put(analysis.getModel(), Format.LowGroup);
        for (AstAbstractClafer abstractClafer : analysis.getAbstractClafers()) {
            analyze(abstractClafer, analysis, formatMap);
        }
        for (AstConcreteClafer child : analysis.getModel().getChildren()) {
            analyze(child, analysis, formatMap);
        }
        return analysis.setFormatMap(formatMap);
    }

    private static void analyze(AstAbstractClafer clafer, Analysis analysis, Map<AstClafer, Format> formatMap) {
        formatMap.put(clafer, Format.ParentGroup);
        for (AstConcreteClafer child : clafer.getChildren()) {
            analyze(child, analysis, formatMap);
        }
    }

    private static void analyze(AstConcreteClafer clafer, Analysis analysis, Map<AstClafer, Format> formatMap) {
        formatMap.put(clafer,
                analysis.getCard(clafer).isExact()
                && analysis.getScope(clafer) >= analysis.getCard(clafer).getHigh() * analysis.getScope(clafer.getParent())
                ? Format.ParentGroup : Format.LowGroup);
        for (AstConcreteClafer child : clafer.getChildren()) {
            analyze(child, analysis, formatMap);
        }
    }
}
