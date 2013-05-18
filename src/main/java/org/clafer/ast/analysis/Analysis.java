package org.clafer.ast.analysis;

import java.util.Map;
import org.clafer.common.Check;
import org.clafer.ast.scope.Scope;
import org.clafer.ast.analysis.AbstractOffsetAnalysis.Offsets;
import org.clafer.ast.analysis.FormatAnalysis.Format;
import org.clafer.ast.analysis.PartialSolutionAnalysis.PartialSolution;
import org.clafer.ast.AstAbstractClafer;
import org.clafer.ast.AstClafer;
import org.clafer.ast.AstExpr;
import org.clafer.ast.AstModel;
import org.clafer.ast.AstRef;
import org.clafer.ast.Card;

/**
 * Analyzes and optimizes the AST.
 * 
 * @author jimmy
 */
public class Analysis {

    private final Map<AstAbstractClafer, Integer> depths;
    private final Map<AstClafer, Card> globalCards;
    private final Scope scope;
    private final Map<AstClafer, Format> formats;
    private final Map<AstAbstractClafer, Offsets> offsets;
    private final Map<AstClafer, PartialSolution> partialSolutions;
    private final Map<AstRef, int[][]> partialInts;
    private final Map<AstExpr, AstClafer> types;

    private Analysis(Map<AstAbstractClafer, Integer> depths,
            Map<AstClafer, Card> globalCards,
            Scope scope,
            Map<AstClafer, Format> formats,
            Map<AstAbstractClafer, Offsets> offsets,
            Map<AstClafer, PartialSolution> partialSolutions,
            Map<AstRef, int[][]> partialInts,
            Map<AstExpr, AstClafer> types) {
        this.depths = depths;
        this.globalCards = globalCards;
        this.scope = scope;
        this.formats = formats;
        this.offsets = offsets;
        this.partialSolutions = partialSolutions;
        this.partialInts = partialInts;
        this.types = types;
    }

    public static Analysis analyze(AstModel model, Scope scope) {
        Check.notNull(model);
        Check.notNull(scope);
        Map<AstExpr, AstClafer> types = TypeAnalysis.analyze(model);
        CanonicalAnalysis.analyze(model, types);

        Map<AstAbstractClafer, Integer> depths = TypeHierarchyDepthAnalysis.analyze(model);

        Map<AstClafer, Card> globalCards = GlobalCardAnalysis.analyze(model, scope);
        Scope optimizedScope = ScopeAnalysis.analyze(model, scope, globalCards);

        CardAnalysis.analyze(model, globalCards);
        Map<AstClafer, Format> formats = FormatAnalysis.analyze(model, optimizedScope);

        Map<AstAbstractClafer, Offsets> offsets = AbstractOffsetAnalysis.analyze(model, globalCards);
        Map<AstClafer, PartialSolution> partialSolutions = PartialSolutionAnalysis.analyze(model, globalCards, formats, offsets);

        // Reanalyze types.
        types = TypeAnalysis.analyze(model);
        Map<AstRef, int[][]> partialInts = PartialIntAnalysis.analyze(model, partialSolutions, offsets, optimizedScope);

        return new Analysis(depths, globalCards, optimizedScope, formats, offsets, partialSolutions, partialInts, types);
    }

    public int getDepth(AstAbstractClafer clafer) {
        return AnalysisUtil.notNull("Cannot find depth analysis for " + clafer, depths.get(clafer)).intValue();
    }

    public Card getGlobalCard(AstClafer clafer) {
        return AnalysisUtil.notNull("Cannot find global card analysis for " + clafer, globalCards.get(clafer));
    }

    public Scope getScope() {
        return scope;
    }

    public Format getFormat(AstClafer clafer) {
        return AnalysisUtil.notNull("Cannot find format analysis for " + clafer, formats.get(clafer));
    }

    public int getOffset(AstAbstractClafer sup, AstClafer sub) {
        return AnalysisUtil.notNull("Cannot find offset analysis for " + sup, offsets.get(sup)).getOffset(sub);
    }

    public PartialSolution getPartialSolution(AstClafer clafer) {
        return AnalysisUtil.notNull("Cannot find partial solution analysis for " + clafer, partialSolutions.get(clafer));
    }

    public int[][] getPartialInts(AstRef ref) {
        return partialInts.get(ref);
    }

    public AstClafer getType(AstExpr expr) {
        return AnalysisUtil.notNull("Cannot find partial solution analysis for " + expr, types.get(expr));
    }
}
