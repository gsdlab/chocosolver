package org.clafer.generator;

import org.clafer.Check;
import org.clafer.analysis.Analysis;
import org.clafer.ast.AstAbstractClafer;
import org.clafer.ast.AstClafer;
import org.clafer.collection.Pair;

/**
 *
 * @author jimmy
 */
class OffsetSolutionMap implements SolutionMap<Pair<AstAbstractClafer, AstClafer>, Integer> {

    private final Analysis analysis;

    OffsetSolutionMap(Analysis analysis) {
        this.analysis = Check.notNull(analysis);
    }

    @Override
    public boolean has(Pair<AstAbstractClafer, AstClafer> a) {
        return true;
    }

    @Override
    public Integer get(Pair<AstAbstractClafer, AstClafer> a) {
        return analysis.getOffset(a.getFst(), a.getSnd());
    }
}
