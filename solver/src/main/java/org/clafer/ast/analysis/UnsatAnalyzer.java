package org.clafer.ast.analysis;

import java.util.Collections;
import org.clafer.ast.AstConstraint;

/**
 * Sets every constraint to soft.
 *
 * @author jimmy
 */
public class UnsatAnalyzer implements Analyzer {

    @Override
    public Analysis analyze(Analysis analysis) {
        return analysis.setHardConstraints(Collections.<AstConstraint>emptySet());
    }
}
