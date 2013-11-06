package org.clafer.ast.analysis;

import java.util.ArrayList;
import java.util.List;
import org.clafer.ast.AstConstraint;

/**
 * Sets every constraint to soft.
 *
 * @author jimmy
 */
public class UnsatAnalyzer implements Analyzer {

    @Override
    public Analysis analyze(Analysis analysis) {
        List<AstConstraint> constraints = analysis.getConstraints();
        List<AstConstraint> softConstraints = new ArrayList<>(constraints.size());
        for (AstConstraint constraint : constraints) {
            softConstraints.add(constraint.asSoft());
        }
        return analysis.setConstraints(softConstraints);
    }
}
