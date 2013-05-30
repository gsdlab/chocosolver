package org.clafer.ast.analysis;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import org.clafer.ast.AstClafer;
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
        List<AstConstraint> softConstraints = new ArrayList<AstConstraint>(constraints.size());
        for (AstConstraint constraint : constraints) {
            softConstraints.add(constraint.asSoft());
        }
        return analysis.withConstraints(softConstraints);
    }
}
