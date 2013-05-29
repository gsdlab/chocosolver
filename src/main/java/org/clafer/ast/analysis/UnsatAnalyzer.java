package org.clafer.ast.analysis;

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
        Map<AstClafer, AstConstraint[]> constraintsMap = analysis.getConstraintsMap();
        Map<AstClafer, AstConstraint[]> softConstraintsMap = analysis.getConstraintsMap();
        for (Entry<AstClafer, AstConstraint[]> entry : constraintsMap.entrySet()) {
            AstClafer clafer = entry.getKey();
            AstConstraint[] constraints = entry.getValue();
            AstConstraint[] softConstraints = new AstConstraint[constraints.length];
            for (int i = 0; i < constraints.length; i++) {
                softConstraints[i] = constraints[i].asSoft();
            }
            softConstraintsMap.put(clafer, softConstraints);
        }
        return analysis.withConstraintsMap(constraintsMap);
    }
}
