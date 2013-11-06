package org.clafer.ast.analysis;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import org.clafer.ast.AstAbstractClafer;
import org.clafer.ast.AstClafer;
import org.clafer.ast.AstConcreteClafer;
import org.clafer.ast.AstConstraint;
import org.clafer.ast.AstSetExpr;
import org.clafer.ast.AstUtil;
import static org.clafer.ast.Asts.$this;
import static org.clafer.ast.Asts.joinParent;
import static org.clafer.ast.Asts.notEqual;

/**
 * Add additional constraints for to handle cases where a descendent inherits an
 * ancestor.
 *
 * <pre>
 * abstract Path
 *     p : Path ?
 * </pre>
 *
 * Will be transformed to
 *
 * <pre>
 * abstract Path
 *     p : Path ?
 *         [this.parent != this]
 * </pre>
 *
 * This forbids {@code p} from being its own parent.
 *
 * @author jimmy
 */
public class CircularityAnalyzer implements Analyzer {

    @Override
    public Analysis analyze(Analysis analysis) {
        List<AstConstraint> constraints = new ArrayList<>(0);
        for (AstConcreteClafer clafer : analysis.getConcreteClafers()) {
            Set<AstAbstractClafer> supers = new HashSet<>(AstUtil.getSupers(clafer));
            // supers contains at least #clafer# which has no children.
            if (supers.size() > 1) {
                int depth = 1;
                AstConcreteClafer cur = clafer;
                while (cur.hasParent()) {
                    AstClafer parent = cur.getParent();
                    if (parent instanceof AstAbstractClafer) {
                        if (supers.contains((AstAbstractClafer) parent)) {
                            AstSetExpr join = $this();
                            for (int i = 0; i < depth; i++) {
                                join = joinParent(join);
                            }
                            constraints.add(new AstConstraint(clafer, notEqual(join, $this())));
                        }
                        break;
                    } else {
                        cur = ((AstConcreteClafer) parent);
                    }
                }
            }
        }
        if (constraints.isEmpty()) {
            return analysis;
        }
        constraints.addAll(analysis.getConstraints());
        return analysis.setConstraints(constraints);
    }
}
