package org.clafer.ast.analysis;

import java.util.ArrayList;
import java.util.List;
import org.clafer.ast.AstClafer;
import org.clafer.ast.AstConcreteClafer;
import org.clafer.ast.AstConstant;
import org.clafer.ast.AstConstraint;
import org.clafer.ast.AstExpr;
import org.clafer.ast.AstExprRewriter;
import org.clafer.ast.AstGlobal;
import org.clafer.ast.AstJoin;
import org.clafer.ast.AstJoinParent;
import org.clafer.ast.AstSetExpr;
import org.clafer.ast.AstThis;
import static org.clafer.ast.Asts.*;
import org.clafer.ast.Card;
import org.clafer.common.Util;

/**
 * Optimizes the expressions inside the constraints. Assumes type checking has
 * already passed.
 *
 * @author jimmy
 */
public class OptimizerAnalyzer extends AstExprRewriter<Analysis> implements Analyzer {

    @Override
    public Analysis analyze(Analysis analysis) {
        List<AstConstraint> optimizedConstraint = new ArrayList<>();
        for (AstConstraint constraint : analysis.getConstraints()) {
            optimizedConstraint.add(constraint.withExpr(rewrite(constraint.getExpr(), analysis)));
        }
        return analysis.setConstraints(optimizedConstraint);
    }

    @Override
    public AstExpr visit(AstJoin ast, Analysis a) {
        AstSetExpr left = rewrite(ast.getLeft(), a);
        if (left instanceof AstThis) {
            if (a.getScope(a.getCommonSupertype(ast.getLeft())) == 1) {
                Card childCard = a.getCard(ast.getRight());
                if (childCard.isExact()) {
                    return constant(ast.getRight(), Util.fromTo(0, childCard.getLow()));
                }
                return global(ast.getRight());
            }
        } else if (left instanceof AstGlobal) {
            return global(ast.getRight());
        } else if (left instanceof AstConstant) {
            Card childCard = a.getCard(ast.getRight());
            if (Format.ParentGroup.equals(a.getFormat(ast.getRight()))) {
                AstConstant constant = (AstConstant) left;
                assert childCard.isExact();
                int[] childConstant = new int[constant.getValue().length * childCard.getLow()];
                for (int i = 0; i < constant.getValue().length; i++) {
                    for (int j = 0; j < childCard.getLow(); j++) {
                        childConstant[i * childCard.getLow() + j] =
                                i * constant.getValue()[i] + j;
                    }
                }
                return constant(ast.getRight(), childConstant);
            }
            assert childCard.getLow() != a.getScope(ast.getRight()) :
                    "Didn't run scope analysis before format analysis?";
        }
        return join(left, ast.getRight());
    }

    @Override
    public AstExpr visit(AstJoinParent ast, Analysis a) {
        AstSetExpr children = rewrite(ast.getChildren(), a);
        if (children instanceof AstThis) {
            AstClafer type = a.getCommonSupertype(ast);
            if (a.getScope(type) == 1) {
                return constant(type, 0);
            }
        } else if (children instanceof AstGlobal) {
            AstClafer childType = a.getCommonSupertype(ast.getChildren());
            if (childType instanceof AstConcreteClafer) {
                AstConcreteClafer concreteChildType = (AstConcreteClafer) childType;
                if (a.getCard(concreteChildType).hasLow()) {
                    return global(a.getCommonSupertype(ast));
                }
            }
        } else if (children instanceof AstConstant) {
            AstConstant constant = (AstConstant) children;
            AstClafer type = a.getCommonSupertype(ast);
            if (constant.getValue().length > 0 && a.getScope(type) == 1) {
                return constant(type, 0);
            }
        }
        return joinParent(children);
    }
    // TDODO: rewrite for all (global)
}
