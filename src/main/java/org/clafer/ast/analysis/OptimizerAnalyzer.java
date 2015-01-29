package org.clafer.ast.analysis;

import java.util.HashMap;
import java.util.Map;
import org.clafer.ast.AstBoolExpr;
import org.clafer.ast.AstChildRelation;
import org.clafer.ast.AstClafer;
import org.clafer.ast.AstCompare;
import org.clafer.ast.AstConcreteClafer;
import org.clafer.ast.AstConstant;
import org.clafer.ast.AstConstraint;
import org.clafer.ast.AstExpr;
import org.clafer.ast.AstExprRewriter;
import org.clafer.ast.AstGlobal;
import org.clafer.ast.AstJoin;
import org.clafer.ast.AstJoinParent;
import org.clafer.ast.AstMembership;
import org.clafer.ast.AstNot;
import org.clafer.ast.AstSetExpr;
import org.clafer.ast.AstSetTest;
import org.clafer.ast.AstThis;
import static org.clafer.ast.Asts.*;
import org.clafer.ast.Card;
import org.clafer.ast.ProductType;
import org.clafer.objective.Objective;

/**
 * Optimizes the expressions inside the constraints. Assumes type checking has
 * already passed.
 *
 * @author jimmy
 */
public class OptimizerAnalyzer extends AstExprRewriter<Analysis> implements Analyzer {

    @Override
    public Analysis analyze(Analysis analysis) {
        Map<AstConstraint, AstBoolExpr> constraintExprs = new HashMap<>(analysis.getConstraints().size());
        for (AstConstraint constraint : analysis.getConstraints()) {
            constraintExprs.put(constraint, rewrite(analysis.getExpr(constraint), analysis));
        }
        Map<Objective, AstSetExpr> objectiveExprs = new HashMap<>(analysis.getObjectives().length);
        for (Objective objective : analysis.getObjectives()) {
            objectiveExprs.put(objective, rewrite(analysis.getExpr(objective), analysis));
        }
        return analysis
                .setConstraintExprs(constraintExprs)
                .setObjectiveExprs(objectiveExprs);
    }

    @Override
    public AstExpr visit(AstJoin ast, Analysis a) {
        if (ast.getRight() instanceof AstChildRelation) {
            AstSetExpr left = rewrite(ast.getLeft(), a);
            AstConcreteClafer right = ((AstChildRelation) ast.getRight()).getChildType();
            if (left instanceof AstGlobal) {
                return global(right);
            } else if (left instanceof AstConstant) {
                Card childCard = a.getCard(right);
                if (Format.ParentGroup.equals(a.getFormat(right))) {
                    AstConstant constant = (AstConstant) left;
                    if (constant.getType().arity() == 1) {
                        int[] value = constant.getValue()[0];
                        assert childCard.isExact();
                        int[] childConstant = new int[value.length * childCard.getLow()];
                        for (int i = 0; i < constant.getValue().length; i++) {
                            for (int j = 0; j < childCard.getLow(); j++) {
                                childConstant[i * childCard.getLow() + j]
                                        = i * value[i] + j;
                            }
                        }
                        return constant(right, childConstant);
                    }
                }
                assert childCard.getLow() != a.getScope(right) :
                        "Didn't run scope analysis before format analysis?";
            }
            return join(left, ast.getRight());
        }
        return ast;
    }

    @Override
    public AstExpr visit(AstJoinParent ast, Analysis a) {
        AstSetExpr children = rewrite(ast.getChildren(), a);
        if (children instanceof AstThis) {
            ProductType type = a.getCommonSupertype(ast);
            if (a.getScope(type) == 1) {
                return constant(type, 0);
            }
        } else if (children instanceof AstGlobal) {
            ProductType childType = a.getCommonSupertype(ast.getChildren());
            if (childType.isClaferType()) {
                AstClafer childClafer = childType.getClaferType();
                if (childClafer instanceof AstConcreteClafer) {
                    AstConcreteClafer concreteChildType = (AstConcreteClafer) childClafer;
                    if (a.getCard(concreteChildType).hasLow()) {
                        return global(concreteChildType.getParent());
                    }
                }
            }
        } else if (children instanceof AstConstant) {
            AstConstant constant = (AstConstant) children;
            ProductType type = a.getCommonSupertype(ast);
            if (constant.getValue().length > 0 && a.getScope(type) == 1) {
                return constant(type, 0);
            }
        }
        return ast;
    }
    // TDODO: rewrite for all (global)

    @Override
    public AstExpr visit(AstNot ast, Analysis a) {
        AstBoolExpr expr = ast.getExpr();
        if (expr instanceof AstNot) {
            AstNot not = (AstNot) expr;
            return not.getExpr();
        } else if (expr instanceof AstSetTest) {
            AstSetTest test = (AstSetTest) expr;
            return test(test.getLeft(), test.getOp().negate(), test.getRight());
        } else if (expr instanceof AstCompare) {
            AstCompare compare = (AstCompare) expr;
            return compare(compare.getLeft(), compare.getOp().negate(), compare.getRight());
        } else if (expr instanceof AstMembership) {
            AstMembership membership = (AstMembership) expr;
            return membership(membership.getMember(), membership.getOp().negate(), membership.getSet());
        }
        return ast;
    }
}
