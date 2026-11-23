package org.clafer.ast.analysis;

import java.util.HashMap;
import java.util.Map;
import org.clafer.assertion.Assertion;
import org.clafer.ast.AstBoolExpr;
import org.clafer.ast.AstCard;
import org.clafer.ast.AstChildRelation;
import org.clafer.ast.AstClafer;
import org.clafer.ast.AstCompare;
import org.clafer.ast.AstConcreteClafer;
import org.clafer.ast.AstConstant;
import org.clafer.ast.AstConstraint;
import org.clafer.ast.AstExpr;
import org.clafer.ast.AstExprRewriter;
import org.clafer.ast.AstGlobal;
import org.clafer.ast.AstIfThenElse;
import org.clafer.ast.AstJoin;
import org.clafer.ast.AstJoinParent;
import org.clafer.ast.AstJoinRef;
import org.clafer.ast.AstMembership;
import org.clafer.ast.AstNot;
import org.clafer.ast.AstSetExpr;
import org.clafer.ast.AstSetTest;
import org.clafer.ast.AstThis;
import org.clafer.ast.AstUpcast;
import static org.clafer.ast.Asts.compare;
import static org.clafer.ast.Asts.constant;
import static org.clafer.ast.Asts.equal;
import static org.clafer.ast.Asts.global;
import static org.clafer.ast.Asts.ifThenElse;
import static org.clafer.ast.Asts.join;
import static org.clafer.ast.Asts.membership;
import static org.clafer.ast.Asts.test;
import org.clafer.ast.Card;
import org.clafer.ast.ProductType;
import org.clafer.common.Util;
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
        Map<Assertion, AstBoolExpr> assertionExprs = new HashMap<>(analysis.getAssertions().length);
        for (Assertion assertion : analysis.getAssertions()) {
            assertionExprs.put(assertion, rewrite(analysis.getExpr(assertion), analysis));
        }
        return analysis
                .setConstraintExprs(constraintExprs)
                .setObjectiveExprs(objectiveExprs)
                .setAssertionExprs(assertionExprs);
    }

    @Override
    public AstExpr visit(AstGlobal ast, Analysis a) {
        Card globalCard = a.getGlobalCard(ast.type());
        if (globalCard.isExact() && (a.getFormat(ast.type()).equals(Format.LowGroup) || globalCard.getLow() == 1)) {
            return constant(ast.type(), Util.fromTo(0, globalCard.getLow()));
        }
        return ast;
    }

    @Override
    public AstExpr visit(AstJoin ast, Analysis a) {
        if (ast.right() instanceof AstChildRelation) {
            AstSetExpr left = rewrite(ast.left(), a);
            AstClafer right = ((AstChildRelation) ast.right()).childType();
            if (left instanceof AstGlobal) {
                return global(right);
            } else if (left instanceof AstConstant && right instanceof AstConcreteClafer) {
                // TODO what if right is abstract?
                Card childCard = a.getCard((AstConcreteClafer) right);
                if (Format.ParentGroup.equals(a.getFormat(right))) {
                    AstConstant constant = (AstConstant) left;
                    if (constant.type().arity() == 1) {
                        int[] value = constant.value()[0];
                        assert childCard.isExact();
                        int[] childConstant = new int[value.length * childCard.getLow()];
                        for (int i = 0; i < constant.value().length; i++) {
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
            return join(left, ast.right());
        }
        return ast;
    }

    @Override
    public AstExpr visit(AstJoinParent ast, Analysis a) {
        AstSetExpr children = rewrite(ast.children(), a);
        if (children instanceof AstThis) {
            ProductType type = a.getCommonSupertype(ast);
            if (a.getScope(type) == 1) {
                return constant(type, 0);
            }
        } else if (children instanceof AstGlobal) {
            ProductType childType = a.getCommonSupertype(ast.children());
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
            if (constant.value().length > 0 && a.getScope(type) == 1) {
                return constant(type, 0);
            }
        }
        return ast;
    }
    // TDODO: rewrite for all (global)

    @Override
    public AstExpr visit(AstNot ast, Analysis a) {
        AstBoolExpr expr = ast.expr();
        if (expr instanceof AstNot) {
            AstNot not = (AstNot) expr;
            return not.expr();
        } else if (expr instanceof AstSetTest) {
            AstSetTest test = (AstSetTest) expr;
            return test(test.left(), test.op().negate(), test.right());
        } else if (expr instanceof AstCompare) {
            AstCompare compare = (AstCompare) expr;
            return compare(compare.left(), compare.op().negate(), compare.right());
        } else if (expr instanceof AstMembership) {
            AstMembership membership = (AstMembership) expr;
            return membership(membership.member(), membership.op().negate(), membership.set());
        }
        return ast;
    }

    private boolean isZero(AstSetExpr expr) {
        if (expr instanceof AstConstant) {
            AstConstant constant = (AstConstant) expr;
            if (constant.type().isInt()) {
                if (constant.value().length == 1) {
                    return constant.value()[0].length == 1 && constant.value()[0][0] == 0;
                }
            }
        }
        return false;
    }

    private AstSetExpr removeUpCast(AstSetExpr expr) {
        if (expr instanceof AstUpcast) {
            return removeUpCast(((AstUpcast) expr).base());
        }
        return expr;
    }

    private AstExpr rewriteIfThenElse(AstBoolExpr antecedent,
            AstSetExpr consequentLeft, AstSetExpr consequentRight, AstSetExpr alternativeLeft, AstSetExpr alternativeRight,
            Analysis analysis) {
        // rewrite
        //   if a then b = c else b = d
        // to
        //   b = (if a then c else d)
        if (consequentLeft.equals(alternativeLeft)) {
            return equal(consequentLeft,
                    ifThenElse(antecedent, consequentRight, alternativeRight));
        }
        // rewrite
        //   if a then b = c else none b
        // to
        //   b = (if a then c else {})
        if (consequentLeft instanceof AstJoinRef) {
            AstJoinRef joinRef = (AstJoinRef) consequentLeft;
            if (alternativeLeft instanceof AstCard && isZero(alternativeRight)) {
                AstCard card = (AstCard) alternativeLeft;
                if (removeUpCast(joinRef.deref()).equals(removeUpCast(card.set()))) {
                    return equal(consequentLeft,
                            ifThenElse(antecedent,
                                    consequentRight,
                                    constant(analysis.getType(consequentRight).getCommonSupertype().getClaferType())
                            ));
                }
            }
        }

        return null;
    }

    @Override
    public AstExpr visit(AstIfThenElse ast, Analysis a) {
        if (ast.consequent() instanceof AstSetTest && ast.alternative() instanceof AstSetTest) {
            AstBoolExpr antecedent = ast.antecedent();
            AstSetTest consequent = (AstSetTest) ast.consequent();
            AstSetTest alternative = (AstSetTest) ast.alternative();

            AstExpr rewrite = rewriteIfThenElse(antecedent, consequent.left(), consequent.right(), alternative.left(), alternative.right(), a);
            if (rewrite != null) {
                return rewrite;
            }
            rewrite = rewriteIfThenElse(antecedent, consequent.left(), consequent.right(), alternative.right(), alternative.left(), a);
            if (rewrite != null) {
                return rewrite;
            }
            rewrite = rewriteIfThenElse(antecedent, consequent.right(), consequent.left(), alternative.left(), alternative.right(), a);
            if (rewrite != null) {
                return rewrite;
            }
            rewrite = rewriteIfThenElse(antecedent, consequent.right(), consequent.left(), alternative.right(), alternative.left(), a);
            if (rewrite != null) {
                return rewrite;
            }
        }
        return ast;
    }
}
