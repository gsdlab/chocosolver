package org.clafer.ast.analysis;

import java.util.HashMap;
import java.util.Map;
import java.util.Optional;
import java.util.Stack;
import org.clafer.ast.AstChildRelation;
import org.clafer.ast.AstClafer;
import org.clafer.ast.AstConcreteClafer;
import org.clafer.ast.AstConstant;
import org.clafer.ast.AstConstraint;
import org.clafer.ast.AstExpr;
import org.clafer.ast.AstJoin;
import org.clafer.ast.AstJoinParent;
import org.clafer.ast.AstJoinRef;
import org.clafer.ast.AstMembership;
import org.clafer.ast.AstSetTest;
import org.clafer.ast.AstThis;
import org.clafer.collection.Pair;

/**
 *
 * @author jimmy
 */
public class InverseAnalyzer {

    private static boolean isZero(AstConstant constant) {
        return constant.getValue().length == 1 && constant.getValue()[0].length == 1 && constant.getValue()[0][0] == 0;
    }

    private static boolean refTypeIsUnique(AstClafer clafer, Analysis analysis) {
        return clafer.hasRef() && (clafer.getRef().isUnique()
                || (clafer instanceof AstConcreteClafer && analysis.getCard((AstConcreteClafer) clafer).getHigh() == 1));
    }

    private static boolean thisJoinParent(AstExpr expr, Analysis analysis) {
        return expr instanceof AstJoinParent && ((AstJoinParent) expr).getChildren() instanceof AstThis
                || (expr instanceof AstConstant && isZero((AstConstant) expr) && analysis.getScope(((AstConstant) expr).getType().getClaferType()) == 1);
    }

    private static Optional<Pair<AstClafer, AstClafer>> thisJoinRefJoinChildJoinRef(AstExpr expr, Analysis analysis) {
        if (expr instanceof AstJoinRef) {
            AstJoinRef set = (AstJoinRef) expr;

            if (refTypeIsUnique(analysis.getType(set.getDeref()).getClaferType(), analysis)) {
                AstClafer type1 = analysis.getType(set.getDeref()).getClaferType();
                if (set.getDeref() instanceof AstJoin) {
                    AstJoin join = (AstJoin) set.getDeref();
                    if (join.getLeft() instanceof AstJoinRef && join.getRight() instanceof AstChildRelation) {
                        AstJoinRef joinRef = (AstJoinRef) join.getLeft();

                        if (joinRef.getDeref() instanceof AstThis && refTypeIsUnique(analysis.getType(joinRef.getDeref()).getClaferType(), analysis)) {
                            AstClafer type2 = analysis.getType(joinRef.getDeref()).getClaferType();
                            return Optional.of(new Pair<>(type1, type2));
                        }
                    }
                }
            }
        }
        return Optional.empty();
    }

    public static Map<AstClafer, AstClafer> analyze(Analysis analysis) {
        Stack<Pair<AstClafer, AstClafer>> direction = new Stack<>();
        Map<AstClafer, AstClafer> inverses = new HashMap<>();

        for (AstConstraint constraint : analysis.getConstraints()) {
            AstExpr expr = analysis.getExpr(constraint);

            if (expr instanceof AstMembership) {
                AstMembership membership = (AstMembership) expr;
                if (membership.getOp().equals(AstMembership.Op.In)) {
                    if (thisJoinParent(membership.getMember(), analysis)) {
                        thisJoinRefJoinChildJoinRef(membership.getSet(), analysis).ifPresent(direction::add);
                    }
                }
            } else if (expr instanceof AstSetTest) {
                AstSetTest setTest = (AstSetTest) expr;
                if (setTest.getOp().equals(AstSetTest.Op.Equal)) {
                    if (thisJoinParent(setTest.getLeft(), analysis)) {
                        thisJoinRefJoinChildJoinRef(setTest.getRight(), analysis).ifPresent(direction::add);
                    } else if (thisJoinParent(setTest.getRight(), analysis)) {
                        thisJoinRefJoinChildJoinRef(setTest.getLeft(), analysis).ifPresent(direction::add);
                    }
                }
            }
        }
        while (!direction.isEmpty()) {
            Pair<AstClafer, AstClafer> d = direction.pop();
            if (direction.remove(new Pair<>(d.getSnd(), d.getFst()))) {
                if (inverses.put(d.getFst(), d.getSnd()) != null) {
                    throw new IllegalStateException("Unexpected two inverse relationships for " + d.getFst());
                }
                if (inverses.put(d.getSnd(), d.getFst()) != null) {
                    throw new IllegalStateException("Unexpected two inverse relationships for " + d.getSnd());
                }
            }
        }
        return inverses;
    }
}
