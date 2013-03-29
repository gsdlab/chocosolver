package org.clafer.analysis;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import org.clafer.ast.AstAbstractClafer;
import org.clafer.ast.AstBoolExpression;
import org.clafer.ast.AstCard;
import org.clafer.ast.AstClafer;
import org.clafer.ast.AstCompare;
import org.clafer.ast.AstConcreteClafer;
import org.clafer.ast.AstConstantInt;
import org.clafer.ast.AstDecl;
import org.clafer.ast.AstExpression;
import org.clafer.ast.AstExpressionVisitor;
import org.clafer.ast.AstJoin;
import org.clafer.ast.AstJoinParent;
import org.clafer.ast.AstJoinRef;
import org.clafer.ast.AstLocal;
import org.clafer.ast.AstModel;
import org.clafer.ast.AstNone;
import org.clafer.ast.AstQuantify;
import org.clafer.ast.AstSetExpression;
import org.clafer.ast.AstThis;
import org.clafer.ast.AstUpcast;
import static org.clafer.ast.Asts.*;

/**
 *
 * @author jimmy
 */
public class CanonicalAnalysis {

    public static void analyze(AstModel model, Map<AstExpression, AstClafer> types) {
        List<AstClafer> clafers = AnalysisUtil.getClafers(model);
        CanonicalVisitor visitor = new CanonicalVisitor(types);
        for (AstClafer clafer : clafers) {
            List<AstBoolExpression> explicitConstraints = new ArrayList<AstBoolExpression>();
            for (AstBoolExpression constraint : clafer.getConstraints()) {
                AstBoolExpression canonicalConstraint = (AstBoolExpression) constraint.accept(visitor, null);
                explicitConstraints.add(canonicalConstraint);
            }
            clafer.withConstraints(explicitConstraints);
        }
    }

    private static class CanonicalVisitor implements AstExpressionVisitor<Void, AstExpression> {

        private final Map<AstExpression, AstClafer> types;

        CanonicalVisitor(Map<AstExpression, AstClafer> typeMap) {
            this.types = typeMap;
        }

        AstClafer getType(AstExpression ast) {
            AstClafer type = types.get(ast);
            if (type == null) {
                throw new AnalysisException(ast + " type not analyzed yet");
            }
            return type;
        }

        @Override
        public AstExpression visit(AstThis ast, Void a) {
            return ast;
        }

        @Override
        public AstExpression visit(AstConstantInt ast, Void a) {
            return ast;
        }

        @Override
        public AstExpression visit(AstJoin ast, Void a) {
            AstSetExpression left = (AstSetExpression) ast.getLeft().accept(this, a);
            AstClafer leftType = getType(ast.getLeft());
            AstConcreteClafer rightType = ast.getRight();

            if (rightType.hasParent()) {
                AstClafer joinType = rightType.getParent();
                if (leftType.equals(joinType)) {
                    return join(left, rightType);
                }
                if (AnalysisUtil.isAssignable(leftType, joinType)) {
                    AstUpcast upcast = upcast(left, (AstAbstractClafer) joinType);
                    return join(upcast, rightType);
                }
            }
            throw new AnalysisException("Cannot join " + leftType.getName() + "." + rightType.getName());
        }

        @Override
        public AstExpression visit(AstJoinParent ast, Void a) {
            AstSetExpression children = (AstSetExpression) ast.getChildren().accept(this, a);
            return joinParent(children);
        }

        @Override
        public AstExpression visit(AstJoinRef ast, Void a) {
            AstSetExpression deref = (AstSetExpression) ast.getDeref().accept(this, a);
            return joinRef(deref);
        }

        @Override
        public AstExpression visit(AstCard ast, Void a) {
            AstSetExpression set = (AstSetExpression) ast.getSet().accept(this, a);
            return card(set);
        }

        @Override
        public AstExpression visit(AstCompare ast, Void a) {
            AstSetExpression left = (AstSetExpression) ast.getLeft().accept(this, a);
            AstSetExpression right = (AstSetExpression) ast.getRight().accept(this, a);
            return compare(left, ast.getOp(), right);
        }

        @Override
        public AstExpression visit(AstUpcast ast, Void a) {
            AstSetExpression base = (AstSetExpression) ast.getBase().accept(this, a);
            AstAbstractClafer to = ast.getTarget();
            return upcast(base, to);
        }

        @Override
        public AstExpression visit(AstNone ast, Void a) {
            AstSetExpression set = (AstSetExpression) ast.getSet().accept(this, a);
            return none(set);
        }

        @Override
        public AstExpression visit(AstLocal ast, Void a) {
            return ast;
        }

        @Override
        public AstExpression visit(AstQuantify ast, Void a) {
            List<AstDecl> decls = new ArrayList<AstDecl>();
            for (AstDecl decl : ast.getDecls()) {
                AstSetExpression body = (AstSetExpression) decl.getBody().accept(this, a);
                decls.add(decl.withBody(body));
            }
            AstBoolExpression body = (AstBoolExpression) ast.getBody().accept(this, a);
            return quantify(ast.getQuantifier(), decls.toArray(new AstDecl[decls.size()]), body);
        }
    }
}
