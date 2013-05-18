package org.clafer.ast.analysis;

import org.clafer.ast.AstUtil;
import org.clafer.ast.AstConstraint;
import org.clafer.ast.AstEqual;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import org.clafer.ast.AstAbstractClafer;
import org.clafer.ast.AstBoolExpr;
import org.clafer.ast.AstCard;
import org.clafer.ast.AstClafer;
import org.clafer.ast.AstCompare;
import org.clafer.ast.AstConcreteClafer;
import org.clafer.ast.AstConstant;
import org.clafer.ast.AstDecl;
import org.clafer.ast.AstExpr;
import org.clafer.ast.AstExprVisitor;
import org.clafer.ast.AstGlobal;
import org.clafer.ast.AstJoin;
import org.clafer.ast.AstJoinParent;
import org.clafer.ast.AstJoinRef;
import org.clafer.ast.AstLocal;
import org.clafer.ast.AstModel;
import org.clafer.ast.AstNone;
import org.clafer.ast.AstQuantify;
import org.clafer.ast.AstSetExpr;
import org.clafer.ast.AstThis;
import org.clafer.ast.AstUpcast;
import static org.clafer.ast.Asts.*;

/**
 * Inserts explicit upcasts.
 * 
 * @author jimmy
 */
public class CanonicalAnalysis {

    private CanonicalAnalysis() {
    }

    public static void analyze(AstModel model, Map<AstExpr, AstClafer> types) {
        List<AstClafer> clafers = AstUtil.getClafers(model);
        CanonicalVisitor visitor = new CanonicalVisitor(types);
        for (AstClafer clafer : clafers) {
            for (AstConstraint constraint : clafer.getConstraints()) {
                AstBoolExpr canonicalConstraint = (AstBoolExpr) constraint.getExpr().accept(visitor, null);
                constraint.setExpr(canonicalConstraint);
            }
        }
    }

    private static class CanonicalVisitor implements AstExprVisitor<Void, AstExpr> {

        private final Map<AstExpr, AstClafer> types;

        CanonicalVisitor(Map<AstExpr, AstClafer> typeMap) {
            this.types = typeMap;
        }

        AstClafer getType(AstExpr ast) {
            AstClafer type = types.get(ast);
            if (type == null) {
                throw new AnalysisException(ast + " type not analyzed yet");
            }
            return type;
        }

        @Override
        public AstExpr visit(AstThis ast, Void a) {
            return ast;
        }

        @Override
        public AstExpr visit(AstGlobal ast, Void a) {
            return ast;
        }

        @Override
        public AstExpr visit(AstConstant ast, Void a) {
            return ast;
        }

        @Override
        public AstExpr visit(AstJoin ast, Void a) {
            AstSetExpr left = (AstSetExpr) ast.getLeft().accept(this, a);
            AstClafer leftType = getType(ast.getLeft());
            AstConcreteClafer rightType = ast.getRight();

            if (rightType.hasParent()) {
                AstClafer joinType = rightType.getParent();
                if (leftType.equals(joinType)) {
                    return join(left, rightType);
                }
                if (AstUtil.isAssignable(leftType, joinType)) {
                    AstUpcast upcast = upcast(left, (AstAbstractClafer) joinType);
                    return join(upcast, rightType);
                }
            }
            throw new AnalysisException("Cannot join " + leftType.getName() + " . " + rightType.getName());
        }

        @Override
        public AstExpr visit(AstJoinParent ast, Void a) {
            AstSetExpr children = (AstSetExpr) ast.getChildren().accept(this, a);
            return joinParent(children);
        }

        @Override
        public AstExpr visit(AstJoinRef ast, Void a) {
            AstSetExpr deref = (AstSetExpr) ast.getDeref().accept(this, a);
            return joinRef(deref);
        }

        @Override
        public AstExpr visit(AstCard ast, Void a) {
            AstSetExpr set = (AstSetExpr) ast.getSet().accept(this, a);
            return card(set);
        }

        @Override
        public AstExpr visit(AstEqual ast, Void a) {
            AstSetExpr left = (AstSetExpr) ast.getLeft().accept(this, a);
            AstSetExpr right = (AstSetExpr) ast.getRight().accept(this, a);
            return equal(left, ast.getOp(), right);
        }

        @Override
        public AstExpr visit(AstCompare ast, Void a) {
            AstSetExpr left = (AstSetExpr) ast.getLeft().accept(this, a);
            AstSetExpr right = (AstSetExpr) ast.getRight().accept(this, a);
            return compare(left, ast.getOp(), right);
        }

        @Override
        public AstExpr visit(AstUpcast ast, Void a) {
            AstSetExpr base = (AstSetExpr) ast.getBase().accept(this, a);
            AstAbstractClafer to = ast.getTarget();
            return upcast(base, to);
        }

        @Override
        public AstExpr visit(AstNone ast, Void a) {
            AstSetExpr set = (AstSetExpr) ast.getSet().accept(this, a);
            return none(set);
        }

        @Override
        public AstExpr visit(AstLocal ast, Void a) {
            return ast;
        }

        @Override
        public AstExpr visit(AstQuantify ast, Void a) {
            List<AstDecl> decls = new ArrayList<AstDecl>();
            for (AstDecl decl : ast.getDecls()) {
                AstSetExpr body = (AstSetExpr) decl.getBody().accept(this, a);
                decls.add(decl.withBody(body));
            }
            AstBoolExpr body = (AstBoolExpr) ast.getBody().accept(this, a);
            return quantify(ast.getQuantifier(), decls.toArray(new AstDecl[decls.size()]), body);
        }
    }
}
