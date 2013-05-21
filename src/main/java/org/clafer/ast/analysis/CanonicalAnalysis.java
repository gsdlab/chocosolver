package org.clafer.ast.analysis;

import java.lang.reflect.Array;
import org.clafer.ast.AstUtil;
import org.clafer.ast.AstConstraint;
import org.clafer.ast.AstEqual;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import org.clafer.ast.AstAbstractClafer;
import org.clafer.ast.AstArithm;
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
                constraint.setExpr(visitor.canonicalize(constraint.getExpr()));
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

        private <T extends AstExpr> T canonicalize(T expr) {
            @SuppressWarnings("unchecked")
            T canonical = (T) expr.accept(this, null);
            return canonical;
        }

        private <T extends AstExpr> T[] canonicalize(T[] exprs) {
            @SuppressWarnings("unchecked")
            T[] canonical = (T[]) Array.newInstance(exprs.getClass().getComponentType(), exprs.length);
            for (int i = 0; i < canonical.length; i++) {
                canonical[i] = canonicalize(exprs[i]);
            }
            return canonical;
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
            AstSetExpr left = canonicalize(ast.getLeft());
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
            return joinParent(canonicalize(ast.getChildren()));
        }

        @Override
        public AstExpr visit(AstJoinRef ast, Void a) {
            return joinRef(canonicalize(ast.getDeref()));
        }

        @Override
        public AstExpr visit(AstCard ast, Void a) {
            return card(canonicalize(ast.getSet()));
        }

        @Override
        public AstExpr visit(AstEqual ast, Void a) {
            return equal(canonicalize(ast.getLeft()), ast.getOp(), canonicalize(ast.getRight()));
        }

        @Override
        public AstExpr visit(AstCompare ast, Void a) {
            return compare(canonicalize(ast.getLeft()), ast.getOp(), canonicalize(ast.getRight()));
        }

        @Override
        public AstExpr visit(AstArithm ast, Void a) {
            return arithm(ast.getOp(), canonicalize(ast.getOperands()));
        }

        @Override
        public AstExpr visit(AstUpcast ast, Void a) {
            return upcast(canonicalize(ast.getBase()), ast.getTarget());
        }

        @Override
        public AstExpr visit(AstNone ast, Void a) {
            return none(canonicalize(ast.getSet()));
        }

        @Override
        public AstExpr visit(AstLocal ast, Void a) {
            return ast;
        }

        @Override
        public AstExpr visit(AstQuantify ast, Void a) {
            List<AstDecl> decls = new ArrayList<AstDecl>();
            for (AstDecl decl : ast.getDecls()) {
                decls.add(decl.withBody(canonicalize(decl.getBody())));
            }
            return quantify(ast.getQuantifier(), decls.toArray(new AstDecl[decls.size()]), canonicalize(ast.getBody()));
        }
    }
}
