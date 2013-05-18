package org.clafer.ast.analysis;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import org.clafer.common.Check;
import org.clafer.ast.AstEqual;
import org.clafer.ast.AstGlobal;
import static org.clafer.ast.Asts.*;
import org.clafer.ast.AstAbstractClafer;
import org.clafer.ast.AstCard;
import org.clafer.ast.AstClafer;
import org.clafer.ast.AstCompare;
import org.clafer.ast.AstConcreteClafer;
import org.clafer.ast.AstConstant;
import org.clafer.ast.AstConstraint;
import org.clafer.ast.AstDecl;
import org.clafer.ast.AstExpr;
import org.clafer.ast.AstExprVisitor;
import org.clafer.ast.AstIntClafer;
import org.clafer.ast.AstJoin;
import org.clafer.ast.AstJoinParent;
import org.clafer.ast.AstJoinRef;
import org.clafer.ast.AstLocal;
import org.clafer.ast.AstModel;
import org.clafer.ast.AstNone;
import org.clafer.ast.AstQuantify;
import org.clafer.ast.AstThis;
import org.clafer.ast.AstUpcast;
import org.clafer.ast.AstUtil;

/**
 * Type checks and creates explicit upcast nodes in the ast.
 * 
 * @author jimmy
 */
public class TypeAnalysis {

    private TypeAnalysis() {
    }

    public static Map<AstExpr, AstClafer> analyze(AstModel model) {
        Map<AstExpr, AstClafer> types = new HashMap<AstExpr, AstClafer>();
        List<AstClafer> clafers = AstUtil.getClafers(model);
        for (AstClafer clafer : clafers) {
            TypeVisitor visitor = new TypeVisitor(clafer, types);
            for (AstConstraint constraint : clafer.getConstraints()) {
                constraint.getExpr().accept(visitor, null);
            }
        }
        return types;
    }

    public static Map<AstExpr, AstClafer> analyze(AstClafer clafer) {
        Map<AstExpr, AstClafer> types = new HashMap<AstExpr, AstClafer>();
        TypeVisitor visitor = new TypeVisitor(clafer, types);
        for (AstConstraint constraint : clafer.getConstraints()) {
            constraint.getExpr().accept(visitor, null);
        }
        return types;
    }

    private static class TypeVisitor implements AstExprVisitor<Void, AstClafer> {

        private final AstClafer context;
        private final Map<AstExpr, AstClafer> types;

        TypeVisitor(AstClafer context, Map<AstExpr, AstClafer> typeMap) {
            this.context = context;
            this.types = typeMap;
        }

        AstClafer put(AstClafer type, AstExpr expression) {
            types.put(expression, type);
            return type;
        }

        @Override
        public AstClafer visit(AstThis ast, Void a) {
            return put(context, ast);
        }

        @Override
        public AstClafer visit(AstGlobal ast, Void a) {
            return put(ast.getType(), ast);
        }

        @Override
        public AstClafer visit(AstConstant ast, Void a) {
            return put(IntType, ast);
        }

        @Override
        public AstClafer visit(AstJoin ast, Void a) {
            AstClafer leftType = ast.getLeft().accept(this, a);
            AstConcreteClafer rightType = ast.getRight();
            if (rightType.hasParent()) {
                AstClafer joinType = rightType.getParent();
                if (AstUtil.isAssignable(leftType, joinType)) {
                    return put(rightType, ast);
                }
            }
            throw new AnalysisException("Cannot join " + leftType.getName() + " . " + rightType.getName());
        }

        @Override
        public AstClafer visit(AstJoinParent ast, Void a) {
            AstClafer childrenType = ast.getChildren().accept(this, a);
            if (!(childrenType instanceof AstConcreteClafer)) {
                throw new AnalysisException("Cannot join " + childrenType.getName() + " . parent");
            }
            AstConcreteClafer concreteChildrenType = (AstConcreteClafer) childrenType;
            if (!concreteChildrenType.hasParent()) {
                throw new AnalysisException("Cannot join " + childrenType.getName() + " . parent");
            }
            return put(concreteChildrenType.getParent(), ast);
        }

        @Override
        public AstClafer visit(AstJoinRef ast, Void a) {
            AstClafer derefType = ast.getDeref().accept(this, a);
            if (!derefType.hasRef()) {
                throw new AnalysisException("Cannot join " + derefType.getName() + " . ref");
            }
            return put(derefType.getRef().getTargetType(), ast);
        }

        @Override
        public AstClafer visit(AstCard ast, Void a) {
            ast.getSet().accept(this, a);
            return put(IntType, ast);
        }

        @Override
        public AstClafer visit(AstEqual ast, Void a) {
            AstClafer leftType = ast.getLeft().accept(this, a);
            AstClafer rightType = ast.getRight().accept(this, a);
            if (!AstUtil.hasNonEmptyIntersectionType(leftType, rightType)) {
                throw new AnalysisException("Cannot " + leftType.getName() + " " + ast.getOp().getSyntax() + " " + rightType.getName());
            }
            return put(BoolType, ast);
        }

        @Override
        public AstClafer visit(AstCompare ast, Void a) {
            AstClafer leftType = ast.getLeft().accept(this, a);
            AstClafer rightType = ast.getRight().accept(this, a);
            if (!(leftType instanceof AstIntClafer) || !(rightType instanceof AstIntClafer)) {
                throw new AnalysisException("Cannot " + leftType.getName() + " " + ast.getOp().getSyntax() + " " + rightType.getName());
            }
            return put(BoolType, ast);
        }

        @Override
        public AstClafer visit(AstUpcast ast, Void a) {
            AstClafer baseType = ast.getBase().accept(this, a);
            AstAbstractClafer to = ast.getTarget();
            if (!AstUtil.isAssignable(baseType, to)) {
                throw new AnalysisException("Cannot upcast from " + baseType + " to " + to);
            }
            return put(to, ast);
        }

        @Override
        public AstClafer visit(AstNone ast, Void a) {
            ast.getSet().accept(this, a);
            return put(BoolType, ast);
        }

        @Override
        public AstClafer visit(AstLocal ast, Void a) {
            return put(AnalysisUtil.notNull(ast + " type not analyzed yet", types.get(ast)), ast);
        }

        @Override
        public AstClafer visit(AstQuantify ast, Void a) {
            for (AstDecl decl : ast.getDecls()) {
                AstClafer bodyType = decl.getBody().accept(this, a);
                for (AstLocal local : decl.getLocals()) {
                    put(bodyType, local);
                }
            }
            ast.getBody().accept(this, a);
            return put(BoolType, ast);
        }
    }

    private static class TypedExpression {

        private final AstClafer type;
        private final AstExpr expression;

        TypedExpression(AstClafer type, AstExpr expression) {
            this.type = Check.notNull(type);
            this.expression = Check.notNull(expression);
        }

        public AstClafer getType() {
            return type;
        }

        public AstExpr getExpression() {
            return expression;
        }
    }
}
