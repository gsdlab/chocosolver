package org.clafer.ast.analysis;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import org.clafer.common.Check;
import org.clafer.ast.AstSetTest;
import org.clafer.ast.AstGlobal;
import static org.clafer.ast.Asts.*;
import org.clafer.ast.AstAbstractClafer;
import org.clafer.ast.AstArithm;
import org.clafer.ast.AstBoolClafer;
import org.clafer.ast.AstBoolExpr;
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
import org.clafer.ast.AstPrimClafer;
import org.clafer.ast.AstQuantify;
import org.clafer.ast.AstSetArithm;
import org.clafer.ast.AstSetExpr;
import org.clafer.ast.AstThis;
import org.clafer.ast.AstUpcast;
import org.clafer.ast.AstUtil;
import org.clafer.common.Util;

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
        for (AstClafer clafer : AstUtil.getClafers(model)) {
            TypeVisitor visitor = new TypeVisitor(clafer, types);
            for (AstConstraint constraint : clafer.getConstraints()) {
                TypedExpr<AstBoolExpr> expr = visitor.typeCheck(constraint.getExpr());
                constraint.setExpr(expr.getExpr());
            }
        }
        return types;
    }

    private static class TypeVisitor implements AstExprVisitor<Void, TypedExpr<?>> {

        private final AstClafer context;
        private final Map<AstExpr, AstClafer> types;

        TypeVisitor(AstClafer context, Map<AstExpr, AstClafer> typeMap) {
            this.context = context;
            this.types = typeMap;
        }

        private <T extends AstExpr> TypedExpr<T> typeCheck(T expr) {
            @SuppressWarnings("unchecked")
            TypedExpr<T> typedExpr = (TypedExpr<T>) expr.accept(this, null);
            return typedExpr;
        }

        private <T extends AstExpr> TypedExpr<T>[] typeCheck(T[] exprs) {
            @SuppressWarnings("unchecked")
            TypedExpr<T>[] typeChecked = new TypedExpr[exprs.length];
            for (int i = 0; i < exprs.length; i++) {
                typeChecked[i] = typeCheck(exprs[i]);
            }
            return typeChecked;
        }

        /**
         * Multilevel upcast.
         *
         * @param expr the expression
         * @param target the target type
         * @return the same expression but with the target type or {@code null}
         * if the upcast is illegal
         */
        private TypedExpr<AstSetExpr> upcastTo(TypedExpr<AstSetExpr> expr, AstClafer target) {
            AstClafer exprType = expr.getType();
            if (exprType.equals(target)) {
                return expr;
            }
            AstSetExpr superExpr = expr.getExpr();
            List<AstAbstractClafer> superTypes = AstUtil.getSupers(exprType);
            for (AstAbstractClafer superType : superTypes) {
                superExpr = upcast(superExpr, superType);
                TypedExpr<AstSetExpr> typedSuper = put(superType, superExpr);
                if (superType.equals(target)) {
                    return typedSuper;
                }
            }
            return null;
        }

        /**
         * Multilevel upcast.
         *
         * @param exprs the expressions
         * @param target the target type
         * @return the same expressions but with the target type or {@code null}
         * if the upcast is illegal
         */
        private TypedExpr<AstSetExpr>[] upcastTo(TypedExpr<AstSetExpr>[] exprs, AstClafer target) {
            @SuppressWarnings("unchecked")
            TypedExpr<AstSetExpr>[] upcasts = new TypedExpr[exprs.length];
            for (int i = 0; i < upcasts.length; i++) {
                TypedExpr<AstSetExpr> upcast = upcastTo(exprs[i], target);
                if (upcast == null) {
                    return null;
                }
                upcasts[i] = upcast;
            }
            return upcasts;
        }

        private <T extends AstExpr> TypedExpr<T> put(AstClafer type, T expr) {
            types.put(expr, type);
            return new TypedExpr<T>(type, expr);
        }

        @Override
        public TypedExpr<AstThis> visit(AstThis ast, Void a) {
            return put(context, ast);
        }

        @Override
        public TypedExpr<AstGlobal> visit(AstGlobal ast, Void a) {
            return put(ast.getType(), ast);
        }

        @Override
        public TypedExpr<AstConstant> visit(AstConstant ast, Void a) {
            return put(IntType, ast);
        }

        @Override
        public TypedExpr<AstSetExpr> visit(AstJoin ast, Void a) {
            TypedExpr<AstSetExpr> left = typeCheck(ast.getLeft());
            AstConcreteClafer rightType = ast.getRight();
            if (!AstUtil.isTop(rightType)) {
                AstClafer joinType = rightType.getParent();
                TypedExpr<AstSetExpr> upcast = upcastTo(left, joinType);
                if (upcast != null) {
                    return put(rightType, join(upcast.getExpr(), rightType));
                }
            }
            throw new AnalysisException("Cannot join " + left.getType().getName() + " . " + rightType.getName());
        }

        @Override
        public TypedExpr<AstSetExpr> visit(AstJoinParent ast, Void a) {
            TypedExpr<AstSetExpr> children = typeCheck(ast.getChildren());
            if (!(children.getType() instanceof AstConcreteClafer)) {
                throw new AnalysisException("Cannot join " + children.getType().getName() + " . parent");
            }
            AstConcreteClafer concreteChildrenType = (AstConcreteClafer) children.getType();
            if (AstUtil.isTop(concreteChildrenType)) {
                throw new AnalysisException("Cannot join " + children.getType().getName() + " . parent");
            }
            return put(concreteChildrenType.getParent(), joinParent(children.getExpr()));
        }

        @Override
        public TypedExpr<AstSetExpr> visit(AstJoinRef ast, Void a) {
            TypedExpr<AstSetExpr> deref = typeCheck(ast.getDeref());
            AstClafer derefType = deref.getType();
            while (derefType != null && !derefType.hasRef()) {
                derefType = derefType.getSuperClafer();
            }
            if (derefType == null) {
                throw new AnalysisException("Cannot join " + deref.getType().getName() + " . ref");
            }
            TypedExpr<AstSetExpr> upcast = upcastTo(deref, derefType);
            assert upcast != null;
            return put(derefType.getRef().getTargetType(), joinRef(upcast.getExpr()));
        }

        @Override
        public TypedExpr<AstSetExpr> visit(AstCard ast, Void a) {
            TypedExpr<AstSetExpr> set = typeCheck(ast.getSet());
            if (set.getType() instanceof AstPrimClafer) {
                throw new AnalysisException("Cannot |" + set.getType().getName() + "|");
            }
            return put(IntType, card(set.getExpr()));
        }

        @Override
        public TypedExpr<AstBoolExpr> visit(AstSetTest ast, Void a) {
            TypedExpr<AstSetExpr> left = typeCheck(ast.getLeft());
            TypedExpr<AstSetExpr> right = typeCheck(ast.getRight());
            AstClafer unionType = AstUtil.getUnionType(left.getType(), right.getType());
            if (unionType == null) {
                throw new AnalysisException("Cannot " + left.getType().getName() + " "
                        + ast.getOp().getSyntax() + " " + right.getType().getName());
            }
            return put(BoolType, test(upcastTo(left, unionType).getExpr(), ast.getOp(),
                    upcastTo(right, unionType).getExpr()));
        }

        @Override
        public TypedExpr<AstBoolExpr> visit(AstCompare ast, Void a) {
            TypedExpr<AstSetExpr> left = typeCheck(ast.getLeft());
            TypedExpr<AstSetExpr> right = typeCheck(ast.getRight());
            if (!(left.getType() instanceof AstIntClafer) || !(right.getType() instanceof AstIntClafer)) {
                throw new AnalysisException("Cannot " + left.getType().getName() + " "
                        + ast.getOp().getSyntax() + " " + right.getType().getName());
            }
            return put(BoolType, compare(left.getExpr(), ast.getOp(), right.getExpr()));
        }

        @Override
        public TypedExpr<AstSetExpr> visit(AstArithm ast, Void a) {
            TypedExpr<AstSetExpr>[] operands = typeCheck(ast.getOperands());
            for (TypedExpr<AstSetExpr> operand : operands) {
                if (!(operand.getType() instanceof AstIntClafer)) {
                    throw new AnalysisException("Cannot "
                            + Util.intercalate(" " + ast.getOp().getSyntax() + " ",
                            AstUtil.getNames(getTypes(operands))));
                }
            }
            return put(IntType, arithm(ast.getOp(), getExprs(operands)));
        }

        @Override
        public TypedExpr<AstSetExpr> visit(AstSetArithm ast, Void a) {
            TypedExpr<AstSetExpr>[] operands = typeCheck(ast.getOperands());
            AstClafer unionType = AstUtil.getUnionType(getTypes(operands));
            if (unionType == null) {
                throw new AnalysisException("Cannot "
                        + Util.intercalate(" " + ast.getOp().getSyntax() + " ",
                        AstUtil.getNames(getTypes(operands))));
            }
            TypedExpr<AstSetExpr>[] upcasts = upcastTo(operands, unionType);
            assert upcasts != null;
            return put(unionType, setArithm(ast.getOp(), getExprs(upcasts)));
        }

        @Override
        public TypedExpr<AstSetExpr> visit(AstUpcast ast, Void a) {
            TypedExpr<AstSetExpr> base = typeCheck(ast.getBase());
            AstAbstractClafer to = ast.getTarget();
            if (!AstUtil.isAssignable(base.getType(), to)) {
                throw new AnalysisException("Cannot upcast from " + base.getType().getName() + " to " + to);
            }
            return put(to, upcast(base.getExpr(), ast.getTarget()));
        }

        @Override
        public TypedExpr<AstBoolExpr> visit(AstNone ast, Void a) {
            TypedExpr<AstSetExpr> set = typeCheck(ast.getSet());
            return put(BoolType, none(set.getExpr()));
        }

        @Override
        public TypedExpr<AstLocal> visit(AstLocal ast, Void a) {
            return put(AnalysisUtil.notNull(ast + " type not analyzed yet", types.get(ast)), ast);
        }

        @Override
        public TypedExpr<AstBoolExpr> visit(AstQuantify ast, Void a) {
            AstDecl[] decls = new AstDecl[ast.getDecls().length];
            for (int i = 0; i < ast.getDecls().length; i++) {
                AstDecl decl = ast.getDecls()[i];
                TypedExpr<AstSetExpr> body = typeCheck(decl.getBody());
                for (AstLocal local : decl.getLocals()) {
                    put(body.getType(), local);
                }
                decls[i] = decl(decl.getLocals(), body.getExpr());
            }
            TypedExpr<AstBoolExpr> body = typeCheck(ast.getBody());
            if (body.getType() instanceof AstBoolClafer) {
                return put(BoolType, quantify(ast.getQuantifier(), decls, body.getExpr()));
            }
            throw new AnalysisException();
        }
    }

    private static AstClafer[] getTypes(TypedExpr<?>... exprs) {
        AstClafer[] types = new AstClafer[exprs.length];
        for (int i = 0; i < types.length; i++) {
            types[i] = exprs[i].getType();
        }
        return types;
    }

    private static <T extends AstSetExpr> AstSetExpr[] getExprs(TypedExpr<T>... exprs) {
        AstSetExpr[] setExprs = new AstSetExpr[exprs.length];
        for (int i = 0; i < setExprs.length; i++) {
            setExprs[i] = exprs[i].getExpr();
        }
        return setExprs;
    }

    private static class TypedExpr<T extends AstExpr> {

        private final AstClafer type;
        private final T expr;

        TypedExpr(AstClafer type, T expr) {
            this.type = Check.notNull(type);
            this.expr = Check.notNull(expr);
        }

        public AstClafer getType() {
            return type;
        }

        public T getExpr() {
            return expr;
        }
    }
}
