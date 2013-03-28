package org.clafer.analysis;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import org.clafer.Check;
import static org.clafer.ast.Asts.*;
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
import org.clafer.ast.AstIntClafer;
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

/**
 * Type checks and creates explicit upcast nodes in the ast.
 * 
 * @author jimmy
 */
public class TypeAnalysis {

    public static Map<AstExpression, AstClafer> analyze(AstModel model) {
        Map<AstExpression, AstClafer> types = new HashMap<AstExpression, AstClafer>();
        List<AstClafer> clafers = AnalysisUtil.getClafers(model);
        for (AstClafer clafer : clafers) {
            TypeVisitor visitor = new TypeVisitor(clafer, types);
            List<AstBoolExpression> explicitConstraints = new ArrayList<AstBoolExpression>();
            for (AstBoolExpression constraint : clafer.getConstraints()) {
                TypedExpression typedConstraint = constraint.accept(visitor, null);
                explicitConstraints.add((AstBoolExpression) typedConstraint.getExpression());
            }
            clafer.withConstraints(explicitConstraints);
        }
        return types;
    }

    private static class TypeVisitor implements AstExpressionVisitor<Void, TypedExpression> {

        private final AstClafer context;
        private final Map<AstExpression, AstClafer> types;

        public TypeVisitor(AstClafer context, Map<AstExpression, AstClafer> typeMap) {
            this.context = context;
            this.types = typeMap;
        }

        TypedExpression put(AstClafer type, AstExpression expression) {
            types.put(expression, type);
            return new TypedExpression(type, expression);
        }

        @Override
        public TypedExpression visit(AstThis ast, Void a) {
            return put(context, ast);
        }

        @Override
        public TypedExpression visit(AstConstantInt ast, Void a) {
            return put(IntType, ast);
        }

        @Override
        public TypedExpression visit(AstJoin ast, Void a) {
            TypedExpression $ast = ast.getLeft().accept(this, a);
            AstClafer leftType = $ast.getType();
            AstSetExpression left = (AstSetExpression) $ast.getExpression();
            AstConcreteClafer rightType = ast.getRight();

            if (rightType.hasParent()) {
                AstClafer joinType = rightType.getParent();
                if (leftType.equals(joinType)) {
                    return put(rightType, join(left, rightType));
                }
                if (AnalysisUtil.isAssignable(leftType, joinType)) {
                    AstUpcast upcast = upcast(left, (AstAbstractClafer) joinType);
                    put(joinType, upcast);
                    return put(rightType, join(upcast, rightType));
                }
            }
            throw new AnalysisException("Cannot join " + leftType.getName() + "." + rightType.getName());
        }

        @Override
        public TypedExpression visit(AstJoinParent ast, Void a) {
            TypedExpression $children = ast.getChildren().accept(this, a);
            AstClafer childrenType = $children.getType();
            AstSetExpression children = (AstSetExpression) $children.getExpression();
            if (!(childrenType instanceof AstConcreteClafer)) {
                throw new AnalysisException("Cannot join " + childrenType.getName() + ".parent");
            }
            AstConcreteClafer concreteChildrenType = (AstConcreteClafer) childrenType;
            if (!concreteChildrenType.hasParent()) {
                throw new AnalysisException("Cannot join " + childrenType.getName() + ".parent");
            }
            return put(concreteChildrenType.getParent(), joinParent(children));
        }

        @Override
        public TypedExpression visit(AstJoinRef ast, Void a) {
            TypedExpression $ast = ast.getDeref().accept(this, a);
            AstClafer derefType = $ast.getType();
            AstSetExpression deref = (AstSetExpression) $ast.getExpression();
            if (!derefType.hasRef()) {
                throw new AnalysisException("Cannot join " + derefType.getName() + ".ref");
            }
            return put(derefType.getRef().getTargetType(), joinRef(deref));
        }

        @Override
        public TypedExpression visit(AstCard ast, Void a) {
            TypedExpression $set = ast.getSet().accept(this, a);
            return put(IntType, card((AstSetExpression) $set.getExpression()));
        }

        @Override
        public TypedExpression visit(AstCompare ast, Void a) {
            TypedExpression $leftAst = ast.getLeft().accept(this, a);
            TypedExpression $rightAst = ast.getRight().accept(this, a);
            AstClafer leftType = $leftAst.getType();
            AstClafer rightType = $rightAst.getType();
            AstExpression left = $leftAst.getExpression();
            AstExpression right = $rightAst.getExpression();
            switch (ast.getOp()) {
                case Equal:
                case NotEqual:
                    if (!AnalysisUtil.hasNonEmptyIntersectionType(leftType, rightType)) {
                        throw new AnalysisException("Cannot " + leftType.getName() + " " + ast.getOp().getSyntax() + " " + rightType.getName());
                    }
                    return put(BoolType, compare(left, ast.getOp(), right));
                case LessThan:
                case LessThanEqual:
                case GreaterThan:
                case GreaterThanEqual:
                    if (!(leftType instanceof AstIntClafer) || !(rightType instanceof AstIntClafer)) {
                        throw new AnalysisException("Cannot " + leftType.getName() + " " + ast.getOp().getSyntax() + " " + rightType.getName());
                    }
                    return put(BoolType, compare(left, ast.getOp(), right));
                default:
                    throw new AnalysisException("Unknown op " + ast.getOp());
            }
        }

        @Override
        public TypedExpression visit(AstUpcast ast, Void a) {
            TypedExpression $ast = ast.getBase().accept(this, a);
            AstClafer fromType = $ast.getType();
            AstSetExpression from = (AstSetExpression) $ast.getExpression();
            AstAbstractClafer to = ast.getTarget();
            if (!AnalysisUtil.isAssignable(fromType, to)) {
                throw new AnalysisException("Cannot upcast from " + fromType + " to " + to);
            }
            return put(to, upcast(from, to));
        }

        @Override
        public TypedExpression visit(AstNone ast, Void a) {
            TypedExpression $set = ast.getSet().accept(this, a);
            return put(BoolType, none((AstSetExpression) $set.getExpression()));
        }

        @Override
        public TypedExpression visit(AstLocal ast, Void a) {
            return put(AnalysisUtil.notNull(ast + " type not analyzed yet", types.get(ast)), ast);
        }

        @Override
        public TypedExpression visit(AstQuantify ast, Void a) {
            List<AstDecl> $decl = new ArrayList<AstDecl>();
            for (AstDecl decl : ast.getDecls()) {
                TypedExpression $body = decl.getBody().accept(this, a);
                for (AstLocal local : decl.getLocals()) {
                    put($body.getType(), local);
                }
                $decl.add(decl.withBody((AstSetExpression) $body.getExpression()));
            }

            TypedExpression $body = ast.getBody().accept(this, a);
            return put(BoolType, quantify(
                    ast.getQuantifier(),
                    $decl.toArray(new AstDecl[$decl.size()]),
                    (AstBoolExpression) $body.getExpression()));
        }
    }

    private static class TypedExpression {

        private final AstClafer type;
        private final AstExpression expression;

        public TypedExpression(AstClafer type, AstExpression expression) {
            this.type = Check.notNull(type);
            this.expression = Check.notNull(expression);
        }

        public AstClafer getType() {
            return type;
        }

        public AstExpression getExpression() {
            return expression;
        }
    }
}
