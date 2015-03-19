package org.clafer.ast.analysis;

import org.clafer.ast.ProductType;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import org.clafer.ast.AstArithm;
import org.clafer.ast.AstBoolArithm;
import org.clafer.ast.AstBoolExpr;
import org.clafer.ast.AstCard;
import org.clafer.ast.AstChildRelation;
import org.clafer.ast.AstClafer;
import org.clafer.ast.AstCompare;
import org.clafer.ast.AstConcat;
import org.clafer.ast.AstConcreteClafer;
import org.clafer.ast.AstConstant;
import org.clafer.ast.AstConstraint;
import org.clafer.ast.AstDecl;
import org.clafer.ast.AstDifference;
import org.clafer.ast.AstDowncast;
import org.clafer.ast.AstExpr;
import org.clafer.ast.AstExprVisitor;
import org.clafer.ast.AstGlobal;
import org.clafer.ast.AstIfThenElse;
import org.clafer.ast.AstIntClafer;
import org.clafer.ast.AstIntersection;
import org.clafer.ast.AstInverse;
import org.clafer.ast.AstJoin;
import org.clafer.ast.AstJoinParent;
import org.clafer.ast.AstJoinRef;
import org.clafer.ast.AstLength;
import org.clafer.ast.AstLocal;
import org.clafer.ast.AstMembership;
import org.clafer.ast.AstMinus;
import org.clafer.ast.AstMod;
import org.clafer.ast.AstNot;
import org.clafer.ast.AstParentRelation;
import org.clafer.ast.AstPrefix;
import org.clafer.ast.AstProduct;
import org.clafer.ast.AstQuantify;
import org.clafer.ast.AstRef;
import org.clafer.ast.AstRefRelation;
import org.clafer.ast.AstSetExpr;
import org.clafer.ast.AstSetTest;
import org.clafer.ast.AstStringClafer;
import org.clafer.ast.AstStringConstant;
import org.clafer.ast.AstSuffix;
import org.clafer.ast.AstSum;
import org.clafer.ast.AstTernary;
import org.clafer.ast.AstThis;
import org.clafer.ast.AstTransitiveClosure;
import org.clafer.ast.AstUnion;
import org.clafer.ast.AstUpcast;
import org.clafer.ast.AstUtil;
import static org.clafer.ast.Asts.*;
import org.clafer.common.Check;
import org.clafer.common.Util;
import org.clafer.objective.Objective;

/**
 * <p>
 * Type checks and creates explicit upcast nodes in the AST. When the
 * expressions are rewritten, the types need to be reanalyzed.
 * </p>
 * <pre>
 * abstract A
 *     a
 * abstract B : A
 *     b
 * C : B
 *     c
 * D : B
 *     d
 * </pre>
 * <p>
 * A lowest common supertype in this solver directly corresponds to how the
 * expression is stored as a set. For example, suppose there is an expression
 * that evaluates to {C0, D0}. Unfortunately, C0 is stored as 0 for the type C
 * and D0 is also stored as 0 for the type D. The way the solver does it is to
 * upcast both C0 and D0 to the type B, where C0 = B0 and D0 = B1 so the set
 * {C0, D0} can be stored as {B0, B1}, ie. {0, 1} in Choco.
 * {@link Type#getCommonSupertype()} of each expression is the type used for
 * representing the set in Choco.
 * </p>
 * <p>
 * Even though the expressions are stored as the common supertype, it is
 * sometimes necessary to reconvert it back to the subtype. For example,
 * consider the expression {@code (C ++ D).d}. {@code (C ++ D)} has the union
 * type {C, D} but is stored as the supertype B. The join {@code .d} is allowed
 * because D is in the union type of {@code (C ++ D)}, but since it is stored as
 * a set of B's, the set first needs to be downcasted to a set of D's before the
 * join can be performed. If the expression were {@code (C ++ D).a}, then the
 * set of B's would need to be upcasted to a set of A's. If the expression were
 * {@code (C ++ D).b} then no casting is required.
 * </p>
 *
 * @author jimmy
 */
public class TypeAnalyzer implements Analyzer {

    @Override
    public Analysis analyze(Analysis analysis) {
        Map<AstExpr, Type> typeMap = new HashMap<>();
        Map<AstConstraint, AstBoolExpr> typedConstraints = new HashMap<>();
        for (AstConstraint constraint : analysis.getConstraints()) {
            AstClafer clafer = constraint.getContext();
            TypeVisitor visitor = new TypeVisitor(Type.basicType(clafer), typeMap);
            TypedExpr<AstBoolExpr> typedConstraint = visitor.typeCheck(analysis.getExpr(constraint));
            typedConstraints.put(constraint, typedConstraint.getExpr());
        }
        Map<Objective, AstSetExpr> objectives = analysis.getObjectiveExprs();
        Map<Objective, AstSetExpr> typedObjectives = new HashMap<>(objectives.size());
        for (Entry<Objective, AstSetExpr> objective : objectives.entrySet()) {
            TypeVisitor visitor = new TypeVisitor(Type.basicType(analysis.getModel()), typeMap);
            TypedExpr<AstSetExpr> typedObjective = visitor.typeCheck(objective.getValue());
            if (!typedObjective.getCommonSupertype().isInt()) {
                throw new TypeException("Cannot optimize on " + typedObjective.getType());
            }
            typedObjectives.put(objective.getKey(), typedObjective.getExpr());
        }
        return analysis.setTypeMap(typeMap)
                .setConstraintExprs(typedConstraints)
                .setObjectiveExprs(typedObjectives);
    }

    private static class TypeVisitor implements AstExprVisitor<Void, TypedExpr<?>> {

        private final Type context;
        private final Map<AstExpr, Type> typeMap;

        TypeVisitor(Type context, Map<AstExpr, Type> typeMap) {
            this.context = context;
            this.typeMap = typeMap;
        }

        private <T extends AstExpr> TypedExpr<T> typeCheck(T expr) {
            @SuppressWarnings("unchecked")
            TypedExpr<T> typedExpr = (TypedExpr<T>) expr.accept(this, null);
            return typedExpr;
        }

        private <T extends AstExpr> TypedExpr<T>[] typeCheck(T[] exprs) {
            @SuppressWarnings("unchecked")
            TypedExpr<T>[] typeChecked = (TypedExpr<T>[]) new TypedExpr<?>[exprs.length];
            for (int i = 0; i < exprs.length; i++) {
                typeChecked[i] = typeCheck(exprs[i]);
            }
            return typeChecked;
        }

        /**
         * Multilevel cast.
         *
         * @param expr the expression
         * @param target the target type
         * @return the same expression but with the target type
         */
        private AstSetExpr castTo(TypedExpr<AstSetExpr> expr, ProductType target) {
            if (isAssignable(expr.getCommonSupertype(), target)) {
                return upcastTo(expr, target);
            } else if (isAnyAssignable(expr.getUnionType(), target)) {
                return downcastTo(expr, target);
            }
            throw new TypeException("Cannot cast " + expr.getType() + " to " + target);
        }

        /**
         * Multilevel cast.
         *
         * @param expr the expression
         * @param target the target type
         * @return the same expression but with the target type
         */
        private AstSetExpr castTo(TypedExpr<AstSetExpr> expr, AstClafer target) {
            return castTo(expr, new ProductType(target));
        }

        /**
         * Multilevel downcast.
         *
         * @param expr the expression
         * @param target the target type
         * @return the same expression but with the target type
         */
        private AstSetExpr downcastTo(TypedExpr<AstSetExpr> expr, ProductType target) {
            if (expr.getType().getCommonSupertype().equals(target)) {
                return expr.getExpr();
            }
            if (isAnyAssignable(target, expr.getUnionType())) {
                AstSetExpr subExpr = downcast(expr.getExpr(), target);
                put(new Type(target), subExpr);
                return subExpr;
            }
            throw new TypeException("Cannot downcast " + expr.getType() + " to " + target);
        }

        /**
         * Multilevel upcast.
         *
         * @param expr the expression
         * @param target the target type
         * @return the same expression but with the target type
         */
        private AstSetExpr upcastTo(TypedExpr<AstSetExpr> expr, ProductType target) {
            if (expr.getType().getCommonSupertype().equals(target)) {
                return expr.getExpr();
            }
            if (isAssignable(expr.getCommonSupertype(), target)) {
                AstSetExpr superExpr = upcast(expr.getExpr(), target);
                put(new Type(target), superExpr);
                return superExpr;
            }
            throw new TypeException("Cannot upcast " + expr.getType() + " to " + target);
        }

        private <T extends AstExpr> TypedExpr<T> put(AstClafer basicType, T expr) {
            return put(Type.basicType(basicType), expr);
        }

        private <T extends AstExpr> TypedExpr<T> put(AstClafer paramType, AstClafer returnType, T expr) {
            return put(new Type(new ProductType(paramType, returnType)), expr);
        }

        private <T extends AstExpr> TypedExpr<T> put(ProductType type, T expr) {
            return put(new Type(type), expr);
        }

        private <T extends AstExpr> TypedExpr<T> put(Type type, T expr) {
            typeMap.put(expr, type);
            return new TypedExpr<>(type, expr);
        }

        @Override
        public TypedExpr<AstThis> visit(AstThis ast, Void a) {
            return put(context, ast);
        }

        @Override
        public TypedExpr<AstGlobal> visit(AstGlobal ast, Void a) {
            return put(Type.basicType(ast.getType()), ast);
        }

        @Override
        public TypedExpr<AstConstant> visit(AstConstant ast, Void a) {
            return put(new Type(ast.getType()), ast);
        }

        @Override
        public TypedExpr<?> visit(AstStringConstant ast, Void a) {
            return put(Type.basicType(AstStringClafer.Singleton), ast);
        }

        @Override
        public TypedExpr<AstSetExpr> visit(AstJoin ast, Void a) {
            TypedExpr<AstSetExpr> left = typeCheck(ast.getLeft());
            TypedExpr<AstSetExpr> right = typeCheck(ast.getRight());
            if (right.getCommonSupertype().arity() == 2) {
                AstClafer rightParamType = right.getCommonSupertype().get(0);
                AstClafer rightReturnType = right.getCommonSupertype().get(1);
                switch (left.getType().getCommonSupertype().arity()) {
                    case 1:
                        if (isAnyAssignable(left.getType().getUnionType(), rightParamType)) {
                            return put(rightReturnType, join(castTo(left, rightParamType), right.getExpr()));
                        }
                        break;
                    case 2:
                        AstClafer leftParamType = left.getCommonSupertype().get(0);
                        AstClafer leftReturnType = left.getCommonSupertype().get(1);
                        if (leftReturnType.equals(rightParamType)) {
                            return put(leftParamType, rightReturnType, join(left.getExpr(), right.getExpr()));
                        }
                        break;
                }
            }
            throw new TypeException("Cannot join " + left.getType() + " . " + right.getType());
        }

        @Override
        public TypedExpr<AstSetExpr> visit(AstJoinParent ast, Void a) {
            TypedExpr<AstSetExpr> children = typeCheck(ast.getChildren());
            if (children.getType().isClaferType()) {
                AstClafer childrenType = children.getType().getClaferType();
                if (childrenType instanceof AstConcreteClafer) {
                    AstConcreteClafer concreteChildrenType = (AstConcreteClafer) childrenType;
                    if (!AstUtil.isTop(concreteChildrenType)) {
                        return put(concreteChildrenType.getParent(),
                                joinParent(children.getExpr()));
                    }
                }
            }
            throw new TypeException("Cannot join " + children.getType() + " . parent");
        }

        @Override
        public TypedExpr<AstSetExpr> visit(AstJoinRef ast, Void a) {
            TypedExpr<AstSetExpr> deref = typeCheck(ast.getDeref());

            Set<AstRef> refs = new HashSet<>();
            for (ProductType type : deref.getUnionType()) {
                if (!type.isClaferType()) {
                    throw new TypeException("Cannot join " + deref.getType() + " . ref");
                }
                AstRef ref = AstUtil.getInheritedRef(type.getClaferType());
                if (ref != null) {
                    refs.add(ref);
                }
            }
            switch (refs.size()) {
                case 0:
                    throw new TypeException("Cannot join " + deref.getType() + " . ref");
                case 1:
                    AstRef ref = refs.iterator().next();
                    return put(ref.getTargetType(), joinRef(castTo(deref, ref.getSourceType())));
                default:
                    throw new TypeException("Ambiguous join " + deref.getType() + " . ref");
            }
        }

        @Override
        public TypedExpr<AstBoolExpr> visit(AstNot ast, Void a) {
            TypedExpr<AstBoolExpr> expr = typeCheck(ast.getExpr());
            return put(BoolType, not(expr.getExpr()));
        }

        @Override
        public TypedExpr<AstSetExpr> visit(AstMinus ast, Void a) {
            TypedExpr<AstSetExpr> expr = typeCheck(ast.getExpr());
            if (expr.getType().isInt()) {
                return put(IntType, minus(expr.getExpr()));
            }
            throw new TypeException("Cannot -" + expr.getType());
        }

        @Override
        public TypedExpr<AstSetExpr> visit(AstCard ast, Void a) {
            TypedExpr<AstSetExpr> set = typeCheck(ast.getSet());
            if (set.getType().isPrimitive()) {
                throw new TypeException("Cannot |" + set.getType() + "|");
            }
            return put(IntType, card(set.getExpr()));
        }

        @Override
        public TypedExpr<AstBoolExpr> visit(AstSetTest ast, Void a) {
            TypedExpr<AstSetExpr> left = typeCheck(ast.getLeft());
            TypedExpr<AstSetExpr> right = typeCheck(ast.getRight());

            if (isDisjoint(left.getType(), right.getType())) {
                throw new TypeException("Cannot " + left.getType() + " "
                        + ast.getOp().getSyntax() + " " + right.getType());
            }

            ProductType commonType = getLowestCommonSupertype(left.getCommonSupertype(), right.getCommonSupertype());
            return put(BoolType, test(upcastTo(left, commonType), ast.getOp(), upcastTo(right, commonType)));
        }

        @Override
        public TypedExpr<AstBoolExpr> visit(AstCompare ast, Void a) {
            TypedExpr<AstSetExpr> left = typeCheck(ast.getLeft());
            TypedExpr<AstSetExpr> right = typeCheck(ast.getRight());
            if (left.getType().isInt() && right.getType().isInt()) {
                return put(BoolType, compare(left.getExpr(), ast.getOp(), right.getExpr()));
            }
            throw new TypeException("Cannot " + left.getType() + " "
                    + ast.getOp().getSyntax() + " " + right.getType());
        }

        @Override
        public TypedExpr<AstSetExpr> visit(AstArithm ast, Void a) {
            TypedExpr<AstSetExpr>[] operands = typeCheck(ast.getOperands());
            for (TypedExpr<AstSetExpr> operand : operands) {
                if (!operand.getType().isInt()) {
                    throw new TypeException("Cannot "
                            + Util.intercalate(" " + ast.getOp().getSyntax() + " ",
                                    getTypes(operands)));
                }
            }
            return put(IntType, arithm(ast.getOp(), getSetExprs(operands)));
        }

        @Override
        public TypedExpr<?> visit(AstMod ast, Void a) {
            TypedExpr<AstSetExpr> dividend = typeCheck(ast.getDividend());
            TypedExpr<AstSetExpr> divisor = typeCheck(ast.getDivisor());
            if (dividend.getType().isInt() && divisor.getType().isInt()) {
                return put(IntType, mod(dividend.getExpr(), divisor.getExpr()));
            }
            throw new TypeException("Cannot " + dividend.getType() + " % " + divisor.getType());
        }

        @Override
        public TypedExpr<AstSetExpr> visit(AstSum ast, Void a) {
            TypedExpr<AstSetExpr> set = typeCheck(ast.getSet());

            Set<AstRef> refs = new HashSet<>();
            for (ProductType product : set.getType()) {
                if (!product.isClaferType()) {
                    throw new TypeException("Cannot sum(" + set.getType() + ")");
                }
                AstRef ref = AstUtil.getInheritedRef(product.getClaferType());
                if (ref != null) {
                    refs.add(ref);
                }
            }
            switch (refs.size()) {
                case 0:
                    throw new TypeException("Cannot sum(" + set.getType() + ")");
                case 1:
                    AstRef ref = refs.iterator().next();
                    if (ref.getTargetType() instanceof AstIntClafer) {
                        return put(ref.getTargetType(), sum(castTo(set, ref.getSourceType())));
                    }
                    throw new TypeException("Cannot sum(" + set.getType() + ")");
                default:
                    throw new TypeException("Ambiguous sum(" + set.getType() + ")");
            }
        }

        @Override
        public TypedExpr<AstSetExpr> visit(AstProduct ast, Void a) {
            TypedExpr<AstSetExpr> set = typeCheck(ast.getSet());

            Set<AstRef> refs = new HashSet<>();
            for (ProductType product : set.getType()) {
                if (!product.isClaferType()) {
                    throw new TypeException("Cannot product(" + set.getType() + ")");
                }
                AstRef ref = AstUtil.getInheritedRef(product.getClaferType());
                if (ref != null) {
                    refs.add(ref);
                }
            }
            switch (refs.size()) {
                case 0:
                    throw new TypeException("Cannot product(" + set.getType() + ")");
                case 1:
                    AstRef ref = refs.iterator().next();
                    if (ref.getTargetType() instanceof AstIntClafer) {
                        return put(ref.getTargetType(), product(castTo(set, ref.getSourceType())));
                    }
                    throw new TypeException("Cannot product(" + set.getType() + ")");
                default:
                    throw new TypeException("Ambiguous product(" + set.getType() + ")");
            }
        }

        @Override
        public TypedExpr<?> visit(AstBoolArithm ast, Void a) {
            TypedExpr<AstBoolExpr>[] operands = typeCheck(ast.getOperands());
            return put(BoolType, arithm(ast.getOp(), getBoolExprs(operands)));
        }

        @Override
        public TypedExpr<?> visit(AstDifference ast, Void a) {
            TypedExpr<AstSetExpr> left = typeCheck(ast.getLeft());
            TypedExpr<AstSetExpr> right = typeCheck(ast.getRight());

            if (!isAssignable(right.getType(), left.getType())) {
                throw new TypeException("Cannot " + left.getType() + " -- " + right.getType());
            }

            // TODO: check for primitives
            return put(left.getType(), diff(
                    left.getExpr(),
                    castTo(right, left.getCommonSupertype())));
        }

        @Override
        public TypedExpr<?> visit(AstIntersection ast, Void a) {
            TypedExpr<AstSetExpr> left = typeCheck(ast.getLeft());
            TypedExpr<AstSetExpr> right = typeCheck(ast.getRight());

            Type intersectionType = intersectionType(left.getType(), right.getType());

            // TODO: check for primitives
            return put(intersectionType, inter(
                    downcastTo(left, intersectionType.getCommonSupertype()),
                    downcastTo(right, intersectionType.getCommonSupertype())));
        }

        @Override
        public TypedExpr<?> visit(AstUnion ast, Void a) {
            TypedExpr<AstSetExpr> left = typeCheck(ast.getLeft());
            TypedExpr<AstSetExpr> right = typeCheck(ast.getRight());

            Type unionType = unionType(left.getType(), right.getType());

            if (unionType.getCommonSupertype() == null) {
                throw new TypeException("Cannot " + left.getType() + " ++ " + right.getType());
            }
            return put(unionType, union(
                    upcastTo(left, unionType.getCommonSupertype()),
                    upcastTo(right, unionType.getCommonSupertype())));
        }

        @Override
        public TypedExpr<AstBoolExpr> visit(AstMembership ast, Void a) {
            TypedExpr<AstSetExpr> member = typeCheck(ast.getMember());
            TypedExpr<AstSetExpr> set = typeCheck(ast.getSet());

            if (isDisjoint(member.getType(), set.getType())) {
                throw new TypeException("Cannot " + member.getType()
                        + " " + ast.getOp().getSyntax() + " " + set.getType());
            }

            ProductType commonType = getLowestCommonSupertype(member.getCommonSupertype(), set.getCommonSupertype());
            return put(BoolType, membership(upcastTo(member, commonType), ast.getOp(), upcastTo(set, commonType)));
        }

        @Override
        public TypedExpr<AstSetExpr> visit(AstTernary ast, Void a) {
            TypedExpr<AstBoolExpr> antecedent = typeCheck(ast.getAntecedent());
            TypedExpr<AstSetExpr> alternative = typeCheck(ast.getAlternative());
            TypedExpr<AstSetExpr> consequent = typeCheck(ast.getConsequent());
            ProductType unionType = getLowestCommonSupertype(alternative.getCommonSupertype(), consequent.getCommonSupertype());
            if (unionType == null) {
                throw new TypeException("Cannot if " + antecedent.getType() + " then "
                        + consequent.getType() + " else " + alternative.getType());
            }
            return put(new Type(unionType), ifThenElse(antecedent.getExpr(),
                    upcastTo(consequent, unionType), upcastTo(alternative, unionType)));
        }

        @Override
        public TypedExpr<AstBoolExpr> visit(AstIfThenElse ast, Void a) {
            TypedExpr<AstBoolExpr> antecedent = typeCheck(ast.getAntecedent());
            TypedExpr<AstBoolExpr> alternative = typeCheck(ast.getAlternative());
            TypedExpr<AstBoolExpr> consequent = typeCheck(ast.getConsequent());
            return put(BoolType, ifThenElse(antecedent.getExpr(), alternative.getExpr(), consequent.getExpr()));
        }

        @Override
        public TypedExpr<?> visit(AstDowncast ast, Void a) {
            TypedExpr<AstSetExpr> base = typeCheck(ast.getBase());
            ProductType to = ast.getTarget();
            if (isAnyAssignable(to, base.getUnionType())) {
                return put(new Type(to), downcast(base.getExpr(), to));
            }
            throw new TypeException("Cannot downcast from " + base.getType() + " to " + to);
        }

        @Override
        public TypedExpr<AstSetExpr> visit(AstUpcast ast, Void a) {
            TypedExpr<AstSetExpr> base = typeCheck(ast.getBase());
            ProductType to = ast.getTarget();
            if (isAssignable(base.getCommonSupertype(), to)) {
                return put(new Type(base.getUnionType(), to), upcast(base.getExpr(), to));
            }
            throw new TypeException("Cannot upcast from " + base.getType() + " to " + to);
        }

        @Override
        public TypedExpr<AstLocal> visit(AstLocal ast, Void a) {
            Type localType = typeMap.get(ast);
            if (localType == null) {
                throw new AnalysisException(ast + " type not analyzed yet.");
            }
            return put(localType, ast);
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
                decls[i] = decl(decl.isDisjoint(), decl.getLocals(), body.getExpr());
            }
            TypedExpr<AstBoolExpr> body = typeCheck(ast.getBody());
            return put(BoolType, quantify(ast.getQuantifier(), decls, body.getExpr()));
        }

        @Override
        public TypedExpr<?> visit(AstLength ast, Void a) {
            TypedExpr<AstSetExpr> string = typeCheck(ast.getString());
            if (string.getType().isString()) {
                return put(IntType, length(ast.getString()));
            }
            throw new TypeException("Cannot length(" + string.getType() + ")");
        }

        @Override
        public TypedExpr<?> visit(AstConcat ast, Void a) {
            TypedExpr<AstSetExpr> left = typeCheck(ast.getLeft());
            TypedExpr<AstSetExpr> right = typeCheck(ast.getRight());
            if (left.getType().isString() && right.getType().isString()) {
                return put(StringType, concat(left.getExpr(), right.getExpr()));
            }
            throw new TypeException("Cannot " + left.getType() + " ++ " + right.getType());
        }

        @Override
        public TypedExpr<?> visit(AstPrefix ast, Void a) {
            TypedExpr<AstSetExpr> prefix = typeCheck(ast.getPrefix());
            TypedExpr<AstSetExpr> word = typeCheck(ast.getWord());
            if (prefix.getType().isString() && word.getType().isString()) {
                return put(BoolType, prefix(prefix.getExpr(), word.getExpr()));
            }
            throw new TypeException("Cannot " + prefix.getType() + " prefix " + word.getType());
        }

        @Override
        public TypedExpr<?> visit(AstSuffix ast, Void a) {
            TypedExpr<AstSetExpr> suffix = typeCheck(ast.getSuffix());
            TypedExpr<AstSetExpr> word = typeCheck(ast.getWord());
            if (suffix.getType().isString() && word.getType().isString()) {
                return put(BoolType, suffix(suffix.getExpr(), word.getExpr()));
            }
            throw new TypeException("Cannot " + suffix.getType() + " suffix " + word.getType());
        }

        @Override
        public TypedExpr<?> visit(AstChildRelation ast, Void a) {
            AstConcreteClafer child = ast.getChildType();
            if (!child.hasParent()) {
                throw new TypeException(child + " does not have a parent");
            }
            return put(child.getParent(), child, ast);
        }

        @Override
        public TypedExpr<?> visit(AstParentRelation ast, Void a) {
            AstConcreteClafer child = ast.getParentRelation();
            if (!child.hasParent()) {
                throw new TypeException(child + " does not have a parent");
            }
            return put(child, child.getParent(), ast);
        }

        @Override
        public TypedExpr<?> visit(AstRefRelation ast, Void a) {
            AstRef ref = ast.getRef();
            return put(ref.getSourceType(), ref.getTargetType(), ast);
        }

        @Override
        public TypedExpr<?> visit(AstInverse ast, Void a) {
            TypedExpr<AstSetExpr> relation = typeCheck(ast.getRelation());
            if (relation.getCommonSupertype().arity() == 2) {
                AstClafer paramType = relation.getCommonSupertype().get(0);
                AstClafer returnType = relation.getCommonSupertype().get(1);
                return put(returnType, paramType, inverse(relation.getExpr()));
            }
            throw new TypeException(relation + " cannot be inversed");
        }

        @Override
        public TypedExpr<?> visit(AstTransitiveClosure ast, Void a) {
            TypedExpr<AstSetExpr> relation = typeCheck(ast.getRelation());
            if (relation.getCommonSupertype().arity() == 2) {
                if (relation.getCommonSupertype().get(0).equals(relation.getCommonSupertype().get(1))) {
                    return put(relation.getType(), transitiveClosure(relation.getExpr(), ast.isReflexive()));
                }
            }
            throw new TypeException(relation + " cannot be transitively closed");
        }
    }

    private static Type[] getTypes(TypedExpr<?>... exprs) {
        Type[] types = new Type[exprs.length];
        for (int i = 0; i < types.length; i++) {
            types[i] = exprs[i].getType();
        }
        return types;
    }

    private static <T extends AstBoolExpr> AstBoolExpr[] getBoolExprs(TypedExpr<T>[] exprs) {
        AstBoolExpr[] boolExprs = new AstBoolExpr[exprs.length];
        for (int i = 0; i < boolExprs.length; i++) {
            boolExprs[i] = exprs[i].getExpr();
        }
        return boolExprs;
    }

    private static <T extends AstSetExpr> AstSetExpr[] getSetExprs(TypedExpr<T>[] exprs) {
        AstSetExpr[] setExprs = new AstSetExpr[exprs.length];
        for (int i = 0; i < setExprs.length; i++) {
            setExprs[i] = exprs[i].getExpr();
        }
        return setExprs;
    }

    private static boolean isAssignable(Type from, Type to) {
        for (ProductType left : from) {
            if (isAnyAssignable(left, to)) {
                return true;
            }
        }
        return false;
    }

    private static boolean isAssignable(ProductType from, ProductType to) {
        AstClafer[] toProduct = to.getProduct();
        AstClafer[] fromProduct = from.getProduct();
        if (fromProduct.length != toProduct.length) {
            return false;
        }
        for (int i = 0; i < fromProduct.length; i++) {
            if (!AstUtil.isAssignable(fromProduct[i], toProduct[i])) {
                return false;
            }
        }
        return true;
    }

    private static boolean isAnyAssignable(Iterable<ProductType> froms, ProductType to) {
        for (ProductType from : froms) {
            if (isAssignable(from, to)) {
                return true;
            }
        }
        return false;
    }

    private static boolean isAnyAssignable(ProductType from, Iterable<ProductType> tos) {
        for (ProductType to : tos) {
            if (isAssignable(from, to)) {
                return true;
            }
        }
        return false;
    }

    private static boolean isAnyAssignable(Iterable<ProductType> froms, AstClafer to) {
        for (ProductType from : froms) {
            if (from.arity() == 1 && AstUtil.isAssignable(from.getProduct()[0], to)) {
                return true;
            }
        }
        return false;
    }

//    private static boolean isAnyAssignable(Iterable<AstClafer> froms, AstClafer to) {
//        for (AstClafer from : froms) {
//            if (AstUtil.isAssignable(from, to)) {
//                return true;
//            }
//        }
//        return false;
//    }
    private static boolean isAnyAssignable(AstClafer from, Iterable<AstClafer> tos) {
        for (AstClafer to : tos) {
            if (AstUtil.isAssignable(from, to)) {
                return true;
            }
        }
        return false;
    }

    private static ProductType getLowestCommonSupertype(ProductType t1, ProductType t2) {
        if (t1.arity() != t2.arity()) {
            throw new IllegalArgumentException();
        }
        AstClafer[] product = new AstClafer[t1.arity()];
        for (int i = 0; i < product.length; i++) {
            product[i] = AstUtil.getLowestCommonSupertype(t1.get(i), t2.get(i));
        }
        return new ProductType(product);
    }

    private static boolean isDisjoint(Type t1, Type t2) {
        for (ProductType leftType : t1) {
            if (isAnyAssignable(leftType, t2.getUnionType())) {
                return false;
            }
        }
        for (ProductType rightType : t2) {
            if (isAnyAssignable(rightType, t1.getUnionType())) {
                return false;
            }
        }
        return true;
    }

    private static Type intersectionType(Type t1, Type t2) {
        Set<ProductType> interType = new HashSet<>();
        for (ProductType leftType : t1) {
            if (isAnyAssignable(leftType, t2.getUnionType())) {
                interType.add(leftType);
            }
        }
        for (ProductType rightType : t2) {
            if (isAnyAssignable(rightType, t1.getUnionType())) {
                interType.add(rightType);
            }
        }
        return new Type(interType);
    }

    private static Type unionType(Type t1, Type t2) {
        Set<ProductType> unionType = new HashSet<>();
        for (ProductType leftType : t1) {
            unionType.add(leftType);
        }
        for (ProductType rightType : t2) {
            unionType.add(rightType);
        }
        return new Type(unionType);
    }

    private static class TypedExpr<T extends AstExpr> {

        private final Type type;
        private final T expr;

        TypedExpr(Type type, T expr) {
            this.type = Check.notNull(type);
            this.expr = Check.notNull(expr);
        }

        public Type getType() {
            return type;
        }

        public Set<ProductType> getUnionType() {
            return type.getUnionType();
        }

        public ProductType getCommonSupertype() {
            return type.getCommonSupertype();
        }

        public T getExpr() {
            return expr;
        }
    }
}
