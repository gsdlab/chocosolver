package org.clafer.ast.analysis;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import org.clafer.common.Check;
import org.clafer.ast.AstSetTest;
import org.clafer.ast.AstGlobal;
import static org.clafer.ast.Asts.*;
import org.clafer.ast.AstAbstractClafer;
import org.clafer.ast.AstArithm;
import org.clafer.ast.AstBoolArithm;
import org.clafer.ast.AstBoolExpr;
import org.clafer.ast.AstCard;
import org.clafer.ast.AstClafer;
import org.clafer.ast.AstCompare;
import org.clafer.ast.AstConcreteClafer;
import org.clafer.ast.AstConstant;
import org.clafer.ast.AstConstraint;
import org.clafer.ast.AstDecl;
import org.clafer.ast.AstDifference;
import org.clafer.ast.AstDowncast;
import org.clafer.ast.AstExpr;
import org.clafer.ast.AstExprVisitor;
import org.clafer.ast.AstIfThenElse;
import org.clafer.ast.AstIntClafer;
import org.clafer.ast.AstIntersection;
import org.clafer.ast.AstJoin;
import org.clafer.ast.AstJoinParent;
import org.clafer.ast.AstJoinRef;
import org.clafer.ast.AstLocal;
import org.clafer.ast.AstMembership;
import org.clafer.ast.AstMinus;
import org.clafer.ast.AstNot;
import org.clafer.ast.AstPrimClafer;
import org.clafer.ast.AstQuantify;
import org.clafer.ast.AstRef;
import org.clafer.ast.AstSetExpr;
import org.clafer.ast.AstSum;
import org.clafer.ast.AstTernary;
import org.clafer.ast.AstThis;
import org.clafer.ast.AstUnion;
import org.clafer.ast.AstUpcast;
import org.clafer.ast.AstUtil;
import org.clafer.common.Util;
import org.clafer.objective.Objective;

/**
 * <p>
 * Type checks and creates explicit upcast nodes in the AST. When the
 * expressions are rewritten, the types need to be reanalyzed.
 * </p>
 * <p>
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
 * </p>
 * <p>
 * A lowest common supertype in this solver directly corresponds to how the
 * expression is stored as a set. For example, suppose there is an expression
 * that evaluates to {C0, D0}. Unfortunately, C0 is stored as 0 for the type C
 * and D0 is also stored as 0 for the type D. The way the solver does it is to
 * upcast both C0 and D0 to the type B, where C0 = B0 and D0 = B1 so the set
 * {C0, D0} can be stored as {B0, B1}, ie. {0, 1} in Choco.
 * {@link Type#getCommonSuperType()} of each expression is the type used for
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
        Map<AstExpr, Type> typeMap = new HashMap<AstExpr, Type>();
        List<AstConstraint> typedConstraints = new ArrayList<AstConstraint>();
        List<Objective> typedObjectives = new ArrayList<Objective>();
        for (AstConstraint constraint : analysis.getConstraints()) {
            AstClafer clafer = constraint.getContext();
            TypeVisitor visitor = new TypeVisitor(Type.basicType(clafer), typeMap);
            TypedExpr<AstBoolExpr> typedConstraint = visitor.typeCheck(constraint.getExpr());
            typedConstraints.add(constraint.withExpr(typedConstraint.getExpr()));
        }
        for (Objective objective : analysis.getObjectives()) {
            TypeVisitor visitor = new TypeVisitor(Type.basicType(analysis.getModel()), typeMap);
            TypedExpr<AstSetExpr> typedObjective = visitor.typeCheck(objective.getExpr());
            typedObjectives.add(objective.withExpr(typedObjective.getExpr()));
        }
        return analysis.setTypeMap(typeMap)
                .setConstraints(typedConstraints)
                .setObjectives(typedObjectives);
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
            TypedExpr<T>[] typeChecked = new TypedExpr[exprs.length];
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
        private AstSetExpr castTo(TypedExpr<AstSetExpr> expr, AstClafer target) {
            int exprDepth = AstUtil.getSuperHierarchy(expr.getCommonSupertype()).size();
            int targetDepth = AstUtil.getSuperHierarchy(target).size();

            if (exprDepth < targetDepth) {
                return downcastTo(expr, target);
            } else if (exprDepth > targetDepth) {
                return upcastTo(expr, target);
            }
            return expr.getExpr();
        }

        private Collection<AstClafer> downcastTrail(AstClafer type, AstClafer target) {
            List<AstClafer> supers = new ArrayList<AstClafer>();
            AstClafer sup = target;
            while (sup != null) {
                if (sup.equals(type)) {
                    Collections.reverse(supers);
                    return supers;
                }
                supers.add(sup);
                sup = sup.getSuperClafer();
            }
            throw new TypeException("Cannot downcast " + type + " to " + target);
        }

        /**
         * Multilevel downcast.
         *
         * @param expr the expression
         * @param target the target type
         * @return the same expression but with the target type
         */
        private AstSetExpr downcastTo(TypedExpr<AstSetExpr> expr, AstClafer target) {
            Type exprType = expr.getType();
            if (exprType.getCommonSuperType().equals(target)) {
                return expr.getExpr();
            }
            AstSetExpr superExpr = expr.getExpr();
            for (AstClafer superType : downcastTrail(exprType.getCommonSuperType(), target)) {
                superExpr = downcast(superExpr, superType);
                put(Type.basicType(superType), superExpr);
            }
            return superExpr;
        }

        /**
         * Multilevel upcast.
         *
         * @param expr the expression
         * @param target the target type
         * @return the same expression but with the target type
         */
        private AstSetExpr upcastTo(TypedExpr<AstSetExpr> expr, AstClafer target) {
            Type exprType = expr.getType();
            if (exprType.getCommonSuperType().equals(target)) {
                return expr.getExpr();
            }
            AstSetExpr superExpr = expr.getExpr();
            List<AstAbstractClafer> superTypes = AstUtil.getSupers(exprType.getCommonSuperType());
            for (AstAbstractClafer superType : superTypes) {
                superExpr = upcast(superExpr, superType);
                put(Type.basicType(superType), superExpr);
                if (superType.equals(target)) {
                    return superExpr;
                }
            }
            throw new TypeException("Cannot upcast " + expr.getType() + " to " + target);
        }

        private <T extends AstExpr> TypedExpr<T> put(AstClafer basicType, T expr) {
            return put(Type.basicType(basicType), expr);
        }

        private <T extends AstExpr> TypedExpr<T> put(Type type, T expr) {
            typeMap.put(expr, type);
            return new TypedExpr<T>(type, expr);
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
            return put(Type.basicType(ast.getType()), ast);
        }

        @Override
        public TypedExpr<AstSetExpr> visit(AstJoin ast, Void a) {
            TypedExpr<AstSetExpr> left = typeCheck(ast.getLeft());
            AstConcreteClafer rightType = ast.getRight();
            if (rightType.hasParent()) {
                AstClafer joinType = rightType.getParent();
                if (isAnyAssignable(left.getType().getUnionType(), joinType)) {
                    return put(rightType, join(castTo(left, joinType), rightType));
                }
            }
            throw new TypeException("Cannot join " + left.getType() + " . " + rightType);
        }

        @Override
        public TypedExpr<AstSetExpr> visit(AstJoinParent ast, Void a) {
            TypedExpr<AstSetExpr> children = typeCheck(ast.getChildren());
            if (children.getType().isBasicType()) {
                AstClafer childrenType = children.getType().getBasicType();
                if (childrenType instanceof AstConcreteClafer) {
                    AstConcreteClafer concreteChildrenType = (AstConcreteClafer) childrenType;
                    if (concreteChildrenType.hasParent()) {
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

            Set<AstRef> refs = new HashSet<AstRef>();
            for (AstClafer type : deref.getUnionType()) {
                AstRef ref = AstUtil.getInheritedRef(type);
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
            if (expr.getCommonSupertype() instanceof AstIntClafer) {
                return put(IntType, minus(expr.getExpr()));
            }
            throw new TypeException("Cannot -" + expr.getType());
        }

        @Override
        public TypedExpr<AstSetExpr> visit(AstCard ast, Void a) {
            TypedExpr<AstSetExpr> set = typeCheck(ast.getSet());
            if (set.getCommonSupertype() instanceof AstPrimClafer) {
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

            AstClafer commonType = AstUtil.getLowestCommonSupertype(left.getCommonSupertype(), right.getCommonSupertype());
            return put(BoolType, test(upcastTo(left, commonType), ast.getOp(), upcastTo(right, commonType)));
        }

        @Override
        public TypedExpr<AstBoolExpr> visit(AstCompare ast, Void a) {
            TypedExpr<AstSetExpr> left = typeCheck(ast.getLeft());
            TypedExpr<AstSetExpr> right = typeCheck(ast.getRight());
            if (left.getCommonSupertype() instanceof AstIntClafer
                    && right.getCommonSupertype() instanceof AstIntClafer) {
                return put(BoolType, compare(left.getExpr(), ast.getOp(), right.getExpr()));
            }
            throw new TypeException("Cannot " + left.getType() + " "
                    + ast.getOp().getSyntax() + " " + right.getType());
        }

        @Override
        public TypedExpr<AstSetExpr> visit(AstArithm ast, Void a) {
            TypedExpr<AstSetExpr>[] operands = typeCheck(ast.getOperands());
            for (TypedExpr<AstSetExpr> operand : operands) {
                if (!(operand.getCommonSupertype() instanceof AstIntClafer)) {
                    throw new TypeException("Cannot "
                            + Util.intercalate(" " + ast.getOp().getSyntax() + " ",
                            getTypes(operands)));
                }
            }
            return put(IntType, arithm(ast.getOp(), getSetExprs(operands)));
        }

        @Override
        public TypedExpr<AstSetExpr> visit(AstSum ast, Void a) {
            TypedExpr<AstSetExpr> set = typeCheck(ast.getSet());

            Set<AstRef> refs = new HashSet<AstRef>();
            for (AstClafer type : set.getUnionType()) {
                AstRef ref = AstUtil.getInheritedRef(type);
                if (ref != null) {
                    refs.add(ref);
                }
            }
            switch (refs.size()) {
                case 0:
                    throw new TypeException("Cannot sum(" + set.getType() + ")");
                case 1:
                    AstRef ref = refs.iterator().next();
                    return put(ref.getTargetType(), sum(castTo(set, ref.getSourceType())));
                default:
                    throw new TypeException("Ambiguous sum(" + set.getType() + ")");
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

            Set<AstClafer> unionType = new HashSet<AstClafer>();
            unionType.addAll(left.getUnionType());
            unionType.addAll(right.getUnionType());

            // TODO: check for primitives

            Type type = new Type(left.getUnionType(), AstUtil.getLowestCommonSupertype(unionType));

            return put(type, diff(
                    upcastTo(left, type.getCommonSuperType()),
                    upcastTo(right, type.getCommonSuperType())));
        }

        @Override
        public TypedExpr<?> visit(AstIntersection ast, Void a) {
            TypedExpr<AstSetExpr> left = typeCheck(ast.getLeft());
            TypedExpr<AstSetExpr> right = typeCheck(ast.getRight());

            Set<AstClafer> unionType = new HashSet<AstClafer>();
            unionType.addAll(left.getUnionType());
            unionType.addAll(right.getUnionType());

            Set<AstClafer> intersectionType = intersectionType(left.getType(), right.getType());

            // TODO: check for primitives

            Type type = new Type(intersectionType, AstUtil.getLowestCommonSupertype(unionType));

            return put(type, inter(
                    upcastTo(left, type.getCommonSuperType()),
                    upcastTo(right, type.getCommonSuperType())));
        }

        @Override
        public TypedExpr<?> visit(AstUnion ast, Void a) {
            TypedExpr<AstSetExpr> left = typeCheck(ast.getLeft());
            TypedExpr<AstSetExpr> right = typeCheck(ast.getRight());

            Set<AstClafer> unionType = new HashSet<AstClafer>();
            unionType.addAll(left.getUnionType());
            unionType.addAll(right.getUnionType());

            // TODO: check for primitives

            Type type = new Type(unionType);

            if (type.getCommonSuperType() == null) {
                throw new TypeException("Cannot " + left.getType() + " ++ " + right.getType());
            }

            return put(type, union(
                    upcastTo(left, type.getCommonSuperType()),
                    upcastTo(right, type.getCommonSuperType())));
        }

        @Override
        public TypedExpr<AstBoolExpr> visit(AstMembership ast, Void a) {
            TypedExpr<AstSetExpr> member = typeCheck(ast.getMember());
            TypedExpr<AstSetExpr> set = typeCheck(ast.getSet());

            if (isDisjoint(member.getType(), set.getType())) {
                throw new TypeException("Cannot " + member.getType()
                        + " " + ast.getOp().getSyntax() + " " + set.getType());
            }

            AstClafer commonType = AstUtil.getLowestCommonSupertype(member.getCommonSupertype(), set.getCommonSupertype());
            return put(BoolType, membership(upcastTo(member, commonType), ast.getOp(), upcastTo(set, commonType)));
        }

        @Override
        public TypedExpr<AstSetExpr> visit(AstTernary ast, Void a) {
            TypedExpr<AstBoolExpr> antecedent = typeCheck(ast.getAntecedent());
            TypedExpr<AstSetExpr> alternative = typeCheck(ast.getAlternative());
            TypedExpr<AstSetExpr> consequent = typeCheck(ast.getConsequent());
            AstClafer unionType = AstUtil.getLowestCommonSupertype(alternative.getCommonSupertype(), consequent.getCommonSupertype());
            if (unionType == null) {
                throw new TypeException("Cannot if " + antecedent.getType() + " then "
                        + consequent.getType() + " else " + alternative.getType());
            }
            return put(unionType, ifThenElse(antecedent.getExpr(),
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
            AstClafer to = ast.getTarget();
            if (isAnyAssignable(base.getUnionType(), to)) {
                return put(to, downcast(base.getExpr(), ast.getTarget()));
            }
            throw new TypeException("Cannot downcast from " + base.getType() + " to " + to);
        }

        @Override
        public TypedExpr<AstSetExpr> visit(AstUpcast ast, Void a) {
            TypedExpr<AstSetExpr> base = typeCheck(ast.getBase());
            AstAbstractClafer to = ast.getTarget();
            if (AstUtil.isAssignable(base.getCommonSupertype(), to)) {
                return put(new Type(base.getUnionType(), to), upcast(base.getExpr(), ast.getTarget()));
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
    }

    private static Type[] getTypes(TypedExpr<?>... exprs) {
        Type[] types = new Type[exprs.length];
        for (int i = 0; i < types.length; i++) {
            types[i] = exprs[i].getType();
        }
        return types;
    }

    private static <T extends AstBoolExpr> AstBoolExpr[] getBoolExprs(TypedExpr<T>... exprs) {
        AstBoolExpr[] boolExprs = new AstBoolExpr[exprs.length];
        for (int i = 0; i < boolExprs.length; i++) {
            boolExprs[i] = exprs[i].getExpr();
        }
        return boolExprs;
    }

    private static <T extends AstSetExpr> AstSetExpr[] getSetExprs(TypedExpr<T>... exprs) {
        AstSetExpr[] setExprs = new AstSetExpr[exprs.length];
        for (int i = 0; i < setExprs.length; i++) {
            setExprs[i] = exprs[i].getExpr();
        }
        return setExprs;
    }

    private static boolean isAnyAssignable(Iterable<AstClafer> froms, AstClafer to) {
        for (AstClafer from : froms) {
            if (AstUtil.isAssignable(from, to)) {
                return true;
            }
        }
        return false;
    }

    private static boolean isAnyAssignable(AstClafer from, Iterable<AstClafer> tos) {
        for (AstClafer to : tos) {
            if (AstUtil.isAssignable(from, to)) {
                return true;
            }
        }
        return false;
    }

    private static boolean isDisjoint(Type t1, Type t2) {
        for (AstClafer leftType : t1.getUnionType()) {
            if (isAnyAssignable(leftType, t2.getUnionType())) {
                return false;
            }
        }
        for (AstClafer rightType : t2.getUnionType()) {
            if (isAnyAssignable(rightType, t1.getUnionType())) {
                return false;
            }
        }
        return true;
    }

    private static Set<AstClafer> intersectionType(Type t1, Type t2) {
        Set<AstClafer> interType = new HashSet<AstClafer>();
        for (AstClafer leftType : t1.getUnionType()) {
            if (isAnyAssignable(leftType, t2.getUnionType())) {
                interType.add(leftType);
            }
        }
        for (AstClafer rightType : t2.getUnionType()) {
            if (isAnyAssignable(rightType, t1.getUnionType())) {
                interType.add(rightType);
            }
        }
        return interType;
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

        public Set<AstClafer> getUnionType() {
            return type.getUnionType();
        }

        public AstClafer getCommonSupertype() {
            return type.getCommonSuperType();
        }

        public boolean isBasicType() {
            return type.isBasicType();
        }

        public AstClafer getBasicType() {
            return type.getBasicType();
        }

        public T getExpr() {
            return expr;
        }
    }
}
