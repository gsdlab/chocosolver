package org.clafer;

import org.clafer.ast.AstAbstractClafer;
import org.clafer.ast.AstConcreteClafer;
import org.clafer.ast.AstModel;
import static org.clafer.ast.Asts.*;
import org.clafer.ast.analysis.TypeException;
import org.clafer.compiler.ClaferCompiler;
import org.clafer.compiler.ClaferSolver;
import org.clafer.instance.InstanceClafer;
import org.clafer.scope.Scope;
import static org.junit.Assert.*;
import org.junit.Test;

/**
 *
 * @author jimmy
 */
public class TypingConstraintTest {

    /**
     * <pre>
     * abstract A
     *     a -> int
     * abstract B : A
     *     b -> int
     * C : B
     *     c -> int
     * D : B ?
     *     d -> int
     *
     * [(C ++ D).b.ref = 1]
     * </pre>
     */
    @Test(timeout = 60000)
    public void testNocastJoin() {
        AstModel model = newModel();

        AstAbstractClafer A = model.addAbstract("A");
        AstConcreteClafer a = A.addChild("a").withCard(Mandatory).refToUnique(IntType);
        AstAbstractClafer B = model.addAbstract("B").extending(A);
        AstConcreteClafer b = B.addChild("b").withCard(Mandatory).refToUnique(IntType);
        AstConcreteClafer C = model.addChild("C").extending(B).withCard(Mandatory);
        AstConcreteClafer c = C.addChild("c").withCard(Mandatory).refToUnique(IntType);
        AstConcreteClafer D = model.addChild("D").extending(B).withCard(Optional);
        AstConcreteClafer d = D.addChild("d").withCard(Mandatory).refToUnique(IntType);

        model.addConstraint(equal(joinRef(join(union(global(C), global(D)), b)), constant(1)));

        ClaferSolver solver = ClaferCompiler.compile(model, Scope.defaultScope(2).intLow(-1).intHigh(1));

        int count = 0;
        while (solver.find()) {
            for (InstanceClafer Ci : solver.instance().getTopClafers(C)) {
                for (InstanceClafer bi : Ci.getChildren(b)) {
                    assertEquals(1, bi.getRef().getValue());
                }
            }
            for (InstanceClafer Di : solver.instance().getTopClafers(D)) {
                for (InstanceClafer bi : Di.getChildren(b)) {
                    assertEquals(1, bi.getRef().getValue());
                }
            }
            count++;
        }
        assertEquals(90, count);
    }

    /**
     * <pre>
     * abstract A
     *     a -> int
     * abstract B : A
     *     b -> int
     * C : B
     *     c -> int
     * D : B ?
     *     d -> int
     *
     * [(C ++ D).d.ref = 1]
     * </pre>
     */
    @Test(timeout = 60000)
    public void testDowncastJoin() {
        AstModel model = newModel();

        AstAbstractClafer A = model.addAbstract("A");
        AstConcreteClafer a = A.addChild("a").withCard(Mandatory).refToUnique(IntType);
        AstAbstractClafer B = model.addAbstract("B").extending(A);
        AstConcreteClafer b = B.addChild("b").withCard(Mandatory).refToUnique(IntType);
        AstConcreteClafer C = model.addChild("C").extending(B).withCard(Mandatory);
        AstConcreteClafer c = C.addChild("c").withCard(Mandatory).refToUnique(IntType);
        AstConcreteClafer D = model.addChild("D").extending(B).withCard(Optional);
        AstConcreteClafer d = D.addChild("d").withCard(Mandatory).refToUnique(IntType);

        model.addConstraint(equal(joinRef(join(union(global(C), global(D)), d)), constant(1)));

        ClaferSolver solver = ClaferCompiler.compile(model, Scope.defaultScope(2).intLow(-1).intHigh(1));

        int count = 0;
        while (solver.find()) {
            for (InstanceClafer Di : solver.instance().getTopClafers(D)) {
                for (InstanceClafer di : Di.getChildren(d)) {
                    assertEquals(1, di.getRef().getValue());
                }
            }
            count++;
        }
        assertEquals(243, count);
    }

    /**
     * <pre>
     * abstract A
     *     a -> int
     * abstract B : A
     *     b -> int
     * C : B
     *     c -> int
     * D : B ?
     *     d -> int
     *
     * [(C ++ D).a.ref = 1]
     * </pre>
     */
    @Test(timeout = 60000)
    public void testUpcastJoin() {
        AstModel model = newModel();

        AstAbstractClafer A = model.addAbstract("A");
        AstConcreteClafer a = A.addChild("a").withCard(Mandatory).refToUnique(IntType);
        AstAbstractClafer B = model.addAbstract("B").extending(A);
        AstConcreteClafer b = B.addChild("b").withCard(Mandatory).refToUnique(IntType);
        AstConcreteClafer C = model.addChild("C").extending(B).withCard(Mandatory);
        AstConcreteClafer c = C.addChild("c").withCard(Mandatory).refToUnique(IntType);
        AstConcreteClafer D = model.addChild("D").extending(B).withCard(Optional);
        AstConcreteClafer d = D.addChild("d").withCard(Mandatory).refToUnique(IntType);

        model.addConstraint(equal(joinRef(join(union(global(C), global(D)), a)), constant(1)));

        ClaferSolver solver = ClaferCompiler.compile(model, Scope.defaultScope(2).intLow(-1).intHigh(1));

        int count = 0;
        while (solver.find()) {
            for (InstanceClafer Ci : solver.instance().getTopClafers(C)) {
                for (InstanceClafer ai : Ci.getChildren(a)) {
                    assertEquals(1, ai.getRef().getValue());
                }
            }
            for (InstanceClafer Di : solver.instance().getTopClafers(D)) {
                for (InstanceClafer ai : Di.getChildren(a)) {
                    assertEquals(1, ai.getRef().getValue());
                }
            }
            count++;
        }
        assertEquals(90, count);
    }

    /**
     * <pre>
     * abstract A -> int
     * abstract B : A
     * C : B
     * D : A
     * E
     *
     * [(C ++ D).ref = 1]
     * </pre>
     */
    @Test(timeout = 60000)
    public void testNocastJoinRef() {
        AstModel model = newModel();

        AstAbstractClafer a = model.addAbstract("A").refToUnique(IntType);
        AstAbstractClafer b = model.addAbstract("B").extending(a);
        AstConcreteClafer c = model.addChild("C").extending(b).withCard(Mandatory);
        AstConcreteClafer d = model.addChild("D").extending(a).withCard(Mandatory);
        AstConcreteClafer e = model.addChild("E").withCard(Mandatory);

        model.addConstraint(equal(joinRef(union(global(c), global(d))), constant(1)));

        ClaferSolver solver = ClaferCompiler.compile(model, Scope.defaultScope(2).intLow(-1).intHigh(1));

        int count = 0;
        while (solver.find()) {
            for (InstanceClafer Ci : solver.instance().getTopClafers(c)) {
                assertEquals(1, Ci.getRef().getValue());
            }
            for (InstanceClafer Di : solver.instance().getTopClafers(d)) {
                assertEquals(1, Di.getRef().getValue());
            }
            count++;
        }
        assertEquals(1, count);
    }

    /**
     * <pre>
     * abstract A
     * abstract B : A
     * C : B -> int
     * D : B
     *
     * [(C ++ D).ref = 1]
     * </pre>
     */
    @Test(timeout = 60000)
    public void testDowncastJoinRef() {
        AstModel model = newModel();

        AstAbstractClafer a = model.addAbstract("A");
        AstAbstractClafer b = model.addAbstract("B").extending(a);
        AstConcreteClafer c = model.addChild("C").extending(b).refToUnique(IntType).withCard(Mandatory);
        AstConcreteClafer d = model.addChild("D").extending(b).withCard(Mandatory);

        model.addConstraint(equal(joinRef(union(global(c), global(d))), constant(1)));

        ClaferSolver solver = ClaferCompiler.compile(model, Scope.defaultScope(2).intLow(-1).intHigh(1));

        int count = 0;
        while (solver.find()) {
            for (InstanceClafer Ci : solver.instance().getTopClafers(c)) {
                assertEquals(1, Ci.getRef().getValue());
            }
            count++;
        }
        assertEquals(1, count);
    }

    /**
     * <pre>
     * abstract A -> int
     * abstract B : A
     * C : B
     * D : B
     * E
     *
     * [(C ++ D).ref = 1]
     * </pre>
     */
    @Test(timeout = 60000)
    public void testUpcastJoinRef() {
        AstModel model = newModel();

        AstAbstractClafer a = model.addAbstract("A").refToUnique(IntType);
        AstAbstractClafer b = model.addAbstract("B").extending(a);
        AstConcreteClafer c = model.addChild("C").extending(b).withCard(Mandatory);
        AstConcreteClafer d = model.addChild("D").extending(b).withCard(Mandatory);
        AstConcreteClafer e = model.addChild("E").withCard(Mandatory);

        model.addConstraint(equal(joinRef(union(global(c), global(d))), constant(1)));

        ClaferSolver solver = ClaferCompiler.compile(model, Scope.defaultScope(2).intLow(-1).intHigh(1));

        int count = 0;
        while (solver.find()) {
            for (InstanceClafer Ci : solver.instance().getTopClafers(c)) {
                assertEquals(1, Ci.getRef().getValue());
            }
            for (InstanceClafer Di : solver.instance().getTopClafers(d)) {
                assertEquals(1, Di.getRef().getValue());
            }
            count++;
        }
        assertEquals(1, count);
    }

    /**
     * <pre>
     * A
     * B ?
     * [ A = A ++ B ]
     * </pre>
     */
    @Test(timeout = 60000)
    public void testNocastEquality() {
        AstModel model = newModel();

        AstConcreteClafer a = model.addChild("A").withCard(Mandatory);
        AstConcreteClafer b = model.addChild("B").withCard(Optional);

        model.addConstraint(equal(global(a), union(global(a), global(b))));

        ClaferSolver solver = ClaferCompiler.compile(model, Scope.defaultScope(2).intLow(-1).intHigh(1));

        assertEquals(1, solver.allInstances().length);
    }

    /**
     * <pre>
     * abstract A
     * B : A
     * C ?
     * [ A = B ++ C ]
     * </pre>
     */
    @Test(timeout = 60000)
    public void testUpcastEquality() {
        AstModel model = newModel();

        AstAbstractClafer a = model.addAbstract("A");
        AstConcreteClafer b = model.addChild("B").extending(a).withCard(Mandatory);
        AstConcreteClafer c = model.addChild("C").withCard(Optional);

        model.addConstraint(equal(global(a), union(global(b), global(c))));

        ClaferSolver solver = ClaferCompiler.compile(model, Scope.defaultScope(2).intLow(-1).intHigh(1));

        assertEquals(1, solver.allInstances().length);
    }

    /**
     * <pre>
     * abstract A
     * B : A
     * C ?
     * [ B = A ++ C ]
     * </pre>
     */
    @Test(timeout = 60000)
    public void testDowncastEquality() {
        AstModel model = newModel();

        AstAbstractClafer a = model.addAbstract("A");
        AstConcreteClafer b = model.addChild("B").extending(a).withCard(Mandatory);
        AstConcreteClafer c = model.addChild("C").withCard(Optional);

        model.addConstraint(equal(global(b), union(global(a), global(c))));

        ClaferSolver solver = ClaferCompiler.compile(model, Scope.defaultScope(2).intLow(-1).intHigh(1));

        assertEquals(1, solver.allInstances().length);
    }

    /**
     * <pre>
     * A
     * B
     * [ A in A ++ B ]
     * </pre>
     */
    @Test(timeout = 60000)
    public void testNocastMembership() {
        AstModel model = newModel();

        AstConcreteClafer a = model.addChild("A").withCard(Mandatory);
        AstConcreteClafer b = model.addChild("B").withCard(Mandatory);

        model.addConstraint(in(global(a), union(global(a), global(b))));

        ClaferSolver solver = ClaferCompiler.compile(model, Scope.defaultScope(2).intLow(-1).intHigh(1));

        assertEquals(1, solver.allInstances().length);
    }

    /**
     * <pre>
     * abstract A
     * B : A
     * C
     * [ A in B ++ C ]
     * </pre>
     */
    @Test(timeout = 60000)
    public void testUpcastMembership() {
        AstModel model = newModel();

        AstAbstractClafer a = model.addAbstract("A");
        AstConcreteClafer b = model.addChild("B").extending(a).withCard(Mandatory);
        AstConcreteClafer c = model.addChild("C").withCard(Mandatory);

        model.addConstraint(in(global(a), union(global(b), global(c))));

        ClaferSolver solver = ClaferCompiler.compile(model, Scope.defaultScope(2).intLow(-1).intHigh(1));

        assertEquals(1, solver.allInstances().length);
    }

    /**
     * <pre>
     * abstract A
     * B : A
     * C
     * [ B in A ++ C ]
     * </pre>
     */
    @Test(timeout = 60000)
    public void testDowncastMembership() {
        AstModel model = newModel();

        AstAbstractClafer a = model.addAbstract("A");
        AstConcreteClafer b = model.addChild("B").extending(a).withCard(Mandatory);
        AstConcreteClafer c = model.addChild("C").withCard(Mandatory);

        model.addConstraint(in(global(b), union(global(a), global(c))));

        ClaferSolver solver = ClaferCompiler.compile(model, Scope.defaultScope(2).intLow(-1).intHigh(1));

        assertEquals(1, solver.allInstances().length);
    }
}
