//package org.clafer.generator;
//
//import choco.kernel.model.variables.integer.IntegerVariable;
//import choco.kernel.model.variables.set.SetVariable;
//import choco.kernel.solver.Solver;
//import java.io.IOException;
//import org.clafer.Check;
//import org.clafer.ast.AstAbstractClafer;
//import org.clafer.ast.AstClafer;
//import org.clafer.ast.AstConcreteClafer;
//import org.clafer.ast.AstIntClafer;
//import org.clafer.ast.AstModel;
//import org.clafer.ast.AstRef;
//import org.clafer.collection.Pair;
//
///**
// * Prints the solution.
// * 
// * Is tightly coupled with the compiler.
// * 
// * @author jimmy
// */
//public class Printer {
//
//    private final AstModel model;
//    private final Solver solver;
//    private final SolutionMap<AstConcreteClafer, SetVariable[]> childrenSet;
//    private final SolutionMap<AstRef, IntegerVariable[]> refs;
//    private final SolutionMap<Pair<AstAbstractClafer, AstClafer>, Integer> offsets;
//
//    Printer(AstModel model,
//            Solver solver,
//            SolutionMap<AstConcreteClafer, SetVariable[]> childrenSet,
//            SolutionMap<AstRef, IntegerVariable[]> refs,
//            SolutionMap<Pair<AstAbstractClafer, AstClafer>, Integer> offsets) {
//        this.model = Check.notNull(model);
//        this.solver = Check.notNull(solver);
//        this.childrenSet = Check.notNull(childrenSet);
//        this.refs = Check.notNull(refs);
//        this.offsets = Check.notNull(offsets);
//    }
//
//    public void print(Appendable out) throws IOException {
//        for (AstConcreteClafer topClafer : model.getTopClafers()) {
//            print(topClafer, 0, "", out);
//        }
//    }
//
//    public String printToString() {
//        StringBuilder out = new StringBuilder();
//        try {
//            print(out);
//        } catch (IOException e) {
//            throw new Error("StringBuilder should not throw IO exceptions!", e);
//        }
//        return out.toString();
//    }
//
//    private void print(AstConcreteClafer clafer, int parent, String indent, Appendable out)
//            throws IOException {
//        int[] nums = solver.getVar(childrenSet.get(clafer)[parent]).getValue();
//        for (int num : nums) {
//            out.append(indent + clafer.getName() + num);
//            if(clafer.hasRef()) {
//                print(clafer.getRef(), num, out);
//            }
//            out.append('\n');
//            for (AstConcreteClafer child : clafer.getChildren()) {
//                print(child, num, indent + "  ", out);
//            }
//            if (clafer.hasSuperClafer()) {
//                print(clafer.getSuperClafer(), clafer, num, indent, out);
//            }
//        }
//    }
//
//    private void print(AstAbstractClafer sup, AstClafer sub, int parent, String indent, Appendable out)
//            throws IOException {
//        parent += offsets.get(new Pair<AstAbstractClafer, AstClafer>(sup, sub));
//        print(sup, parent, indent, out);
//    }
//
//    private void print(AstAbstractClafer clafer, int parent, String indent, Appendable out)
//            throws IOException {
//        for (AstConcreteClafer child : clafer.getChildren()) {
//            print(child, parent, indent + "  ", out);
//        }
//        if (clafer.hasSuperClafer()) {
//            print(clafer.getSuperClafer(), clafer, parent, indent, out);
//        }
//    }
//
//    private void print(AstRef ref, int parent, Appendable out) throws IOException {
//        int val = solver.getVar(refs.get(ref)[parent]).getVal();
//        out.append(" = ");
//        if (!(ref.getTargetType() instanceof AstIntClafer)) {
//            out.append(ref.getTargetType().getName());
//        }
//        out.append(Integer.toString(val));
//    }
//}
