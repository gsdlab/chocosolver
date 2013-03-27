package org.clafer.compiler;

import org.clafer.Check;
import org.clafer.ast.AstClafer;
import org.clafer.compiler.SolutionMap.Children;
import solver.Solver;

/**
 *
 * @author jimmy
 */
public class ChocoSolver {

    private final Solver solver;
    private final SolutionMap solutionMap;
    private boolean first = true;

    ChocoSolver(Solver solver, SolutionMap solutionMap) {
        this.solver = Check.notNull(solver);
        this.solutionMap = Check.notNull(solutionMap);
    }

    public boolean nextSolution() {
        if (first) {
            first = false;
            return solver.findSolution();
        }
        return solver.nextSolution();
    }

    public String solution() {
        StringBuilder result = new StringBuilder();
        for (Children top : solutionMap.getTopChildren()) {
            for (int id : top.getIds()) {
                solution("", top.getType(), id, result);
            }
        }
        return result.toString();
    }

    private void solution(String indent, AstClafer clafer, int id, StringBuilder out) {
        out.append(indent).append(clafer.getName()).append(id).append('\n');
        for (Children child : solutionMap.getChildren(clafer, id)) {
            for (int childId : child.getIds()) {
                solution(indent + "    ", child.getType(), childId, out);
            }
        }
    }
}
