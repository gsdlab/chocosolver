package org.clafer.ast.analysis;

import org.clafer.common.Util;

/**
 *
 * @author jimmy
 */
public class PartialSolution {

    // solution[i] = true <=> i exists
    // solution[i] = false <=> i unknown
    private final boolean[] solution;
    // parent[i] = the list of possible parents
    private final int[][] parent;

    public PartialSolution(boolean[] solution, int[][] parent) {
        this.solution = solution;
        this.parent = parent;
    }

    /**
     * @param id
     * @return true if id exists, false if unknown.
     */
    public boolean hasClafer(int id) {
        return solution[id];
    }

    public boolean[] getSolution() {
        return solution;
    }

    public int[] getKnownClafers() {
        return Util.trues(solution);
    }

    public int[] getUnknownClafers() {
        return Util.falses(solution);
    }

    /**
     * @param id
     * @return id's possible parents
     */
    public int[] getPossibleParents(int id) {
        return parent[id];
    }

    public int size() {
        return solution.length;
    }
}
