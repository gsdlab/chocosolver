package org.clafer.ast.analysis;

import gnu.trove.list.array.TIntArrayList;
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
    // siblings[i] = the list of possible siblings
    private final int[][] siblings;

    public PartialSolution(boolean[] solution, int[][] parent) {
        this.solution = solution;
        this.parent = parent;
        this.siblings = new int[parent.length][];
        TIntArrayList array = new TIntArrayList(parent.length);
        for (int i = 0; i < siblings.length; i++) {
            array.clear();
            for (int j = 1; j < siblings.length; j++) {
                if (i != j) {
                    // TODO: shouldn't be null.
                    if (parent[i] != null && parent[j] != null) {
                        if (overlaps(parent[i], parent[j])) {
                            array.add(j);
                        }
                    }
                }
            }
            siblings[i] = array.toArray();
        }
    }

    private static boolean overlaps(int[] a, int[] b) {
        for (int i : a) {
            if (Util.in(i, b)) {
                return true;
            }
        }
        return false;
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
     * @return possible parents of {@code id}
     */
    public int[] getPossibleParents(int id) {
        return parent[id];
    }

    /**
     * @return {@code true} if and only if all parents are known, {@code false}
     * otherwise
     */
    public boolean parentSolutionKnown() {
        for (int[] p : parent) {
            if (p.length != 1) {
                return false;
            }
        }
        return true;
    }

    /**
     * @param id
     * @return possible siblings of {@code id}
     */
    public int[] getPossiblesSiblings(int id) {
        return siblings[id];
    }

    public int size() {
        return solution.length;
    }
}
