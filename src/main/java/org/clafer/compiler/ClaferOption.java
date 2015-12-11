package org.clafer.compiler;

import java.util.Set;
import org.clafer.ast.AstClafer;
import org.clafer.common.Check;

/**
 * Defines the options for the Clafer compiler. Start with the default options
 * and configure it to your needs. For example,
 * {@code ClaferOptions.Default.preferSmallerInstances()} will prefer smaller
 * instances and use default settings for all the other option.
 *
 * @author jimmy
 */
public class ClaferOption {

    private final ClaferSearchStrategy strategy;
    /**
     * If true then basic symmetry breaking, else full symmetry breaking.
     */
    private final boolean basicSymmetryBreaking;
    /**
     * If true then basic optimizations else full optimizations.
     */
    private final boolean basicOptimizations;
    /**
     * The branching heuristic will use the priorities to determine the order of
     * branching.
     *
     * For example, consider the priorities [{a,b}, {c,d}, {e}]. This forces the
     * branching heuristic to branch on either a or b. Once both a and b are
     * assigned, then the branching heuristic will branch on either c or d. Once
     * all a, b, c, and d are assigned then the branching heuristic will branch
     * on e. Once all a, b, c, d, and e are assigned, then the branching
     * heuristic will branch on the remaining Clafers.
     *
     * Assumes that the sets in this array are all pairwise disjoint.
     */
    private final Set<AstClafer>[] branchingPriority;

    /**
     * Use the default options.
     */
    public static final ClaferOption Optimized = new ClaferOption(ClaferSearchStrategy.PreferSmallerInstances, false, false);
    public static final ClaferOption Basic = new ClaferOption(ClaferSearchStrategy.PreferSmallerInstances, true, true);
    public static final ClaferOption Default = Optimized;

    private ClaferOption(ClaferSearchStrategy strategy, boolean basicSymmetryBreaking, boolean basicOptimizations, Set<AstClafer>[] branchingPriority) {
        this.strategy = Check.notNull(strategy);
        this.basicSymmetryBreaking = basicSymmetryBreaking;
        this.basicOptimizations = basicOptimizations;
        this.branchingPriority = branchingPriority;
    }

    private ClaferOption(ClaferSearchStrategy strategy, boolean basicSymmetryBreaking, boolean basicOptimizations) {
        this(strategy, basicSymmetryBreaking, basicOptimizations, new Set[0]);
    }

    public ClaferSearchStrategy getStrategy() {
        return strategy;
    }

    public ClaferOption setStrategy(ClaferSearchStrategy strategy) {
        return new ClaferOption(strategy, basicSymmetryBreaking, basicOptimizations);
    }

    public boolean isBasicSymmetryBreaking() {
        return basicSymmetryBreaking;
    }

    public boolean isFullSymmetryBreaking() {
        return !basicSymmetryBreaking;
    }

    public ClaferOption basicSymmetryBreaking() {
        return new ClaferOption(strategy, true, basicOptimizations);
    }

    public ClaferOption fullSymmetryBreaking() {
        return new ClaferOption(strategy, false, basicOptimizations);
    }

    public boolean isBasicOptimizations() {
        return basicOptimizations;
    }

    public boolean isFullOptimizations() {
        return !basicOptimizations;
    }

    public ClaferOption basicOptimizations() {
        return new ClaferOption(strategy, basicSymmetryBreaking, true);
    }

    public ClaferOption fullOptimizations() {
        return new ClaferOption(strategy, basicSymmetryBreaking, false);
    }

    public Set<AstClafer>[] getBranchingPriority() {
        return branchingPriority;
    }

    public ClaferOption setBranchingPriority(Set<AstClafer>... branchingPriority) {
        return new ClaferOption(strategy, basicSymmetryBreaking, basicOptimizations, branchingPriority);
    }

    @Override
    public String toString() {
        return strategy + "\n"
                + (basicSymmetryBreaking ? "perform only basic symmetry breaking\n" : "perform full symmetry breaking\n")
                + (basicOptimizations ? "perform only basic optimizations\n" : "perform full optimizations\n");
    }
}
