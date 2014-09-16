package org.clafer.compiler;

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
    /*
     * true: basic symmetry breaking
     * false: full symmetry breaking
     */
    private final boolean basicSymmetryBreaking;
    /*
     * true: basic optimizations
     * false: full optimizations
     */
    private final boolean basicOptimizations;
    /**
     * Use the default options.
     */
    public static final ClaferOption Optimized = new ClaferOption(ClaferSearchStrategy.PreferLargerInstances, false, false);
    public static final ClaferOption Basic = new ClaferOption(ClaferSearchStrategy.PreferLargerInstances, true, true);
    public static final ClaferOption Default = Optimized;

    private ClaferOption(ClaferSearchStrategy strategy, boolean basicSymmetryBreaking, boolean basicOptimizations) {
        this.strategy = Check.notNull(strategy);
        this.basicSymmetryBreaking = basicSymmetryBreaking;
        this.basicOptimizations = basicOptimizations;
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

    @Override
    public String toString() {
        return strategy + "\n"
                + (basicSymmetryBreaking ? "perform only basic symmetry breaking\n" : "perform full symmetry breaking\n")
                + (basicOptimizations ? "perform only basic optimizations\n" : "perform full optimizations\n");
    }
}
