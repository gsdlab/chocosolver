package org.clafer.compiler;

/**
 * Defines the options for the Clafer compiler. Start with the default options
 * and configure it to your needs. For example,
 * {@code ClaferOptions.Default.preferSmallerInstances()} will prefer smaller
 * instances and use default settings for all the other option.
 *
 * @author jimmy
 */
public class ClaferOptions {

    /*
     * true: prefer larger instances
     * false: prefer smaller instances
     */
    private final boolean preferSmallerInstances;
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
    public static ClaferOptions Optimized = new ClaferOptions(false, false, false);
    public static ClaferOptions Basic = new ClaferOptions(false, true, true);
    public static ClaferOptions Default = Optimized;

    private ClaferOptions(boolean preferSmallerInstances, boolean basicSymmetryBreaking, boolean basicOptimizations) {
        this.preferSmallerInstances = preferSmallerInstances;
        this.basicSymmetryBreaking = basicSymmetryBreaking;
        this.basicOptimizations = basicOptimizations;
    }

    public boolean isPreferSmallerInstances() {
        return preferSmallerInstances;
    }

    public boolean isPreferLargerInstances() {
        return !preferSmallerInstances;
    }

    public ClaferOptions preferSmallerInstances() {
        return new ClaferOptions(true, basicSymmetryBreaking, basicOptimizations);
    }

    public ClaferOptions preferLargerInstances() {
        return new ClaferOptions(false, basicSymmetryBreaking, basicOptimizations);
    }

    public boolean isBasicSymmetryBreaking() {
        return basicSymmetryBreaking;
    }

    public boolean isFullSymmetryBreaking() {
        return !basicSymmetryBreaking;
    }

    public ClaferOptions basicSymmetryBreaking() {
        return new ClaferOptions(preferSmallerInstances, true, basicOptimizations);
    }

    public ClaferOptions fullSymmetryBreaking() {
        return new ClaferOptions(preferSmallerInstances, false, basicOptimizations);
    }

    public boolean isBasicOptimizations() {
        return basicOptimizations;
    }

    public boolean isFullOptimizations() {
        return !basicOptimizations;
    }

    public ClaferOptions basicOptimizations() {
        return new ClaferOptions(preferSmallerInstances, basicSymmetryBreaking, true);
    }

    public ClaferOptions fullOptimizations() {
        return new ClaferOptions(preferSmallerInstances, basicSymmetryBreaking, false);
    }

    public String toString() {
        return (preferSmallerInstances ? "prefer smaller instances\n" : "prefer larger instances\n")
                + (basicSymmetryBreaking ? "perform only basic symmetry breaking\n" : "perform full symmetry breaking\n")
                + (basicOptimizations ? "perform only basic optimizations\n" : "perform full optimizations\n");
    }
}
