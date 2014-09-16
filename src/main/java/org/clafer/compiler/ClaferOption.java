package org.clafer.compiler;

/**
 * Defines the options for the Clafer compiler. Start with the default options
 * and configure it to your needs. For example,
 * {@code ClaferOptions.Default.preferSmallerInstances()} will prefer smaller
 * instances and use default settings for all the other option.
 *
 * @author jimmy
 */
public class ClaferOption {

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
    public static ClaferOption Optimized = new ClaferOption(false, false, false);
    public static ClaferOption Basic = new ClaferOption(false, true, true);
    public static ClaferOption Default = Optimized;

    private ClaferOption(boolean preferSmallerInstances, boolean basicSymmetryBreaking, boolean basicOptimizations) {
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

    public ClaferOption preferSmallerInstances() {
        return new ClaferOption(true, basicSymmetryBreaking, basicOptimizations);
    }

    public ClaferOption preferLargerInstances() {
        return new ClaferOption(false, basicSymmetryBreaking, basicOptimizations);
    }

    public boolean isBasicSymmetryBreaking() {
        return basicSymmetryBreaking;
    }

    public boolean isFullSymmetryBreaking() {
        return !basicSymmetryBreaking;
    }

    public ClaferOption basicSymmetryBreaking() {
        return new ClaferOption(preferSmallerInstances, true, basicOptimizations);
    }

    public ClaferOption fullSymmetryBreaking() {
        return new ClaferOption(preferSmallerInstances, false, basicOptimizations);
    }

    public boolean isBasicOptimizations() {
        return basicOptimizations;
    }

    public boolean isFullOptimizations() {
        return !basicOptimizations;
    }

    public ClaferOption basicOptimizations() {
        return new ClaferOption(preferSmallerInstances, basicSymmetryBreaking, true);
    }

    public ClaferOption fullOptimizations() {
        return new ClaferOption(preferSmallerInstances, basicSymmetryBreaking, false);
    }

    public String toString() {
        return (preferSmallerInstances ? "prefer smaller instances\n" : "prefer larger instances\n")
                + (basicSymmetryBreaking ? "perform only basic symmetry breaking\n" : "perform full symmetry breaking\n")
                + (basicOptimizations ? "perform only basic optimizations\n" : "perform full optimizations\n");
    }
}
