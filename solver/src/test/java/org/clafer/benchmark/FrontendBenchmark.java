package org.clafer.benchmark;

import java.io.File;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Arrays;
import java.util.List;
import org.clafer.cli.Frontend;
import org.clafer.javascript.Javascript;

/**
 * The compile times of the Java frontend: for each model, what chocosolver
 * solves, from the {@code .cfr} file ({@link Frontend#readWithJava}: parse,
 * symbols, resolution, scopes, the explicit IR and {@code ToAst}), and, if
 * the compiler's {@code model.js} is next to the model, the time Rhino takes
 * to read it, which is the rest of the {@code .js} path before solving. No
 * model is solved. Run by {@code benchmarking/frontend-speed/run.sh}.
 * <p>
 * Usage: {@code FrontendBenchmark WARMUPS RUNS MODEL...}, which prints one
 * tab-separated line per model: the model, the Java frontend's status
 * ({@code ok}, {@code rejected: first error}, {@code url-import},
 * {@code escape}), the median and the minimum of the Java frontend's runs in
 * milliseconds, and the median of Rhino's, or {@code -}. With 0 warm-ups and
 * 1 run, it times a cold start.
 */
public final class FrontendBenchmark {

    private FrontendBenchmark() {
    }

    public static void main(String[] args) throws Exception {
        int warmups = Integer.parseInt(args[0]);
        int runs = Integer.parseInt(args[1]);
        for (int i = 2; i < args.length; i++) {
            System.out.println(measure(new File(args[i]), warmups, runs));
        }
    }

    private static String measure(File model, int warmups, int runs) throws Exception {
        String text = Files.readString(model.toPath());
        String status = text.contains("[choco|") ? "escape" : "ok";
        double[] java = new double[runs];
        for (int i = 0; i < warmups + runs; i++) {
            long start = System.nanoTime();
            try {
                Frontend.readWithJava(model, List.of());
            } catch (Frontend.ModelException e) {
                if (status.equals("ok")) {
                    String first = e.getMessage().lines().findFirst().orElse("");
                    status = first.contains("imports a URL") ? "url-import" : "rejected: " + first;
                }
            } catch (RuntimeException e) {
                status = "crashed: " + e;
            }
            long end = System.nanoTime();
            if (i >= warmups) {
                java[i - warmups] = (end - start) / 1e6;
            }
        }
        String rhino = "-";
        Path path = model.toPath();
        Path js = path.resolveSibling(path.getFileName().toString().replaceFirst("\\.cfr$", ".js"));
        if (Files.isRegularFile(js)) {
            double[] times = new double[runs];
            for (int i = 0; i < warmups + runs; i++) {
                long start = System.nanoTime();
                try {
                    Javascript.readModel(js.toFile());
                } catch (Exception e) {
                    rhino = "error";
                }
                long end = System.nanoTime();
                if (i >= warmups) {
                    times[i - warmups] = (end - start) / 1e6;
                }
            }
            if (!rhino.equals("error")) {
                rhino = String.format("%.2f", median(times));
            }
        }
        double[] sorted = java.clone();
        Arrays.sort(sorted);
        return String.join("\t", model.getPath(), status.replace('\t', ' '), String.format("%.2f", median(java)),
                String.format("%.2f", sorted[0]), rhino);
    }

    private static double median(double[] values) {
        double[] sorted = values.clone();
        Arrays.sort(sorted);
        int n = sorted.length;
        return n % 2 == 1 ? sorted[n / 2] : (sorted[n / 2 - 1] + sorted[n / 2]) / 2;
    }
}
