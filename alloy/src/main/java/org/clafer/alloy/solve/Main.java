package org.clafer.alloy.solve;

import java.io.BufferedReader;
import java.io.File;
import java.io.InputStreamReader;
import java.nio.charset.StandardCharsets;
import java.util.List;
import org.clafer.frontend.ModelFiles;
import org.clafer.frontend.ToIr;
import org.clafer.ig.Repl;

/**
 * The solution loop of a temporal model with the Alloy backend, as
 * {@code claferIG FILE} runs it.
 */
public final class Main {

    private Main() {
    }

    /**
     * Reads the model with the Java frontend.
     *
     * @throws IllegalArgumentException with the errors of the model
     */
    public static ToIr.Result read(String path, List<String> imports) {
        ToIr.Result r = ToIr.toIr(ModelFiles.read(path, imports));
        if (!r.messages().isEmpty()) {
            throw new IllegalArgumentException(String.join("\n", r.messages()));
        }
        return r;
    }

    public static void main(String[] args) throws Exception {
        String path = args[0];
        AlloyBackend backend = new AlloyBackend(() -> read(path, List.of()), new File(path).getName(),
                AlloyBackend.Options.Default);
        new Repl(backend, new Repl.Io(new BufferedReader(new InputStreamReader(System.in, StandardCharsets.UTF_8)),
                System.out, System.err), false).run();
    }
}
