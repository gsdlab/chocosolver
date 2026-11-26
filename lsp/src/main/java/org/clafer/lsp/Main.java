package org.clafer.lsp;

import java.util.concurrent.ExecutionException;
import org.eclipse.lsp4j.jsonrpc.Launcher;
import org.eclipse.lsp4j.launch.LSPLauncher;
import org.eclipse.lsp4j.services.LanguageClient;

/**
 * Runs the Clafer language server on stdin and stdout: {@code clafer-ls},
 * or {@code java -jar clafer-ls.jar}. Nothing else may write to stdout, so
 * messages go to stderr.
 */
public final class Main {

    private Main() {
    }

    public static void main(String[] args) throws InterruptedException {
        for (String a : args) {
            if (a.equals("--version")) {
                String v = Main.class.getPackage().getImplementationVersion();
                System.out.println("clafer-ls " + (v == null ? "0.6.0" : v));
                return;
            }
            if (a.equals("--help") || a.equals("-h")) {
                System.out.println("clafer-ls: the Clafer language server, over stdin and stdout (LSP).");
                System.out.println("Start it from an editor's LSP client; it takes no options but --version.");
                return;
            }
            // --stdio, which some clients pass, is what the server does anyway
        }
        // the JSON-RPC messages are the only output on stdout
        java.io.PrintStream out = System.out;
        System.setOut(System.err);
        ClaferLanguageServer server = new ClaferLanguageServer(System::exit);
        Launcher<LanguageClient> launcher = LSPLauncher.createServerLauncher(server, System.in, out);
        server.connect(launcher.getRemoteProxy());
        try {
            launcher.startListening().get();
        } catch (ExecutionException e) {
            System.err.println("clafer-ls: " + e.getCause());
            System.exit(1);
        }
    }
}
