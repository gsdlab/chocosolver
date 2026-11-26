package org.clafer.lsp;

import java.io.IOException;
import java.io.PipedInputStream;
import java.io.PipedOutputStream;
import java.util.List;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.TimeUnit;
import org.eclipse.lsp4j.ClientCapabilities;
import org.eclipse.lsp4j.DidChangeTextDocumentParams;
import org.eclipse.lsp4j.DidCloseTextDocumentParams;
import org.eclipse.lsp4j.DidOpenTextDocumentParams;
import org.eclipse.lsp4j.DocumentSymbol;
import org.eclipse.lsp4j.DocumentSymbolCapabilities;
import org.eclipse.lsp4j.DocumentSymbolParams;
import org.eclipse.lsp4j.InitializeParams;
import org.eclipse.lsp4j.InitializeResult;
import org.eclipse.lsp4j.InitializedParams;
import org.eclipse.lsp4j.MessageActionItem;
import org.eclipse.lsp4j.MessageParams;
import org.eclipse.lsp4j.PublishDiagnosticsParams;
import org.eclipse.lsp4j.ShowMessageRequestParams;
import org.eclipse.lsp4j.TextDocumentClientCapabilities;
import org.eclipse.lsp4j.TextDocumentContentChangeEvent;
import org.eclipse.lsp4j.TextDocumentIdentifier;
import org.eclipse.lsp4j.TextDocumentItem;
import org.eclipse.lsp4j.VersionedTextDocumentIdentifier;
import org.eclipse.lsp4j.jsonrpc.Launcher;
import org.eclipse.lsp4j.jsonrpc.messages.Either;
import org.eclipse.lsp4j.launch.LSPLauncher;
import org.eclipse.lsp4j.services.LanguageClient;
import org.eclipse.lsp4j.services.LanguageServer;

/**
 * A server and a client connected by in-memory streams, through LSP4J's
 * launchers, so that the tests send real JSON-RPC messages.
 */
final class ServerHarness implements AutoCloseable {

    final BlockingQueue<PublishDiagnosticsParams> diagnostics = new LinkedBlockingQueue<>();
    final LanguageServer server;
    final CompletableFuture<Integer> exitCode = new CompletableFuture<>();
    private final ExecutorService executor = Executors.newCachedThreadPool();
    private final PipedOutputStream toServer;
    private final PipedOutputStream toClient;
    private int version = 1;

    ServerHarness() throws IOException {
        PipedInputStream serverIn = new PipedInputStream(1 << 16);
        toServer = new PipedOutputStream(serverIn);
        PipedInputStream clientIn = new PipedInputStream(1 << 16);
        toClient = new PipedOutputStream(clientIn);

        ClaferLanguageServer impl = new ClaferLanguageServer(exitCode::complete);
        Launcher<LanguageClient> serverLauncher = LSPLauncher.createServerLauncher(impl, serverIn, toClient,
                executor, null);
        impl.connect(serverLauncher.getRemoteProxy());
        serverLauncher.startListening();

        Launcher<LanguageServer> clientLauncher = LSPLauncher.createClientLauncher(new Client(), clientIn,
                toServer, executor, null);
        clientLauncher.startListening();
        server = clientLauncher.getRemoteProxy();
    }

    private final class Client implements LanguageClient {

        @Override
        public void telemetryEvent(Object object) {
        }

        @Override
        public void publishDiagnostics(PublishDiagnosticsParams params) {
            diagnostics.add(params);
        }

        @Override
        public void showMessage(MessageParams params) {
        }

        @Override
        public CompletableFuture<MessageActionItem> showMessageRequest(ShowMessageRequestParams params) {
            return CompletableFuture.completedFuture(null);
        }

        @Override
        public void logMessage(MessageParams params) {
        }
    }

    InitializeResult initialize() throws Exception {
        InitializeParams params = new InitializeParams();
        ClientCapabilities capabilities = new ClientCapabilities();
        TextDocumentClientCapabilities text = new TextDocumentClientCapabilities();
        DocumentSymbolCapabilities symbols = new DocumentSymbolCapabilities();
        symbols.setHierarchicalDocumentSymbolSupport(true);
        text.setDocumentSymbol(symbols);
        capabilities.setTextDocument(text);
        params.setCapabilities(capabilities);
        InitializeResult result = server.initialize(params).get(10, TimeUnit.SECONDS);
        server.initialized(new InitializedParams());
        return result;
    }

    void open(String uri, String text) {
        server.getTextDocumentService().didOpen(
                new DidOpenTextDocumentParams(new TextDocumentItem(uri, "clafer", version++, text)));
    }

    void change(String uri, String text) {
        server.getTextDocumentService().didChange(new DidChangeTextDocumentParams(
                new VersionedTextDocumentIdentifier(uri, version++), List.of(new TextDocumentContentChangeEvent(text))));
    }

    void close(String uri) {
        server.getTextDocumentService().didClose(new DidCloseTextDocumentParams(new TextDocumentIdentifier(uri)));
    }

    /**
     * @return the next diagnostics published for the document, skipping
     * those of other documents
     */
    PublishDiagnosticsParams nextDiagnostics(String uri) throws InterruptedException {
        while (true) {
            PublishDiagnosticsParams p = diagnostics.poll(10, TimeUnit.SECONDS);
            if (p == null) {
                throw new AssertionError("no diagnostics for " + uri);
            }
            if (p.getUri().equals(uri)) {
                return p;
            }
        }
    }

    List<DocumentSymbol> outline(String uri) throws Exception {
        List<Either<org.eclipse.lsp4j.SymbolInformation, DocumentSymbol>> symbols = server.getTextDocumentService()
                .documentSymbol(new DocumentSymbolParams(new TextDocumentIdentifier(uri))).get(10, TimeUnit.SECONDS);
        return symbols.stream().map(Either::getRight).toList();
    }

    @Override
    public void close() throws Exception {
        toServer.close();
        toClient.close();
        executor.shutdownNow();
    }
}
