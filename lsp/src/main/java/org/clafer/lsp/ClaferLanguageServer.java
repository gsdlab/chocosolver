package org.clafer.lsp;

import java.net.URI;
import java.nio.file.Path;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.concurrent.CompletableFuture;
import java.util.function.IntConsumer;
import org.eclipse.lsp4j.DefinitionParams;
import org.eclipse.lsp4j.DidChangeConfigurationParams;
import org.eclipse.lsp4j.Hover;
import org.eclipse.lsp4j.HoverParams;
import org.eclipse.lsp4j.Location;
import org.eclipse.lsp4j.LocationLink;
import org.eclipse.lsp4j.MarkupContent;
import org.eclipse.lsp4j.MarkupKind;
import org.eclipse.lsp4j.ReferenceParams;
import org.eclipse.lsp4j.DidChangeTextDocumentParams;
import org.eclipse.lsp4j.DidChangeWatchedFilesParams;
import org.eclipse.lsp4j.DidCloseTextDocumentParams;
import org.eclipse.lsp4j.DidOpenTextDocumentParams;
import org.eclipse.lsp4j.DidSaveTextDocumentParams;
import org.eclipse.lsp4j.DocumentSymbol;
import org.eclipse.lsp4j.DocumentSymbolParams;
import org.eclipse.lsp4j.InitializeParams;
import org.eclipse.lsp4j.InitializeResult;
import org.eclipse.lsp4j.PublishDiagnosticsParams;
import org.eclipse.lsp4j.ServerCapabilities;
import org.eclipse.lsp4j.ServerInfo;
import org.eclipse.lsp4j.SymbolInformation;
import org.eclipse.lsp4j.TextDocumentContentChangeEvent;
import org.eclipse.lsp4j.TextDocumentSyncKind;
import org.eclipse.lsp4j.jsonrpc.messages.Either;
import org.eclipse.lsp4j.services.LanguageClient;
import org.eclipse.lsp4j.services.LanguageClientAware;
import org.eclipse.lsp4j.services.LanguageServer;
import org.eclipse.lsp4j.services.TextDocumentService;
import org.eclipse.lsp4j.services.WorkspaceService;

/**
 * The Clafer language server. It keeps the text of the open documents, and
 * analyzes each of them again, completely, whenever one changes, since one
 * may import another.
 * <p>
 * LSP4J delivers the notifications one by one on the thread that reads the
 * messages, so the documents are changed and analyzed on that thread; the
 * requests read the latest analysis.
 */
public final class ClaferLanguageServer implements LanguageServer, LanguageClientAware {

    /**
     * The open documents, by URI, in the order they were opened.
     */
    private final Map<String, Document> documents = new LinkedHashMap<>();
    private final IntConsumer exit;
    private LanguageClient client;
    private boolean shutdown = false;

    /**
     * An open document: its text and its latest analysis.
     */
    private record Document(String uri, String path, String text, Analysis analysis) {
    }

    /**
     * @param exit what {@code exit} does with the exit code, e.g.,
     * {@code System::exit}
     */
    public ClaferLanguageServer(IntConsumer exit) {
        this.exit = exit;
    }

    @Override
    public void connect(LanguageClient client) {
        this.client = client;
    }

    @Override
    public CompletableFuture<InitializeResult> initialize(InitializeParams params) {
        ServerCapabilities capabilities = new ServerCapabilities();
        capabilities.setTextDocumentSync(TextDocumentSyncKind.Full);
        capabilities.setDocumentSymbolProvider(true);
        capabilities.setDefinitionProvider(true);
        capabilities.setHoverProvider(true);
        capabilities.setReferencesProvider(true);
        String version = Optional.ofNullable(ClaferLanguageServer.class.getPackage().getImplementationVersion())
                .orElse("0.6.0");
        return CompletableFuture.completedFuture(
                new InitializeResult(capabilities, new ServerInfo("clafer-ls", version)));
    }

    @Override
    public CompletableFuture<Object> shutdown() {
        shutdown = true;
        return CompletableFuture.completedFuture(null);
    }

    @Override
    public void exit() {
        exit.accept(shutdown ? 0 : 1);
    }

    @Override
    public TextDocumentService getTextDocumentService() {
        return textDocuments;
    }

    @Override
    public WorkspaceService getWorkspaceService() {
        return workspace;
    }

    private final TextDocumentService textDocuments = new TextDocumentService() {

        @Override
        public void didOpen(DidOpenTextDocumentParams params) {
            String uri = params.getTextDocument().getUri();
            documents.put(uri, new Document(uri, path(uri), params.getTextDocument().getText(), null));
            analyzeAll();
        }

        @Override
        public void didChange(DidChangeTextDocumentParams params) {
            String uri = params.getTextDocument().getUri();
            Document d = documents.get(uri);
            if (d == null) {
                return;
            }
            String text = d.text();
            // full sync: the last change is the whole text
            for (TextDocumentContentChangeEvent change : params.getContentChanges()) {
                if (change.getRange() == null) {
                    text = change.getText();
                }
            }
            documents.put(uri, new Document(uri, d.path(), text, null));
            analyzeAll();
        }

        @Override
        public void didClose(DidCloseTextDocumentParams params) {
            String uri = params.getTextDocument().getUri();
            if (documents.remove(uri) != null) {
                publish(uri, List.of());
                analyzeAll();
            }
        }

        @Override
        public void didSave(DidSaveTextDocumentParams params) {
        }

        @Override
        public CompletableFuture<List<Either<SymbolInformation, DocumentSymbol>>> documentSymbol(
                DocumentSymbolParams params) {
            Document d = documents.get(params.getTextDocument().getUri());
            List<Either<SymbolInformation, DocumentSymbol>> symbols = d == null ? List.of()
                    : d.analysis().outline().stream().map(Either::<SymbolInformation, DocumentSymbol>forRight)
                            .toList();
            return CompletableFuture.completedFuture(symbols);
        }

        @Override
        public CompletableFuture<Either<List<? extends Location>, List<? extends LocationLink>>> definition(
                DefinitionParams params) {
            List<Location> found = index(params.getTextDocument().getUri())
                    .flatMap(i -> i.definition(params.getPosition())).map(List::of).orElse(List.of());
            return CompletableFuture.completedFuture(Either.forLeft(found));
        }

        @Override
        public CompletableFuture<List<? extends Location>> references(ReferenceParams params) {
            boolean withDeclaration = params.getContext() == null || params.getContext().isIncludeDeclaration();
            List<Location> found = index(params.getTextDocument().getUri())
                    .map(i -> i.references(params.getPosition(), withDeclaration)).orElse(List.of());
            return CompletableFuture.completedFuture(found);
        }

        @Override
        public CompletableFuture<Hover> hover(HoverParams params) {
            Optional<Index> index = index(params.getTextDocument().getUri());
            Hover hover = index.flatMap(i -> i.hover(params.getPosition()).map(text -> {
                Hover h = new Hover(new MarkupContent(MarkupKind.MARKDOWN, text));
                i.rangeAt(params.getPosition()).ifPresent(h::setRange);
                return h;
            })).orElse(null);
            return CompletableFuture.completedFuture(hover);
        }
    };

    private final WorkspaceService workspace = new WorkspaceService() {

        @Override
        public void didChangeConfiguration(DidChangeConfigurationParams params) {
        }

        @Override
        public void didChangeWatchedFiles(DidChangeWatchedFilesParams params) {
            // an imported file changed on disk
            analyzeAll();
        }
    };

    private Optional<Index> index(String uri) {
        Document d = documents.get(uri);
        return d == null || d.analysis() == null ? Optional.empty() : Optional.of(d.analysis().index());
    }

    /**
     * Analyzes every open document again, and publishes its diagnostics.
     */
    private void analyzeAll() {
        for (Document d : List.copyOf(documents.values())) {
            Analysis a = Analysis.of(d.uri(), d.path(), d.text(), this::openText);
            documents.put(d.uri(), new Document(d.uri(), d.path(), d.text(), a));
            publish(d.uri(), a.diagnostics());
        }
    }

    private void publish(String uri, List<org.eclipse.lsp4j.Diagnostic> diagnostics) {
        if (client != null) {
            client.publishDiagnostics(new PublishDiagnosticsParams(uri, diagnostics));
        }
    }

    /**
     * @param path a file, as a model names it
     * @return the text of its open document, if it is open
     */
    private Optional<String> openText(String path) {
        String key = key(path);
        for (Document d : documents.values()) {
            if (d.path().equals(key)) {
                return Optional.of(d.text());
            }
        }
        return Optional.empty();
    }

    /**
     * @param uri a document's URI
     * @return its file, absolute and normalized, or the URI itself if it is
     * not a file (an unsaved document)
     */
    static String path(String uri) {
        try {
            URI u = URI.create(uri);
            if ("file".equals(u.getScheme())) {
                return Path.of(u).toAbsolutePath().normalize().toString();
            }
        } catch (IllegalArgumentException | java.nio.file.FileSystemNotFoundException e) {
            // not a file
        }
        return uri;
    }

    private static String key(String path) {
        try {
            return Path.of(path).toAbsolutePath().normalize().toString();
        } catch (java.nio.file.InvalidPathException e) {
            return path;
        }
    }
}
