package org.clafer.lsp;

import java.net.URI;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.concurrent.CompletableFuture;
import java.util.function.IntConsumer;
import org.clafer.frontend.Names;
import org.eclipse.lsp4j.CompletionItem;
import org.eclipse.lsp4j.CompletionList;
import org.eclipse.lsp4j.CompletionOptions;
import org.eclipse.lsp4j.CompletionParams;
import org.eclipse.lsp4j.DefinitionParams;
import org.eclipse.lsp4j.Position;
import org.eclipse.lsp4j.PrepareRenameDefaultBehavior;
import org.eclipse.lsp4j.PrepareRenameParams;
import org.eclipse.lsp4j.PrepareRenameResult;
import org.eclipse.lsp4j.Range;
import org.eclipse.lsp4j.RenameOptions;
import org.eclipse.lsp4j.RenameParams;
import org.eclipse.lsp4j.TextEdit;
import org.eclipse.lsp4j.WorkspaceEdit;
import org.eclipse.lsp4j.jsonrpc.ResponseErrorException;
import org.eclipse.lsp4j.jsonrpc.messages.Either3;
import org.eclipse.lsp4j.jsonrpc.messages.ResponseError;
import org.eclipse.lsp4j.jsonrpc.messages.ResponseErrorCode;
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
    private record Document(String uri, String path, String text, Analysis analysis, Index known) {

        Document(String uri, String path, String text) {
            this(uri, path, text, null, null);
        }
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
        capabilities.setCompletionProvider(new CompletionOptions(false, List.of(".")));
        capabilities.setRenameProvider(new RenameOptions(true));
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
            documents.put(uri, new Document(uri, path(uri), params.getTextDocument().getText()));
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
            documents.put(uri, new Document(uri, d.path(), text, null, d.known()));
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
            Optional<Index> index = index(params.getTextDocument().getUri());
            Optional<Location> declaration = index.flatMap(i -> i.definition(params.getPosition()));
            List<Location> found = declaration.map(d -> everywhere(d, withDeclaration))
                    .orElse(index.map(i -> i.references(params.getPosition(), withDeclaration)).orElse(List.of()));
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

        @Override
        public CompletableFuture<Either<List<CompletionItem>, CompletionList>> completion(CompletionParams params) {
            Document d = documents.get(params.getTextDocument().getUri());
            List<CompletionItem> items = new ArrayList<>();
            if (d != null && d.analysis() != null) {
                for (Index.Completion c : d.analysis().index().completion(params.getPosition(), d.known())) {
                    CompletionItem item = new CompletionItem(c.label());
                    item.setKind(c.kind());
                    item.setDetail(c.detail());
                    item.setSortText(c.rank() + c.label());
                    items.add(item);
                }
            }
            return CompletableFuture.completedFuture(Either.forLeft(items));
        }

        @Override
        public CompletableFuture<Either3<Range, PrepareRenameResult, PrepareRenameDefaultBehavior>> prepareRename(
                PrepareRenameParams params) {
            Optional<Index> index = index(params.getTextDocument().getUri());
            Position p = params.getPosition();
            if (index.isEmpty() || index.get().renameTarget(p).isEmpty()) {
                return CompletableFuture.completedFuture(null);
            }
            Range range = index.get().rangeAt(p).orElseThrow();
            return CompletableFuture.completedFuture(
                    Either3.forSecond(new PrepareRenameResult(range, index.get().nameAt(p).orElse(""))));
        }

        @Override
        public CompletableFuture<WorkspaceEdit> rename(RenameParams params) {
            try {
                return CompletableFuture.completedFuture(renameAt(params.getTextDocument().getUri(),
                        params.getPosition(), params.getNewName()));
            } catch (ResponseErrorException e) {
                return CompletableFuture.failedFuture(e);
            }
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

    /**
     * @return every name in the models of the open documents that refers to
     * what is declared at a location, each once, the model of each document
     * in its order
     */
    private List<Location> everywhere(Location declaration, boolean withDeclaration) {
        List<Location> all = new ArrayList<>();
        for (Document d : documents.values()) {
            if (d.analysis() == null) {
                continue;
            }
            for (Location l : d.analysis().index().occurrencesOf(declaration, withDeclaration)) {
                if (all.stream().noneMatch(x -> Index.sameLocation(x, l))) {
                    all.add(l);
                }
            }
        }
        return all;
    }

    /**
     * The edits that rename a clafer or a local, in every file of the models
     * of the open documents.
     */
    private WorkspaceEdit renameAt(String uri, Position position, String newName) {
        Index index = index(uri).orElseThrow(() -> refused("The document is not open"));
        Location declaration = index.renameTarget(position)
                .orElseThrow(() -> refused("Only the names of clafers and of locals can be renamed"));
        if (!Names.isClaferName(newName)) {
            throw refused("'" + newName + "' cannot be the name of a clafer or a local");
        }
        if (!index.isLocalAt(position)) {
            for (Document d : documents.values()) {
                if (d.analysis() != null && d.analysis().index().occurrencesOf(declaration, true).size() > 0
                        && !d.analysis().diagnostics().isEmpty()) {
                    throw refused("Fix the errors of " + d.uri() + " first: the names of a model with errors are"
                            + " not all known");
                }
            }
        }
        if (index.nameTaken(position, newName)) {
            throw refused("The name '" + newName + "' is taken: renaming to it could change what names mean");
        }
        Map<String, List<TextEdit>> changes = new LinkedHashMap<>();
        List<Location> all = new ArrayList<>();
        for (Document d : documents.values()) {
            if (d.analysis() != null) {
                for (Location l : d.analysis().index().renameOccurrences(declaration)) {
                    if (all.stream().noneMatch(x -> Index.sameLocation(x, l))) {
                        all.add(l);
                    }
                }
            }
        }
        for (Location l : all) {
            changes.computeIfAbsent(l.getUri(), k -> new ArrayList<>()).add(new TextEdit(l.getRange(), newName));
        }
        return new WorkspaceEdit(changes);
    }

    private static ResponseErrorException refused(String message) {
        return new ResponseErrorException(new ResponseError(ResponseErrorCode.InvalidParams, message, null));
    }

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
            // the latest index whose model had its clafers, for completion while the model is broken
            Index known = a.index().hasClafers() || d.known() == null ? a.index() : d.known();
            documents.put(d.uri(), new Document(d.uri(), d.path(), d.text(), a, known));
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
