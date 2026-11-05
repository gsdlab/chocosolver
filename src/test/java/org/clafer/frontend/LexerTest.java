package org.clafer.frontend;

import java.io.File;
import java.net.URISyntaxException;
import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;
import org.clafer.ast.AstPrinter;
import org.clafer.ast.SourceSpan;
import org.clafer.javascript.Javascript;
import org.clafer.javascript.JavascriptFile;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import org.junit.Test;

public class LexerTest {

    private static List<String> texts(String source) {
        Lexer.Result result = Lexer.lex(source);
        assertEquals(List.of(), result.diagnostics());
        return result.tokens().stream()
                .filter(t -> t.kind() != TokenKind.EOF)
                .map(t -> t.kind() + " " + t.text())
                .collect(Collectors.toList());
    }

    @Test
    public void testClafer() {
        assertEquals(List.of("WORD abstract", "INT 0", "SYMBOL ..", "SYMBOL *", "WORD c0_T",
                "SYMBOL ->>", "WORD int", "INT 0", "SYMBOL ..", "SYMBOL *", "SYMBOL {", "SYMBOL }"),
                texts("abstract 0..* c0_T ->> int 0..* {}"));
    }

    @Test
    public void testLongestSymbolWins() {
        assertEquals(List.of("SYMBOL <=>", "SYMBOL <=", "SYMBOL <<", "SYMBOL <:", "SYMBOL <",
                "SYMBOL ->>", "SYMBOL ->", "SYMBOL -", "SYMBOL --", "SYMBOL ::", "SYMBOL :>", "SYMBOL :"),
                texts("<=> <= << <: < ->> -> - -- :: :> :"));
    }

    @Test
    public void testNumbers() {
        assertEquals(List.of("INT 1", "SYMBOL ..", "INT 2"), texts("1..2"));
        assertEquals(List.of("DECIMAL 19.99", "SYMBOL .", "WORD x"), texts("19.99.x"));
        assertEquals(List.of("SYMBOL -", "INT 16", "SYMBOL ..", "INT 16"), texts("-16..16"));
        assertEquals(List.of("WORD c0_x'", "SYMBOL .", "WORD dref"), texts("c0_x'.dref"));
    }

    @Test
    public void testWhitespaceIsInsignificant() {
        assertEquals(texts("0..* c0_A 1..1 { 0..* c0_B 0..1 {} }"),
                texts("0..* c0_A 1..1 {\n\t0..*   c0_B\r\n0..1\r{\n}}"));
    }

    @Test
    public void testSpans() {
        Lexer.Result result = Lexer.lex("c0_A {\r\n  [this]\n}");
        List<Token> tokens = result.tokens();
        assertEquals(new SourceSpan(1, 1, 1, 5), tokens.get(0).span());
        assertEquals(new SourceSpan(1, 6, 1, 7), tokens.get(1).span());
        assertEquals(new SourceSpan(2, 3, 2, 4), tokens.get(2).span());
        assertEquals(new SourceSpan(2, 4, 2, 8), tokens.get(3).span());
        assertEquals(new SourceSpan(3, 1, 3, 2), tokens.get(5).span());
        assertEquals(TokenKind.EOF, tokens.get(6).kind());
        assertEquals(new SourceSpan(3, 2, 3, 2), tokens.get(6).span());
    }

    @Test
    public void testComments() {
        Lexer.Result result = Lexer.lex("a // one\n/* two\nlines */ b");
        assertEquals(List.of("a", "b", ""), result.tokens().stream().map(Token::text).collect(Collectors.toList()));
        assertEquals(List.of("// one", "/* two\nlines */"),
                result.comments().stream().map(Token::text).collect(Collectors.toList()));
        assertEquals(new SourceSpan(2, 1, 3, 9), result.comments().get(1).span());
        assertEquals(new SourceSpan(3, 10, 3, 11), result.tokens().get(1).span());
    }

    @Test
    public void testStrings() {
        String source = "\"a\\\"b\\n\" \"\\u00e9\\\\\"";
        assertEquals(List.of("STRING \"a\\\"b\\n\"", "STRING \"\\u00e9\\\\\""), texts(source));
        List<Token> tokens = Lexer.lex(source).tokens();
        assertEquals("a\"b\n", Lexer.unescape(tokens.get(0).text()));
        assertEquals("é\\", Lexer.unescape(tokens.get(1).text()));
    }

    @Test
    public void testErrorsDoNotStopTheLexer() {
        Lexer.Result result = Lexer.lex("a @ b \"open\nc \"x\\q\" /* never closed");
        assertEquals(List.of(
                "1:3: Unexpected character '@'",
                "1:7: Unterminated string",
                "2:5: Unknown escape in a string; use \\\", \\\\, \\n, \\t or \\uXXXX",
                "2:9: Unterminated comment"),
                result.diagnostics().stream().map(Diagnostic::toString).collect(Collectors.toList()));
        assertEquals(List.of("WORD a", "ERROR @", "WORD b", "ERROR \"open", "WORD c",
                "STRING \"x\\q\"", "ERROR /* never closed", "EOF "),
                result.tokens().stream().map(t -> t.kind() + " " + t.text()).collect(Collectors.toList()));
    }

    /**
     * Every model in the test resources, printed in the explicit form, lexes
     * without errors, and each token's text is the source at its span.
     */
    @Test
    public void testPrintedModels() throws Exception {
        int models = 0;
        for (String dir : List.of("/solve-positive", "/solve-negative", "/assert-positive", "/assert-negative", "/optimization")) {
            File[] files = new File(LexerTest.class.getResource(dir).toURI()).listFiles((d, n) -> n.endsWith(".js"));
            for (File file : files) {
                JavascriptFile js = Javascript.readModel(file);
                String source = AstPrinter.print(js.getModel(), js.getScope(), js.getObjectives(), js.getAssertions());
                Lexer.Result result = Lexer.lex(source);
                assertEquals(file.getName(), List.of(), result.diagnostics());
                List<String> lines = lines(source);
                for (Token token : result.tokens()) {
                    assertEquals(file.getName() + " " + token, token.text(), at(lines, token.span()));
                }
                models++;
            }
        }
        assertTrue(models > 100);
    }

    private static List<String> lines(String source) {
        List<String> lines = new ArrayList<>(List.of(source.split("\n", -1)));
        return lines;
    }

    private static String at(List<String> lines, SourceSpan span) {
        assertEquals("one-line token", span.line(), span.endLine());
        return lines.get(span.line() - 1).substring(span.column() - 1, span.endColumn() - 1);
    }
}
