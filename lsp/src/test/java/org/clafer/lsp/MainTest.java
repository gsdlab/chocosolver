package org.clafer.lsp;

import static org.junit.Assert.assertTrue;

import org.junit.Test;

public class MainTest {

    @Test
    public void versionHasTheBuildDate() {
        String v = Main.version();
        assertTrue(v, v.matches("clafer-ls v\\d+\\.\\d+\\.\\d+\\.\\d{4}-\\d{2}-\\d{2}"));
    }
}
