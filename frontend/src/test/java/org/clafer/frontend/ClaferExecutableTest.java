package org.clafer.frontend;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Optional;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.TemporaryFolder;

/**
 * clafersg and claferc find clafer next to their jar, as in Clafer Tools.
 */
public class ClaferExecutableTest {

    @Rule
    public TemporaryFolder folder = new TemporaryFolder();

    private static final String Clafer = System.getProperty("os.name", "").startsWith("Windows")
            ? "clafer.exe" : "clafer";

    @Test
    public void claferNextToTheJar() throws IOException {
        Path jar = Files.createFile(folder.getRoot().toPath().resolve("clafersg.jar"));
        Path clafer = Files.createFile(folder.getRoot().toPath().resolve(Clafer));
        assertFalse("not executable", ClaferExecutable.nextTo(jar).isPresent());
        clafer.toFile().setExecutable(true);
        assertEquals(Optional.of(clafer), ClaferExecutable.nextTo(jar));
    }

    @Test
    public void noClaferNextToTheJar() throws IOException {
        Path jar = Files.createFile(folder.getRoot().toPath().resolve("clafersg.jar"));
        assertFalse(ClaferExecutable.nextTo(jar).isPresent());
    }

    @Test
    public void aFolderOfClassesIsNotAJar() throws IOException {
        Path classes = folder.newFolder("classes").toPath();
        Files.createFile(folder.getRoot().toPath().resolve(Clafer)).toFile().setExecutable(true);
        assertFalse(ClaferExecutable.nextTo(classes).isPresent());
    }

    @Test
    public void theProperty() {
        String old = System.getProperty("clafer.executable");
        try {
            System.setProperty("clafer.executable", "/opt/clafer/clafer");
            assertEquals("/opt/clafer/clafer", ClaferExecutable.find());
        } finally {
            if (old == null) {
                System.clearProperty("clafer.executable");
            } else {
                System.setProperty("clafer.executable", old);
            }
        }
    }

    @Test
    public void thePathInTheTests() {
        String old = System.getProperty("clafer.executable");
        try {
            System.clearProperty("clafer.executable");
            // the classes are in target/classes, not in a jar
            assertEquals("clafer", ClaferExecutable.find());
        } finally {
            if (old != null) {
                System.setProperty("clafer.executable", old);
            }
        }
    }
}
