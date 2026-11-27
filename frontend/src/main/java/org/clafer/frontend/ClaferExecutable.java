package org.clafer.frontend;

import java.net.URISyntaxException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.security.CodeSource;
import java.util.Optional;

/**
 * The Clafer compiler, the Haskell {@code clafer}, that the Java tools run for what they do not do
 * themselves ({@code clafersg} for a model with a {@code [choco| |]} escape or with
 * {@code --haskell-frontend}, {@code claferc} for the modes it forwards):
 * <ul>
 * <li>{@code -Dclafer.executable}, if it is set;</li>
 * <li>otherwise {@code clafer} next to the jar of this class, as in Clafer Tools, where both
 * bundles unpack into one folder with {@code clafersg.jar}, {@code claferc.jar} and
 * {@code clafer}, so that the folder need not be on the {@code PATH};</li>
 * <li>otherwise {@code clafer} on the {@code PATH}.</li>
 * </ul>
 */
public final class ClaferExecutable {

    private ClaferExecutable() {
    }

    /**
     * The command that runs the Clafer compiler.
     */
    public static String find() {
        String property = System.getProperty("clafer.executable");
        if (property != null && !property.isEmpty()) {
            return property;
        }
        return jar().flatMap(ClaferExecutable::nextTo).map(Path::toString).orElse("clafer");
    }

    /**
     * The executable {@code clafer} ({@code clafer.exe} on Windows) in the folder of a jar, if
     * there is one.
     */
    static Optional<Path> nextTo(Path jar) {
        if (!Files.isRegularFile(jar) || jar.getParent() == null) {
            return Optional.empty();
        }
        String name = System.getProperty("os.name", "").startsWith("Windows") ? "clafer.exe" : "clafer";
        Path clafer = jar.getParent().resolve(name);
        return Files.isRegularFile(clafer) && Files.isExecutable(clafer) ? Optional.of(clafer) : Optional.empty();
    }

    /**
     * The jar that this class was loaded from; none when it was loaded from a folder of classes,
     * as in the tests.
     */
    private static Optional<Path> jar() {
        try {
            CodeSource source = ClaferExecutable.class.getProtectionDomain().getCodeSource();
            return source == null || source.getLocation() == null
                    ? Optional.empty() : Optional.of(Path.of(source.getLocation().toURI()));
        } catch (URISyntaxException | RuntimeException e) {
            return Optional.empty();
        }
    }
}
