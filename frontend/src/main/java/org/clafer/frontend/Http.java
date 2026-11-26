package org.clafer.frontend;

import java.io.IOException;
import java.io.InputStream;
import java.net.ConnectException;
import java.net.Inet4Address;
import java.net.InetAddress;
import java.net.NoRouteToHostException;
import java.net.Proxy;
import java.net.SocketTimeoutException;
import java.net.URI;
import java.net.URISyntaxException;
import java.net.UnknownHostException;
import java.nio.charset.StandardCharsets;
import java.time.Duration;
import java.util.List;
import java.util.Locale;
import java.util.Optional;
import java.util.concurrent.TimeUnit;
import java.util.regex.Pattern;
import javax.net.ssl.SSLException;
import okhttp3.ConnectionPool;
import okhttp3.Dns;
import okhttp3.HttpUrl;
import okhttp3.OkHttpClient;
import okhttp3.Protocol;
import okhttp3.Request;
import okhttp3.Response;
import okhttp3.ResponseBody;

/**
 * Downloads a model file from an {@code http} or {@code https} URL, as the
 * compiler does ({@code Import.readLocation}): a GET without a proxy, which
 * follows up to 10 redirects; a status other than 2xx is an error; the body
 * may have at most 16 MB, and is decoded as UTF-8, a malformed byte as
 * U+FFFD. An import must still name a {@code .cfr} file after the redirects,
 * and must not be served as HTML, JSON or XML ({@link #isModelContentType}).
 * <p>
 * With {@code publicOnly} ({@code --public-imports}), each connection, also
 * after a redirect, is made to the first public IPv4 address of the host
 * ({@link #isPublic}), and to no other, so that a model cannot make the
 * server read from its own network: the client's {@link Dns} gives OkHttp
 * that address alone, and a host written as an address, which OkHttp does
 * not look up, is checked before the request.
 * <p>
 * The client is OkHttp, with HTTP/1.1 only, as the compiler's
 * {@code http-client}, so that a status has its reason phrase; the redirects
 * are followed here, to check each one.
 */
final class Http {

    private Http() {
    }

    /**
     * Why a URL could not be read, in the words of the compiler's message
     * after {@code cannot read URL: }.
     */
    static final class Failure extends Exception {

        private static final long serialVersionUID = 1L;

        Failure(String problem) {
            super(problem, null, false, false);
        }
    }

    /**
     * The largest file that is downloaded.
     */
    static final int MaxDownload = 16 * 1024 * 1024;

    private static final int MaxRedirects = 10;

    private static final Duration Timeout = Duration.ofSeconds(30);

    /**
     * A host that OkHttp takes as an address and does not look up
     * ({@code canParseAsIpAddress}).
     */
    private static final Pattern AddressHost = Pattern.compile("([0-9a-fA-F]*:[0-9a-fA-F:.]*)|([\\d.]+)");

    /**
     * The lookup of {@code --public-imports} failed: the host has no public
     * IPv4 address.
     */
    private static final class NoPublicAddress extends UnknownHostException {

        private static final long serialVersionUID = 1L;

        NoPublicAddress(String host) {
            super("the host " + host + " has no public address");
        }
    }

    /**
     * The first public IPv4 address of the host, alone.
     */
    private static final Dns PublicDns = host -> {
        for (InetAddress a : Dns.SYSTEM.lookup(host)) {
            if (a instanceof Inet4Address v4 && isPublic(v4)) {
                return List.of(a);
            }
        }
        throw new NoPublicAddress(host);
    };

    /**
     * The clients, made when a URL is first read.
     */
    private static final class Clients {

        static final OkHttpClient Any = client(Dns.SYSTEM);
        static final OkHttpClient PublicOnly = client(PublicDns);

        private static OkHttpClient client(Dns dns) {
            return new OkHttpClient.Builder()
                    .proxy(Proxy.NO_PROXY)
                    .dns(dns)
                    .protocols(List.of(Protocol.HTTP_1_1))
                    .followRedirects(false)
                    .followSslRedirects(false)
                    .retryOnConnectionFailure(false)
                    // no idle connections, so that nothing is left running
                    .connectionPool(new ConnectionPool(0, 1, TimeUnit.SECONDS))
                    .connectTimeout(Timeout)
                    .readTimeout(Timeout)
                    .writeTimeout(Timeout)
                    .build();
        }
    }

    /**
     * @param url an absolute http or https URL
     * @param isImport whether it is an import, which must be a {@code .cfr}
     * file served as a model
     * @param publicOnly whether to connect to public addresses only
     * @return the text
     * @throws Failure if it cannot be read
     */
    static String get(URI url, boolean isImport, boolean publicOnly) throws Failure {
        URI current = url;
        for (int redirects = 0;; redirects++) {
            HttpUrl httpUrl = HttpUrl.parse(current.toString());
            if (httpUrl == null) {
                throw new Failure("InvalidUrlException " + current + " Invalid URL");
            }
            if (publicOnly && AddressHost.matcher(httpUrl.host()).matches()) {
                checkAddress(httpUrl.host());
            }
            Request request = new Request.Builder().url(httpUrl).header("User-Agent", "clafer-frontend")
                    .header("Accept", "*/*").build();
            OkHttpClient client = publicOnly ? Clients.PublicOnly : Clients.Any;
            try (Response r = client.newCall(request).execute()) {
                Optional<String> location = Optional.ofNullable(r.header("Location"));
                if (List.of(301, 302, 303, 307, 308).contains(r.code()) && location.isPresent()) {
                    if (redirects == MaxRedirects) {
                        throw new Failure("TooManyRedirects");
                    }
                    try {
                        current = current.resolve(new URI(escape(location.get().trim())));
                    } catch (URISyntaxException | IllegalArgumentException e) {
                        throw new Failure("InvalidDestinationHost " + location.get());
                    }
                    continue;
                }
                if (r.code() < 200 || r.code() >= 300) {
                    throw new Failure("HTTP " + r.code() + " " + r.message());
                }
                if (isImport && !ModelFiles.isModelPath(path(current))) {
                    throw new Failure("redirected to " + current + ", which is not a .cfr file");
                }
                Optional<String> contentType = Optional.ofNullable(r.header("Content-Type"));
                if (isImport && !isModelContentType(contentType)) {
                    throw new Failure("it is served as " + contentType.orElse("") + ", not as a Clafer model");
                }
                byte[] body = bounded(r.body());
                if (body == null) {
                    throw new Failure("larger than " + MaxDownload / (1024 * 1024) + " MB");
                }
                return new String(body, StandardCharsets.UTF_8);
            } catch (NoPublicAddress e) {
                throw new Failure(e.getMessage());
            } catch (SocketTimeoutException e) {
                String m = String.valueOf(e.getMessage()).toLowerCase(Locale.ROOT);
                throw new Failure(m.contains("connect") ? "ConnectionTimeout" : "ResponseTimeout");
            } catch (ConnectException | UnknownHostException | NoRouteToHostException e) {
                throw new Failure("does not exist");
            } catch (SSLException e) {
                throw new Failure("TLS: " + e.getMessage());
            } catch (IOException e) {
                throw new Failure(String.valueOf(e.getMessage()));
            }
        }
    }

    /**
     * Checks a host written as an address, for {@code --public-imports}.
     */
    private static void checkAddress(String host) throws Failure {
        try {
            if (InetAddress.getByName(host) instanceof Inet4Address v4 && isPublic(v4)) {
                return;
            }
        } catch (UnknownHostException e) {
            // not an address after all
        }
        throw new Failure("the host " + host + " has no public address");
    }

    /**
     * The body, or null if it is larger than {@link #MaxDownload}; only that
     * much is read.
     */
    private static byte[] bounded(ResponseBody body) throws IOException {
        try (InputStream in = body.byteStream()) {
            byte[] bytes = in.readNBytes(MaxDownload + 1);
            return bytes.length > MaxDownload ? null : bytes;
        }
    }

    /**
     * @return the path of the URL, unescaped
     */
    static String path(URI url) {
        return url.getPath() == null ? "" : url.getPath();
    }

    /**
     * Whether a Content-Type may be a Clafer model: not HTML, JSON or XML,
     * which web pages and services are served as, whatever their URLs.
     */
    static boolean isModelContentType(Optional<String> value) {
        if (value.isEmpty()) {
            return true;
        }
        String v = value.get();
        int semi = v.indexOf(';');
        String type = (semi < 0 ? v : v.substring(0, semi)).strip().toLowerCase(Locale.ROOT);
        return !(List.of("text/html", "application/xhtml+xml", "application/json", "text/json", "application/xml",
                "text/xml").contains(type) || type.endsWith("+json") || type.endsWith("+xml"));
    }

    /**
     * Whether an IPv4 address is public: not unspecified, loopback, private,
     * shared, link-local, reserved, benchmarking, multicast or broadcast
     * ({@code Import.isPublicAddress}).
     */
    static boolean isPublic(Inet4Address address) {
        byte[] b = address.getAddress();
        int a0 = b[0] & 0xff;
        int a1 = b[1] & 0xff;
        int a2 = b[2] & 0xff;
        boolean notPublic = a0 == 0 || a0 == 10 || (a0 == 100 && a1 >= 64 && a1 < 128) || a0 == 127
                || (a0 == 169 && a1 == 254) || (a0 == 172 && a1 >= 16 && a1 < 32) || (a0 == 192 && a1 == 0 && a2 == 0)
                || (a0 == 192 && a1 == 0 && a2 == 2) || (a0 == 192 && a1 == 168) || (a0 == 198 && (a1 == 18 || a1 == 19))
                || (a0 == 198 && a1 == 51 && a2 == 100) || (a0 == 203 && a1 == 0 && a2 == 113) || a0 >= 224;
        return !notPublic;
    }

    /**
     * The characters that a URI may not contain percent-encoded, as
     * {@code escapeURIString isUnescapedInURI} does.
     */
    static String escape(String s) {
        StringBuilder sb = new StringBuilder();
        for (byte b : s.getBytes(StandardCharsets.UTF_8)) {
            int c = b & 0xff;
            if (c < 0x80 && (Character.isLetterOrDigit(c) || "-._~:/?#[]@!$&'()*+,;=%".indexOf(c) >= 0)) {
                sb.append((char) c);
            } else {
                sb.append('%').append(String.format("%02X", c));
            }
        }
        return sb.toString();
    }
}
