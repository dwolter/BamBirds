package de.uniba.sme.bambirds.common.utils;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.URI;
import java.net.URISyntaxException;
import java.net.URL;
import java.net.URLDecoder;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Enumeration;
import java.util.List;
import java.util.jar.JarEntry;
import java.util.jar.JarFile;
import java.util.stream.Stream;

public final class SrcFileCopy {
	private static final Logger LOG = LogManager.getLogger(SrcFileCopy.class);

	private SrcFileCopy() {
	}

	private static void copyResourceFolder(final String sourceFolder, final String destinationFolder) {
		List<String> resFiles = getFilenamesForResourceDir(sourceFolder);
		try {
			for (String fileStr : resFiles) {
				File file = new File(destinationFolder, fileStr);
				if (file.exists() && !file.delete()) {
					throw new IOException("Failed to remove existing file " + file);
				}
				File parent = file.getParentFile();
				if (!parent.mkdirs()) {
					throw new IOException("Failed to create parent directories for file " + file);
				}
				LOG.debug("Copying file: " + fileStr);

				InputStream inStream = Thread.currentThread().getContextClassLoader().getResourceAsStream(fileStr);

				if (inStream != null) {
					OutputStream outStream = new FileOutputStream(file);

					byte[] buffer = new byte[8 * 1024];
					int bytesRead;
					while ((bytesRead = inStream.read(buffer)) != -1) {
						outStream.write(buffer, 0, bytesRead);
					}
					inStream.close();
					outStream.close();
				}

			}
		} catch (IOException e) {
			LOG.error("Resource Folder could not be copied", e);
		}
	}

	private static List<String> getFilenamesForResourceDir(final String directoryName) {
		List<String> filenames = new ArrayList<>();
		try {
			URL url = Thread.currentThread().getContextClassLoader().getResource(directoryName);
			if (url != null) {
				String dirname = directoryName + "/";
				if (url.getProtocol().equals("jar")) {
					String path = url.getPath();
					String jarPath = path.substring(5, path.indexOf("!"));
					JarFile jar = new JarFile(URLDecoder.decode(jarPath, StandardCharsets.UTF_8.name()));
					Enumeration<JarEntry> entries = jar.entries();
					while (entries.hasMoreElements()) {
						JarEntry entry = entries.nextElement();
						LOG.debug("Has resource: " + entry);
						String name = entry.getName();
						if (name.startsWith(dirname) && !dirname.equals(name) && !name.endsWith("/")) {
							filenames.add(name);
						}
					}
					jar.close();
				} else if (url.getProtocol().equals("file")) {
					URI uri = url.toURI();
					Path root = Paths.get(uri);
					try (Stream<Path> paths = Files.walk(Paths.get(uri))) {
						String rootPath = root.toFile().getPath();
						paths.filter(Files::isRegularFile)
								.forEach((p) -> filenames.add(p.toString().replace(rootPath, directoryName)));
					}
				} else {
					LOG.error("Resource protocol {} not supported", url.getProtocol());
				}
			}
		} catch (IOException e) {
			LOG.error("The filenames could not be gathered", e);
		} catch (URISyntaxException e) {
			LOG.error("The resource url could not be parsed to a uri", e);
		}
		return filenames;
	}

	public static void extract(final String source, final String target) {
		LOG.debug("exporting " + source + " to " + target);
		copyResourceFolder(source, target);
	}
}
