package de.uniba.sme.bambirds.common.utils;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.io.FileWriter;
import java.io.IOException;
import java.nio.file.Path;
import java.nio.file.Paths;

/**
 * Utilities for saving files.
 */
public final class FileUtil {

	private FileUtil() {
	}

	private static final Logger LOG = LogManager.getLogger();

	public static String replacePathDelimiters(final String path) {
		return path.replace("\\", "/");
	}

	public static Path writeTemp(final String filename, final String content) {
		return writeTemp(filename, content, false);
	}
	
	public static Path writeTemp(final String filename, final String content, final boolean append) {
		return write(Settings.TEMP_DIR.resolve(filename).toString(), content, append);
	}

	public static Path write(final String filepath, final String content) {
		return write(filepath, content, false);
	}

	public static Path write(final String filepath, final String content, final boolean append) {
		Path filePath = Paths.get(filepath).toAbsolutePath().normalize();
		try (FileWriter fw = new FileWriter(filePath.toFile(), append)) {
			fw.write(content);
		} catch (NullPointerException e) {
			LOG.error("Could not open file because pathname is null!", e);
			return null;
		} catch (IOException e) {
			LOG.error("Could not write to file " + filepath, e);
			return null;
		}
		return filePath;
	}

}
