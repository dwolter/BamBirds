package de.uniba.sme.bambirds.common.utils;

import java.io.FileWriter;
import java.io.IOException;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

/**
 * FileUtil
 */
public class FileUtil {
	private static final Logger log = LogManager.getLogger();

	public static String replacePathDelimiters(String path) {
		return path.replace("\\", "/");
	}

	public static boolean write(String filename, String content) {
		return write(filename, content, false);
	}

	public static boolean write(String filename, String content, boolean append) {
		try (FileWriter fw = new FileWriter(filename, append)) {
			fw.write(content);
		} catch (NullPointerException e) {
			log.error("Could not open file because pathname is null!",e);
			return false;
		} catch (IOException e) {
			log.error("Could not write to file " + filename,e);
			return false;
		}
		return true;
	}

}