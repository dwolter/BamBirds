package de.uniba.sme.bambirds.common.utils;

/**
 * FileUtil
 */
public class FileUtil {

	public static String replacePathDelimiters(String path) {
		return path.replace("\\", "/");
	}
}