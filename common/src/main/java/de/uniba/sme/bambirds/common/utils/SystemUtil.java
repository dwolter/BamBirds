package de.uniba.sme.bambirds.common.utils;

import java.util.Map;

public final class SystemUtil {

	private SystemUtil() {
	}

	public static String[] envMapToArray(final Map<String, String> env) {
		String[] result = new String[env.size()];
		int index = 0;
		for (Map.Entry<String, String> entry : env.entrySet()) {
			result[index] = entry.getKey() + "=" + entry.getValue();
			index++;
		}
		return result;
	}
}
