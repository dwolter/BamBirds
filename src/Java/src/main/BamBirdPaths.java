package main;

import java.nio.file.Paths;

public final class BamBirdPaths {
	public static final String PROLOG_FILE_EXTENSION = ".pl";
	public static final String PROLOG_FUNCTIONS = (
			Paths.get("Prolog/old/functions.pl")).toAbsolutePath().normalize().toString();
	public static final String NEW_PROLOG_FUNCTIONS = (
			Paths.get("Prolog/new/ShotCandidatePlanner.pl")).toAbsolutePath().normalize().toString();

	public static String replacePathDelimiters(String path) {
		return path.replace("\\", "/");
	}
}
