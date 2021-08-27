package de.uniba.sme.bambirds.common.utils;

/**
 * Common Math functions
 */
public class MathUtil {

	public static boolean isclose(double a, double b, double tolerance) {
		return Math.abs(a - b) < tolerance;
	}
}
