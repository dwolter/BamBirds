package de.uniba.sme.bambirds.common.utils;

import java.util.Arrays;

/**
 * Common Math utilities that are not present in {@link Math}.
 */
public final class MathUtil {

	private MathUtil() {
	}

	public static final double PI_2 = Math.PI / 2;
	public static final double PI_4 = Math.PI / 4;
	public static final double PI_8 = Math.PI / 8;

	public static boolean isClose(final double a, final double b, final double tolerance) {
		return Math.abs(a - b) < tolerance;
	}

	public static int round(final double i) {
		return (int) Math.round(i);
	}

	public static double harmonicMean(double... values) {
		return values.length / Arrays.stream(values).map(v -> 1/v).sum();
	}
}
