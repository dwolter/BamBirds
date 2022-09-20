package de.uniba.sme.bambirds.common.utils;

import java.nio.ByteBuffer;
import java.nio.charset.StandardCharsets;

/**
 * Utilities for converting from java types to bytes and back.
 */
public final class ByteUtil {

	private ByteUtil() {
	}

	/**
	 * Convert a byte array to int value. Bytes after the first 4 places are ignored.
	 *
	 * @param b a byte array of at least 4 bytes
	 * @return an integer
	 * @throws java.nio.BufferUnderflowException when there are less than 4 bytes
	 */
	public static int bytesToInt(final byte... b) {
		return ByteBuffer.wrap(b).getInt();
	}

	/**
	 * convert an int value to byte[4] array.
	 *
	 * @param a an integer
	 * @return the integer as byte array
	 */
	public static byte[] intToByteArray(final int a) {
		return ByteBuffer.allocate(Integer.BYTES).putInt(a).array();
	}

	/**
	 * convert a string to a byte array using UTF-8 encoding.
	 *
	 * @param a a string
	 * @return the string as byte array
	 */
	public static byte[] stringToByteArray(final String a) {
		return a.getBytes(StandardCharsets.UTF_8);
	}

	/**
	 * convert a float to a byte array.
	 *
	 * @param a a float
	 * @return the float encoded as byte array
	 */
	public static byte[] floatToBytes(final float a) {
		return ByteBuffer.allocate(Float.BYTES).putFloat(a).array();
	}

	/**
	 * Convert an integer array to a byte array.
	 *
	 * @param a an integer array
	 * @return the integer array as one byte array
	 */
	public static byte[] intArrayToByteArray(final int[] a) {
		ByteBuffer buffer = ByteBuffer.allocate(Integer.BYTES * a.length);
		for (int b : a) {
			buffer.putInt(b);
		}
		return buffer.array();
	}
}
