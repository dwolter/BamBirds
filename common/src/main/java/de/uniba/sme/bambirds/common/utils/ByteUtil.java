package de.uniba.sme.bambirds.common.utils;

import java.nio.ByteBuffer;
import java.nio.charset.Charset;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

/**
 * ByteUtil
 */
public class ByteUtil {
	private static final Logger log = LogManager.getLogger();

	// convert a byte[4] array to int value
	public static int bytesToInt(byte... b) {
		return ByteBuffer.wrap(b).getInt();
	}

	// convert an int value to byte[4] array
	public static byte[] intToByteArray(int a) {
		return ByteBuffer.allocate(Integer.BYTES).putInt(a).array();
	}

	// convert an int value to byte[4] array
	public static byte[] stringToByteArray(String a) {
		return a.getBytes(Charset.forName("UTF-8"));
	}

	public static byte[] floatToBytes(float a) {
		return ByteBuffer.allocate(Float.BYTES).putFloat(a).array();
	}

	public static byte[] intArrayToByteArray(int[] a) {
		ByteBuffer buffer = ByteBuffer.allocate(Integer.BYTES * a.length);
		for(int b : a) {
			buffer.putInt(b);
		}
		return buffer.array();
	}
}