package de.uniba.sme.bambirds.client;

import de.uniba.sme.bambirds.common.utils.ByteUtil;
import de.uniba.sme.bambirds.common.utils.Settings;

public class SBMessageEncoder extends ClientMessageEncoder {

	// encode configure message
	public static byte[] configure(byte[] id) {
		byte[] msg = new byte[1 + id.length + 1];
		msg = mergeArrays(encodeMessageID(ClientMessageTable.configure), id,
				new byte[] { Settings.GAME_MODE.getValue() });

		return msg;
	}

	public static byte[] getCurrentLevelScore() {
		return encodeMessageID(ClientMessageTable.getCurrentLevelScore);
	}

	// encode loadlevel message
	// allow 0 or 4 input argument
	public static byte[] loadLevel(byte... level) {
		byte[] levelID = new byte[4];
		if (level.length >= 4) {
			System.arraycopy(level, 0, levelID, 0, 4);
		}
		byte[] msg = mergeArrays(
			encodeMessageID(ClientMessageTable.loadLevel), levelID);
		return msg;
	}

	public static byte[] readyForNewSet() {
		return encodeMessageID(ClientMessageTable.readyForNewSet);
	}

	public static byte[] requestNoveltyInformation() {
		return encodeMessageID(ClientMessageTable.requestNoveltyInformation);
	}

	public static byte[] selectNextLevel() {
		return encodeMessageID(ClientMessageTable.selectNextLevel);
	}

	public static byte[] reportNoveltyLikelihood(float noveltyLikelihood, float nonNoveltyLikelihood,
			int[] novelObjectIDs, int noveltyLevel, String noveltyDescription) {

		byte[] noveltyDescriptionBytes = ByteUtil.stringToByteArray(noveltyDescription);
		byte[] noveltyDescriptionBytesLength = 
		ByteUtil.intToByteArray(noveltyDescriptionBytes.length);

		byte[] message = mergeArrays(
			encodeMessageID(ClientMessageTable.reportNoveltyLikelihood),
			ByteUtil.floatToBytes(noveltyLikelihood),
			ByteUtil.floatToBytes(nonNoveltyLikelihood),
			ByteUtil.intToByteArray(novelObjectIDs.length),
			ByteUtil.intArrayToByteArray(novelObjectIDs),
			ByteUtil.intToByteArray(noveltyLevel),
			noveltyDescriptionBytesLength,
			noveltyDescriptionBytes,
			noveltyDescriptionBytesLength
		);
		
		// new byte[4 + 4 + 4 + novelObjectIDs.length * 4 + 4 + noveltyDescriptionBytes.length + 4];
		return message;
	}

	public static byte[] cshoot(byte[] dx, byte[] dy, byte[] t1, byte[] t2) {
		return mergeArrays(
			encodeMessageID(ClientMessageTable.cShoot),
			dx, dy, t1, t2);
	}

	public static byte[] cFastshoot(byte[] dx, byte[] dy, byte[] t1, byte[] t2) {
		return mergeArrays(
			encodeMessageID(ClientMessageTable.cFastShoot),
				dx, dy, t1, t2);
	}

}