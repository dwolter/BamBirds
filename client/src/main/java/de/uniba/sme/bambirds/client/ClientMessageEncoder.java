/*****************************************************************************
 ** ANGRYBIRDS AI AGENT FRAMEWORK
 ** Copyright (c) 2014, XiaoYu (Gary) Ge, Stephen Gould, Jochen Renz
 **  Sahan Abeyasinghe,Jim Keys,  Andrew Wang, Peng Zhang
 ** All rights reserved.
**This work is licensed under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
**To view a copy of this license, visit http://www.gnu.org/licenses/
 *****************************************************************************/

package de.uniba.sme.bambirds.client;

/** encode the messages to byte arrays. */
public class ClientMessageEncoder {

	protected ClientMessageEncoder() {	}

	protected static byte[] encodeMessageID(final ClientMessageTable id) {
		return new byte[] {id.get()};
	};

	//encode screenshot message
	public static byte[] encodeDoScreenShot() {
		return encodeMessageID(ClientMessageTable.doScreenShot);
	}

	//encode configure message
	public static byte[] configure(final byte[] id) {
		return mergeArrays(
			encodeMessageID(ClientMessageTable.configure),
				id);
	}

	//encode loadlevel message
	//allow 0 or 1 input argument
	public static byte[] loadLevel(final byte... level) {
		return new byte[]{
				ClientMessageTable.loadLevel.get(),
				((level.length == 0) ? 0 : level[0]) };
	}

	//encode restart message
	public static byte[] restart() {
		return encodeMessageID(ClientMessageTable.restartLevel);
	}

	//encode cshoot message (safe mode)
	public static byte[] cShoot(final byte[] fx, final byte[] fy, final byte[] dx,
															final byte[] dy, final byte[] t1, final byte[] t2) {
		return mergeArrays(
			encodeMessageID(ClientMessageTable.cShoot),
				fx, fy, dx, dy, t1, t2);
	}

	//encode pshoot message (safe mode)
	public static byte[] pShoot(final byte[] fx, final byte[] fy, final byte[] dx,
															final byte[] dy, final byte[] t1, final byte[] t2) {
		return mergeArrays(
						encodeMessageID(ClientMessageTable.pShoot),
						fx, fy, dx, dy, t1, t2);
	}
	//encode pshoot message in fast mode
	public static byte[] pFastShoot(final byte[] fx, final byte[] fy, final byte[] dx,
																	final byte[] dy, final byte[] t1, final byte[] t2) {
		return mergeArrays(
						encodeMessageID(ClientMessageTable.pFastShoot),
						fx, fy, dx, dy, t1, t2);
	}
	//encode cshoot message in fast mode
	public static byte[] cFastShoot(final byte[] fx, final byte[] fy, final byte[] dx,
																	final byte[] dy, final byte[] t1, final byte[] t2) {
		byte[] message = new byte[1 + fx.length + fy.length + dx.length
				+ dy.length + t1.length + t2.length];
		message = mergeArrays(
			encodeMessageID(ClientMessageTable.cFastShoot),
				fx, fy, dx, dy, t1, t2);
		return message;

	}

	/** encode fully zoom out message.
	 * @return the byte encoded message
	 */
	public static byte[] fullyZoomOut() {
		return encodeMessageID(ClientMessageTable.fullyZoomOut);
	}

	/** encode fully zoom in message.
	 * @return the byte encoded message
	 */
	public static byte[] fullyZoomIn() {
		return encodeMessageID(ClientMessageTable.fullyZoomIn);
	}

	/** encode click in center message.
	 * @return the byte encoded message
	 */
	public static byte[] clickInCenter() {
		return encodeMessageID(ClientMessageTable.clickInCenter);
	}

	/** encode get state message.
	 * @return the byte encoded message
	 */
	public static byte[] getState() {
		return encodeMessageID(ClientMessageTable.getState);
	}

	/** encode get best scores message.
	 * @return the byte encoded message
	 */
	public static byte[] getBestScores() {
		return encodeMessageID(ClientMessageTable.getBestScores);
	}

	/** encode get my score message.
	 * @return the byte encoded message
	 */
	public static byte[] getMyScore() {
		return encodeMessageID(ClientMessageTable.getMyScore);
	}

	/** encode get current level message.
	 * @return the byte encoded message
	 */
	public static byte[] getCurrentLevel() {
		return encodeMessageID(ClientMessageTable.getCurrentLevel);
	}

	/** encode get number of levels message.
	 * @return the byte encoded message
	 */
	public static byte[] getNumberOfLevels() {
		return encodeMessageID(ClientMessageTable.getNumberOfLevels);
	}

	/** encode ready for new set message.
	 * @return the byte encoded message
	 */
	public static byte[] readyForNewSet() {
		return encodeMessageID(ClientMessageTable.readyForNewSet);
	}

	/** encode request novelty information message.
	 * @return the byte encoded message
	 */
	public static byte[] requestNoveltyInformation() {
		return encodeMessageID(ClientMessageTable.requestNoveltyInformation);
	}

	/** merge byte arrays into one array.
	 * @param arrays arrays to merge
	 * @return a merged array
	 */
	public static byte[] mergeArrays(final byte[]... arrays) {
		int size = 0;
		for (byte[] a : arrays) {
			size += a.length;
		}
		byte[] res = new byte[size];

		int destPos = 0;
		for (int i = 0; i < arrays.length; i++) {
			if (i > 0) {
				destPos += arrays[i - 1].length;
			}
			int length = arrays[i].length;
			System.arraycopy(arrays[i], 0, res, destPos, length);
		}
		return res;
	}
}
