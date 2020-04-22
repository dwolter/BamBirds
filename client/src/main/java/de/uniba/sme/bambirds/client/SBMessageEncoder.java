package de.uniba.sme.bambirds.client;

import de.uniba.sme.bambirds.common.utils.Settings;

public class SBMessageEncoder extends ClientMessageEncoder {

	//encode configure message
	public static byte[] configure(byte[] id) {
		byte[] msg = new byte[1 + id.length + 1];
		msg = mergeArray(
			new byte[] { ClientMessageTable.configure.get() },
			id,
			new byte[] { Settings.GAME_MODE.getValue() }
		);
		
		return msg;
	}

	public static byte[] getCurrentLevelScore(){
		byte[] msg = {ClientMessageTable.getCurrentLevelScore.get()};
		return msg;
	}

	//encode loadlevel message
	//allow 0 or 4 input argument
	public static byte[] loadLevel(byte... level) {
		byte[] levelID = new byte[4];
		if (level.length >= 4){
			System.arraycopy(level, 0, levelID, 0, 4);
		}
		byte[] msg = mergeArray(new byte[]{
				ClientMessageTable.loadLevel.get()},
				levelID);
		return msg;
	}

}