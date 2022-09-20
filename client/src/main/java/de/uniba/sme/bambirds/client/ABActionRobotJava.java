/*****************************************************************************
 ** ANGRYBIRDS AI AGENT FRAMEWORK
 ** Copyright (c) 2014, XiaoYu (Gary) Ge, Stephen Gould, Jochen Renz
 **  Sahan Abeyasinghe,Jim Keys,  Andrew Wang, Peng Zhang
 ** All rights reserved.
**This work is licensed under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
**To view a copy of this license, visit http://www.gnu.org/licenses/
 *****************************************************************************/

package de.uniba.sme.bambirds.client;

import java.util.Arrays;
import java.util.List;

import de.uniba.sme.bambirds.common.utils.ConfigureResponse;
import org.apache.commons.lang3.NotImplementedException;

import de.uniba.sme.bambirds.common.utils.ActionRobotJava;
import de.uniba.sme.bambirds.common.utils.ByteUtil;
import de.uniba.sme.bambirds.common.utils.Settings.ServerType;
import de.uniba.sme.bambirds.common.exceptions.ServerException;
import de.uniba.sme.bambirds.common.objects.GameState;
import de.uniba.sme.bambirds.common.objects.Shot;

//Java interface of ClientActionRobot
public class ABActionRobotJava extends ABActionRobot implements ActionRobotJava {

	public ABActionRobotJava(final String... args) throws ServerException {
		super(args);
	}

	// return game state as enum format
	@Override
	public GameState getGameState() throws ServerException {
		byte result = super.getState();
		return GameState.values()[result];
	}

	// return an array of best scores. the nth slot stores the score of (n+1)th
	// level
	@Override
	public int[] getBestScoresInt() throws ServerException {
		byte[] scores = super.getBestScores();
		int[] intScores = new int[scores.length / Integer.BYTES];
		for (int i = 0; i < intScores.length; i++) {
			intScores[i] = ByteUtil.bytesToInt(Arrays.copyOfRange(scores, i * Integer.BYTES, Integer.BYTES));
		}
		return intScores;
	}

	// send a shot message using int values as input
	@Override
	public boolean shootSafe(final Shot shot) throws ServerException {
		return shoot(ByteUtil.intToByteArray(shot.getSlingX())/* fx */, ByteUtil.intToByteArray(shot.getSlingY())/* fy */,
				ByteUtil.intToByteArray(shot.getDragX())/* dx */, ByteUtil.intToByteArray(shot.getDragY())/* dy */,
				ByteUtil.intToByteArray((int) shot.getShotTime())/* t1 */, ByteUtil.intToByteArray((int) shot.getTapTime())/* t2 */,
				false) == 1;
	}

	@Override
	public boolean shootFast(final Shot shot) throws ServerException {
		return shootFast(ByteUtil.intToByteArray(shot.getSlingX())/* fx */, ByteUtil.intToByteArray(shot.getSlingY())/* fy */,
				ByteUtil.intToByteArray(shot.getDragX())/* dx */, ByteUtil.intToByteArray(shot.getDragY())/* dy */,
				ByteUtil.intToByteArray((int) shot.getShotTime())/* t1 */, ByteUtil.intToByteArray((int) shot.getTapTime())/* t2 */,
				false) == 1;
	}

	// send a shot sequence message using int arrays as input
	// one array per shot
	@Override
	public boolean[] shootSequenceSafe(final List<Shot> shots) throws ServerException {
		byte[][] byteShots = new byte[shots.size()][];
		int shotCount = 0;
		for (Shot shot : shots) {
			byteShots[shotCount] = ClientMessageEncoder.mergeArrays(ByteUtil.intToByteArray(shot.getSlingX())/* fx */,
					ByteUtil.intToByteArray(shot.getSlingY())/* fy */, ByteUtil.intToByteArray(shot.getDragX())/* dx */,
					ByteUtil.intToByteArray(shot.getDragY())/* dy */, ByteUtil.intToByteArray((int) shot.getShotTime())/* t1 */,
					ByteUtil.intToByteArray((int) shot.getTapTime())/* t2 */);
			shotCount++;
		}
		boolean[] succeeded = new boolean[shots.size()];
		shotCount = 0;
		for (byte result : shootSequence(byteShots)) {
			succeeded[shotCount] = result == 1;
			shotCount++;
		}
		return succeeded;
	}

	@Override
	public int[] getMyScoreInt() throws ServerException {
		byte[] scores = super.getMyScore();
		int[] intScores = new int[scores.length / Integer.BYTES];
		for (int i = 0; i < intScores.length; i++) {
			intScores[i] = ByteUtil.bytesToInt(Arrays.copyOfRange(scores, i * Integer.BYTES, Integer.BYTES));
		}
		return intScores;
	}

	@Override
	public ConfigureResponse configure(final int teamId) throws ServerException {
		byte[] result = super.configure(ByteUtil.intToByteArray(teamId));
		return new ConfigureResponse(result[0], result[1], result[2]);
	}

	@Override
	public int getNumberOfLevelsInt() throws ServerException {
		throw new UnsupportedOperationException("ABServer does not support getting the number of levels");
	}

	@Override
	public boolean loadLevel(final int i) throws ServerException {
		return super.loadLevel((byte) i) == 1;
	}

	@Override
	public boolean[] shootSequenceFast(final List<Shot> shots) {
		throw new NotImplementedException("Not implemented yet");
	}

	@Override
	public int getCurrentLevelScoreInt() throws ServerException {
		return ByteUtil.bytesToInt(getCurrentLevelScore());
	}

	@Override
	public int selectNextLevelInt() throws ServerException {
		throw new UnsupportedOperationException("Not supported by ABServer");
	}

	@Override
	public boolean reportNoveltyLikelihood(final float noveltyLikelihood, final float nonNoveltyLikelihood, final int[] novelObjectIDs,
																				 final int noveltyLevel, final String noveltyDescription) throws ServerException {
		throw new UnsupportedOperationException("Not supported by ABServer");
	}

	@Override
	public ServerType getServerType() {
		return ServerType.ANGRY_BIRDS;
	}
}
