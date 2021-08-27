/*****************************************************************************
 ** ANGRYBIRDS AI AGENT FRAMEWORK
 ** Copyright (c) 2014, XiaoYu (Gary) Ge, Stephen Gould, Jochen Renz
 **  Sahan Abeyasinghe,Jim Keys,  Andrew Wang, Peng Zhang
 ** All rights reserved.
**This work is licensed under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
**To view a copy of this license, visit http://www.gnu.org/licenses/
 *****************************************************************************/

package de.uniba.sme.bambirds.client;

import java.util.List;

import org.apache.commons.lang3.NotImplementedException;

import de.uniba.sme.bambirds.common.utils.ActionRobotJava;
import de.uniba.sme.bambirds.common.utils.ByteUtil;
import de.uniba.sme.bambirds.common.utils.Settings.ServerType;
import de.uniba.sme.bambirds.common.exceptions.ServerException;
import de.uniba.sme.bambirds.common.objects.GameState;
import de.uniba.sme.bambirds.common.objects.Shot;

//Java interface of ClientActionRobot
public class ABActionRobotJava extends ABActionRobot implements ActionRobotJava {

	public ABActionRobotJava(String ip) throws ServerException {
		super(ip);
	}

	// return game state as enum format
	@Override
	public GameState getGameState() throws ServerException {
		byte result = super.getState();
		GameState state = GameState.values()[result];
		return state;
	}

	// return an array of best scores. the nth slot stores the score of (n+1)th
	// level
	@Override
	public int[] getBestScoresInt() throws ServerException {
		byte[] scores = super.getBestScores();
		int[] _scores = new int[scores.length / 4];
		for (int i = 0; i < _scores.length; i++) {
			_scores[i] = ByteUtil.bytesToInt(scores[i * 4], scores[i * 4 + 1], scores[i * 4 + 2], scores[i * 4 + 3]);
		}
		return _scores;
	}

	// send a shot message using int values as input
	@Override
	public byte shootSafe(Shot shot) throws ServerException {
		return shoot(ByteUtil.intToByteArray(shot.getSlingX())/* fx */, ByteUtil.intToByteArray(shot.getSlingY())/* fy */,
				ByteUtil.intToByteArray(shot.getDragX())/* dx */, ByteUtil.intToByteArray(shot.getDragY())/* dy */,
				ByteUtil.intToByteArray((int) shot.getShotTime())/* t1 */, ByteUtil.intToByteArray((int) shot.getTapTime())/* t2 */,
				false);
	}

	@Override
	public byte shootFast(Shot shot) throws ServerException {
		return shootFast(ByteUtil.intToByteArray(shot.getSlingX())/* fx */, ByteUtil.intToByteArray(shot.getSlingY())/* fy */,
				ByteUtil.intToByteArray(shot.getDragX())/* dx */, ByteUtil.intToByteArray(shot.getDragY())/* dy */,
				ByteUtil.intToByteArray((int) shot.getShotTime())/* t1 */, ByteUtil.intToByteArray((int) shot.getTapTime())/* t2 */,
				false);
	}

	// send a shot sequence message using int arrays as input
	// one array per shot
	@Override
	public byte[] shootSequenceSafe(List<Shot> shots) throws ServerException {
		byte[][] byteShots = new byte[shots.size()][24];
		int shotCount = 0;
		for (Shot shot : shots) {
			byteShots[shotCount] = ClientMessageEncoder.mergeArray(ByteUtil.intToByteArray(shot.getSlingX())/* fx */,
					ByteUtil.intToByteArray(shot.getSlingY())/* fy */, ByteUtil.intToByteArray(shot.getDragX())/* dx */,
					ByteUtil.intToByteArray(shot.getDragY())/* dy */, ByteUtil.intToByteArray((int) shot.getShotTime())/* t1 */,
					ByteUtil.intToByteArray((int) shot.getTapTime())/* t2 */);
			shotCount++;
		}
		return shootSequence(byteShots);
	}

	public int[] getMyScoreInt() throws ServerException {
		byte[] scores = super.getMyScore();
		int[] _scores = new int[scores.length / 4];
		for (int i = 0; i < _scores.length; i++) {
			_scores[i] = ByteUtil.bytesToInt(scores[i * 4], scores[i * 4 + 1], scores[i * 4 + 2], scores[i * 4 + 3]);
		}
		return _scores;
	}

	@Override
	public byte[] configure(int team_id) throws ServerException {
		return super.configure(ByteUtil.intToByteArray(team_id));
	}

	@Override
	public byte loadLevel(int i) throws ServerException {
		return super.loadLevel((byte)i);
	}

	@Override
	public byte[] shootSequenceFast(List<Shot> shots) {
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
	public byte reportNoveltyLikelihood(float noveltyLikelihood, float nonNoveltyLikelihood, int[] novelObjectIDs,
			int noveltyLevel, String noveltyDescription) throws ServerException {
		throw new UnsupportedOperationException("Not supported by ABServer");
	}

	@Override
	public ServerType getServerType() {
		return ServerType.ANGRY_BIRDS;
	}
}
