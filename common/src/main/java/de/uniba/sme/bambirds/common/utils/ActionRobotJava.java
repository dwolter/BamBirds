/*****************************************************************************
 ** ANGRYBIRDS AI AGENT FRAMEWORK
 ** Copyright (c) 2014,XiaoYu (Gary) Ge, Stephen Gould,Jochen Renz
 **  Sahan Abeyasinghe, Jim Keys,   Andrew Wang, Peng Zhang
 ** All rights reserved.
**This work is licensed under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
**To view a copy of this license, visit http://www.gnu.org/licenses/
 *****************************************************************************/
package de.uniba.sme.bambirds.common.utils;

import de.uniba.sme.bambirds.common.exceptions.ServerException;
import de.uniba.sme.bambirds.common.objects.GameState;
import de.uniba.sme.bambirds.common.objects.Shot;

import java.util.List;

/**
 * Util class for basic functions
 *
 */
public interface ActionRobotJava extends ActionRobot {

	// A java util class for the standalone version. It provides common
	// functions an agent would use. E.g. get the screenshot

	public byte[] configure(int team_id) throws ServerException;

	public byte loadLevel(int i) throws ServerException;

	public byte shootSafe(Shot shot) throws ServerException;

	public byte shootFast(Shot shot) throws ServerException;

	public byte[] shootSequenceSafe(List<Shot> shots) throws ServerException;

	public byte[] shootSequenceFast(List<Shot> shots) throws ServerException;

	public GameState getGameState() throws ServerException;

	public int[] getBestScoresInt() throws ServerException;

	public int[] getMyScoreInt() throws ServerException;

}
