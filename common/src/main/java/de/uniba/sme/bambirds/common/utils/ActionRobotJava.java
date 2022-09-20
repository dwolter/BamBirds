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
import de.uniba.sme.bambirds.common.utils.Settings.ServerType;

import java.util.List;

/**
 * Extension of {@link ActionRobot} to be able to provide and receive Java types or Objects.
 */
public interface ActionRobotJava extends ActionRobot {

	// A java util class for the standalone version. It provides common
	// functions an agent would use. E.g. get the screenshot

	ConfigureResponse configure(int teamId) throws ServerException;

	int getNumberOfLevelsInt() throws ServerException;

	boolean loadLevel(int i) throws ServerException;

	boolean shootSafe(Shot shot) throws ServerException;

	boolean shootFast(Shot shot) throws ServerException;

	boolean[] shootSequenceSafe(List<Shot> shots) throws ServerException;

	boolean[] shootSequenceFast(List<Shot> shots) throws ServerException;

	GameState getGameState() throws ServerException;

	int[] getBestScoresInt() throws ServerException;

	int[] getMyScoreInt() throws ServerException;

	int getCurrentLevelScoreInt() throws ServerException;

	int selectNextLevelInt() throws ServerException;

	boolean reportNoveltyLikelihood(float noveltyLikelihood, float nonNoveltyLikelihood, int[] novelObjectIDs, int noveltyLevel, String noveltyDescription) throws ServerException;

	ServerType getServerType();

}
