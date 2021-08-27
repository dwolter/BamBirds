/*****************************************************************************
 ** ANGRYBIRDS AI AGENT FRAMEWORK
 ** Copyright (c) 2014,XiaoYu (Gary) Ge, Stephen Gould,Jochen Renz
 **  Sahan Abeyasinghe, Jim Keys,   Andrew Wang, Peng Zhang
 ** All rights reserved.
**This work is licensed under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
**To view a copy of this license, visit http://www.gnu.org/licenses/
 *****************************************************************************/
package de.uniba.sme.bambirds.common.utils;

import java.awt.image.BufferedImage;

import de.uniba.sme.bambirds.common.exceptions.ServerException;

/**
 * Interface for all basic Server Messages
 */
public interface ActionRobot {


	public byte simpleMessage(byte msg) throws ServerException;
	public byte[] simpleMessage(byte msg, int responseLength) throws ServerException;
	public byte simpleMessage(byte[] msg) throws ServerException;
	public byte[] simpleMessage(byte[] msg, int responseLength) throws ServerException;

	/**
	 * Register at Server
	 * @param team_id as 4 byte array
	 * @return [round info][time limit][available levels]
	 * @throws ServerException
	 */
	public byte[] configure(byte[] team_id) throws ServerException;

	public byte setSimulationSpeed(byte[] speed) throws ServerException;

	public BufferedImage doScreenShot() throws ServerException;

	public byte getState() throws ServerException;

	public byte[] getBestScores() throws ServerException;

	public byte[] getCurrentLevel() throws ServerException;

	public byte[] getNumberOfLevels() throws ServerException;

	public byte[] getMyScore() throws ServerException;

	public byte shoot(byte[] fx, byte[] fy, byte[] dx, byte[] dy, byte[] t1, byte[] t2, boolean polar) throws ServerException;

	public byte shootFast(byte[] fx, byte[] fy, byte[] dx, byte[] dy, byte[] t1, byte[] t2, boolean polar) throws ServerException;

	public byte[] shootSequence(byte[]... shots) throws ServerException;

	public byte[] shootSequenceFast(byte[]... shots) throws ServerException;

	public byte fullyZoomOut() throws ServerException;

	public byte fullyZoomIn() throws ServerException;

	public byte clickInCenter() throws ServerException;

	public byte loadLevel(byte... i) throws ServerException;

	public byte[] selectNextLevel() throws ServerException;

	public byte restartLevel() throws ServerException;

	public byte[] getGroundTruth(boolean withScreenshot, boolean noisy) throws ServerException;

	public byte[] getCurrentLevelScore() throws ServerException;

	public byte reportNoveltyLikelihood(byte... info) throws ServerException;

	public byte[] readyForNewSet() throws ServerException;

	public byte[] requestNoveltyInformation() throws ServerException;

	public void close();

}
