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

import java.awt.image.BufferedImage;

/**
 * Interface for all basic Server Messages.
 */
public interface ActionRobot {

	/**
	 * Send a simple single-byte message to the Server.
	 *
	 * @param msg the message in one byte
	 * @return a single byte as response
	 * @throws ServerException if the server is shutting down, sends an unknown response or is the communication fails
	 */
	byte simpleMessage(byte msg) throws ServerException;

	/**
	 * Send a simple single-byte message to the Server with a multibyte response.
	 *
	 * @param msg            the message in one byte
	 * @param responseLength the length of the response
	 * @return an array of bytes as response of length responseLength
	 * @throws ServerException if the server is shutting down, sends an unknown response or is the communication fails
	 */
	byte[] simpleMessage(byte msg, int responseLength) throws ServerException;

	/**
	 * Send a simple multibyte message to the Server with a single-byte response.
	 *
	 * @param msg the message as an array of bytes
	 * @return a single byte as response
	 * @throws ServerException if the server is shutting down, sends an unknown response or is the communication fails
	 */
	byte simpleMessage(byte[] msg) throws ServerException;

	/**
	 * Send a simple multibyte message to the Server with a multibyte response.
	 *
	 * @param msg            the message as an array of bytes
	 * @param responseLength the length of the response
	 * @return an array of bytes as response of length responseLength
	 * @throws ServerException if the server is shutting down, sends an unknown response or is the communication fails
	 */
	byte[] simpleMessage(byte[] msg, int responseLength) throws ServerException;

	/**
	 * Register at Server.
	 *
	 * @param teamId as 4 byte array
	 * @return [round info][time limit][available levels]
	 * @throws ServerException if the server is shutting down, sends an unknown response or is the communication fails
	 */
	byte[] configure(byte[] teamId) throws ServerException;

	byte setSimulationSpeed(byte[] speed) throws ServerException;

	BufferedImage doScreenShot() throws ServerException;

	byte getState() throws ServerException;

	byte[] getBestScores() throws ServerException;

	byte[] getCurrentLevel() throws ServerException;

	byte[] getNumberOfLevels() throws ServerException;

	byte[] getMyScore() throws ServerException;

	byte shoot(byte[] fx, byte[] fy, byte[] dx, byte[] dy, byte[] t1, byte[] t2, boolean polar) throws ServerException;

	byte shootFast(byte[] fx, byte[] fy, byte[] dx, byte[] dy, byte[] t1, byte[] t2, boolean polar) throws ServerException;

	byte[] shootSequence(byte[]... shots) throws ServerException;

	byte[] shootSequenceFast(byte[]... shots) throws ServerException;

	byte fullyZoomOut() throws ServerException;

	byte fullyZoomIn() throws ServerException;

	byte clickInCenter() throws ServerException;

	byte loadLevel(byte... i) throws ServerException;

	byte[] selectNextLevel() throws ServerException;

	byte restartLevel() throws ServerException;

	byte[] getGroundTruth(boolean withScreenshot, boolean noisy) throws ServerException;

	byte[] getCurrentLevelScore() throws ServerException;

	byte reportNoveltyLikelihood(byte... info) throws ServerException;

	byte[] readyForNewSet() throws ServerException;

	byte[] requestNoveltyInformation() throws ServerException;

	void close();

}
