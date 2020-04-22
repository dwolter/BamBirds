/*****************************************************************************
** ANGRYBIRDS AI AGENT FRAMEWORK
** Copyright (c) 2014, XiaoYu (Gary) Ge, Jochen Renz, Stephen Gould,
**  Sahan Abeyasinghe,Jim Keys,   Andrew Wang, Peng Zhang
** All rights reserved.
**This work is licensed under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
**To view a copy of this license, visit http://www.gnu.org/licenses/
*****************************************************************************/

package de.uniba.sme.bambirds.client;

/**
 * This class maintains all the client messages and its corresponding MIDs.
 */
public enum ClientMessageTable {
	configure(1), 
	setSimulationSpeed(2), 
	doScreenShot(11),
	getState(12), 
	getBestScores(13), 
	getCurrentLevel(14),
	getNumberOfLevels(15),
	getMyScore(23),
	cshoot(31), 
	pshoot(32),
	shootSeq(33),
	fullyZoomOut(34),
	fullyZoomIn(35), 
	clickInCentre(36),
	cFastshoot(41), 
	pFastshoot(42), 
	shootSeqFast(43),
	loadLevel(51),
	restartLevel(52),
	getGTWithScreenshot(61),
	getGTWithoutScreenshot(62),
	getNoisyGTWithScreenshot(63),
	getNoisyGTWithoutScreenshot(64),
	getCurrentLevelScore(65);

	private int message_code;

	private ClientMessageTable(int message_code) {
		this.message_code = message_code;
	}

	// map message from int to enum
	public static ClientMessageTable getType(int message_code) {
		switch (message_code) {
			case 1:
				return configure;
			case 2:
				return setSimulationSpeed;
			case 11:
				return doScreenShot;
			case 12:
				return getState;
			case 13:
				return getBestScores;
			case 14:
				return getCurrentLevel;
			case 15:
				return getNumberOfLevels;
			case 23:
				return getMyScore;
			case 31:
				return cshoot;
			case 32:
				return pshoot;
			case 33:
				return shootSeq;
			case 34:
				return fullyZoomOut;
			case 35:
				return fullyZoomIn;
			case 36:
				return clickInCentre;
			case 41:
				return cFastshoot;
			case 42:
				return pFastshoot;
			case 43:
				return shootSeqFast;
			case 51:
				return loadLevel;
			case 52:
				return restartLevel;
			case 61:
				return getGTWithScreenshot;
			case 62:
				return getGTWithoutScreenshot;
			case 63:
				return getNoisyGTWithScreenshot;
			case 64:
				return getNoisyGTWithoutScreenshot;
			case 65:
				return getCurrentLevelScore;
		}
		return null;
	}

	// map message from enum to byte
	public static byte getValue(ClientMessageTable message) {
		switch (message) {
			case configure:
				return 1;
			case setSimulationSpeed:
				return 2;
			case doScreenShot:
				return 11;
			case getState:
				return 12;
			case getBestScores:
				return 13;
			case getCurrentLevel:
				return 14;
			case getNumberOfLevels:
				return 15;
			case getMyScore:
				return 23;
			case cshoot:
				return 31;
			case pshoot:
				return 32;
			case shootSeq:
				return 33;
			case fullyZoomOut:
				return 34;
			case fullyZoomIn:
				return 35;
			case clickInCentre:
				return 36;
			case cFastshoot:
				return 41;
			case pFastshoot:
				return 42;
			case shootSeqFast:
				return 43;
			case loadLevel:
				return 51;
			case restartLevel:
				return 52;
			case getGTWithScreenshot:
				return 61;
			case getGTWithoutScreenshot:
				return 62;
			case getNoisyGTWithScreenshot:
				return 63;
			case getNoisyGTWithoutScreenshot:
				return 64;
			case getCurrentLevelScore:
				return 65;
			default:
				break;
		}
		return 0;
	}

	public byte get(){
		return (byte) message_code;
	}

}
