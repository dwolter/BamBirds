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
	/** Configure the agent at the server. */
	configure(1),
	/** Set the simulation speed on the server. */
	setSimulationSpeed(2),
	/** Do a Screenshot. */
	doScreenShot(11),
	/** Get the current State. */
	getState(12),
	/** Get the best scores of this and other connected agents. */
	getBestScores(13),
	/** Get the current level. */
	getCurrentLevel(14),
	/** Get the number of levels available. */
	getNumberOfLevels(15),
	/** Get this agents score. */
	getMyScore(23),
	/** Shoot with cartesian coordinates. */
	cShoot(31),
	/** Shoot with polar coordinates. */
	pShoot(32),
	/** Shoot a sequence of shots. */
	shootSeq(33),
	/** Fully zoom out the level. */
	fullyZoomOut(34),
	/** Fully zoom in the level. */
	fullyZoomIn(35),
	/** Click in the center of the level (use for switching between birds and pigs). */
	clickInCenter(36),
	/** Shoot fast with cartesian coordinates (no waiting until scene is stable). */
	cFastShoot(41),
	/** Shoot fast with polar coordinates (no waiting until scene is stable). */
	pFastShoot(42),
	/** Shoot a sequence of shots fast (no waiting until scene is stable). */
	shootSeqFast(43),
	/** Load a level. */
	loadLevel(51),
	/** Restart the current level. */
	restartLevel(52),
	/** Select the next level. */
	selectNextLevel(53),
	/** Get ground truth with screenshot. */
	getGTWithScreenshot(61),
	/** Get ground truth without screenshot. */
	getGTWithoutScreenshot(62),
	/** Get noisy ground truth with screenshot. */
	getNoisyGTWithScreenshot(63),
	/** Get noisy ground truth without screenshot. */
	getNoisyGTWithoutScreenshot(64),
	/** Get the score in the current level. */
	getCurrentLevelScore(65),
	/** Report novelty likelihood (is the agent sure if there is some novelty). */
	reportNoveltyLikelihood(66),
	/** Tell the server that the agent is ready for a new level set. */
	readyForNewSet(68),
	/** Request information about novelties. */
	requestNoveltyInformation(69);

	/** Code of the message. */
	private final byte messageCode;

	ClientMessageTable(final int messageCodeInput) {
		this.messageCode = (byte) messageCodeInput;
	}

	/** get the message code.
	 * @return the message code
	 */
	public byte get() {
		return messageCode;
	}

}
