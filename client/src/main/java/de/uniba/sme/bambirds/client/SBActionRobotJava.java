package de.uniba.sme.bambirds.client;

import de.uniba.sme.bambirds.common.utils.ConfigureResponse;
import org.apache.commons.lang3.NotImplementedException;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import de.uniba.sme.bambirds.common.exceptions.ServerException;
import de.uniba.sme.bambirds.common.objects.Shot;
import de.uniba.sme.bambirds.common.utils.ByteUtil;
import de.uniba.sme.bambirds.common.utils.Settings.ServerType;

/**
 * Action Robot for communication with Science Birds.
 * <p>extends most parts from {@link ABActionRobotJava} but some things are overwritten</p>
 */
public class SBActionRobotJava extends ABActionRobotJava {
	/** Logger. */
	private static final Logger LOG = LogManager.getLogger();

	private static final int CONFIGURE_RESPONSE_LENGTH = 4;
	private static final int GET_CURRENT_LEVEL_RESPONSE_LENGTH = 4;
	private static final int GET_NUMBER_OF_LEVELS_RESPONSE_LENGTH = 4;
	private static final int GET_CURRENT_LEVEL_SCORE_RESPONSE_LENGTH = 4;
	private static final int READY_FOR_NEW_SET_RESPONSE_LENGTH = 19;
	private static final int REQUEST_NOVELTY_INFORMATION_RESPONSE_LENGTH = 4;
	private static final int SELECT_NEXT_LEVEL_RESPONSE_LENGTH = 4;

	public SBActionRobotJava(final String... args) throws ServerException {
		super(args);
	}

	@Override
	public byte[] configure(final byte[] teamId) throws ServerException {
		return simpleMessage(SBMessageEncoder.configure(teamId), CONFIGURE_RESPONSE_LENGTH);
	}

	@Override
	public ConfigureResponse configure(final int teamId) throws ServerException {
		ConfigureResponse response = super.configure(teamId);
		int actualNumberOfLevels = getNumberOfLevelsInt();
		return new ConfigureResponse(response.getRound(), response.getTimeLimit(), actualNumberOfLevels);
	}

	@Override
	public byte setSimulationSpeed(final byte[] speed) {
		throw new NotImplementedException("Not yet implemented");
	}

	@Override
	public byte[] getCurrentLevel() throws ServerException {
		return simpleMessage(ClientMessageEncoder.getCurrentLevel(), GET_CURRENT_LEVEL_RESPONSE_LENGTH);
	}

	@Override
	public byte[] getNumberOfLevels() throws ServerException {
		return simpleMessage(ClientMessageEncoder.getNumberOfLevels(), GET_NUMBER_OF_LEVELS_RESPONSE_LENGTH);
	}

	@Override
	public int getNumberOfLevelsInt() throws ServerException {
		return ByteUtil.bytesToInt(getNumberOfLevels());
	}

	@Override
	public byte[] getGroundTruth(final boolean withScreenshot, final boolean noisy) {
		throw new NotImplementedException("Not yet implemented");
	}

	@Override
	public byte[] getCurrentLevelScore() throws ServerException {
		return simpleMessage(SBMessageEncoder.getCurrentLevelScore(), GET_CURRENT_LEVEL_SCORE_RESPONSE_LENGTH);
	}

	@Override
	public byte restartLevel() throws ServerException {
		// return super.restartLevel();
		int nextLevel = selectNextLevelInt();
		LOG.warn("ScienceBirds server currently does not support restarting levels. Loading the next level instead");
		LOG.warn("Next selected level is: {}", nextLevel);
		return 1;
	}

	@Override
	public byte loadLevel(final byte... i) throws ServerException {
		// return simpleMessage(SBMessageEncoder.loadLevel(i));
		LOG.warn("ScienceBirds server currently does not support loading individual levels. Loading the next level instead");
		LOG.warn("Next selected level is: {}", selectNextLevelInt());
		return 1;
	}

	@Override
	public boolean loadLevel(final int i) throws ServerException {
		return loadLevel(ByteUtil.intToByteArray(i)) == 1;
	}

	@Override
	public byte reportNoveltyLikelihood(final byte... info) throws ServerException {
		return simpleMessage(info);
	}

	@Override
	public boolean reportNoveltyLikelihood(final float noveltyLikelihood, final float nonNoveltyLikelihood, final int[] novelObjectIDs,
																				 final int noveltyLevel, final String noveltyDescription) throws ServerException {
	  byte[] info = SBMessageEncoder.reportNoveltyLikelihood(noveltyLikelihood, nonNoveltyLikelihood, novelObjectIDs, noveltyLevel, noveltyDescription);
		return reportNoveltyLikelihood(info) == 1;
	}

	@Override
	public byte[] readyForNewSet() throws ServerException {
		return simpleMessage(SBMessageEncoder.readyForNewSet(), READY_FOR_NEW_SET_RESPONSE_LENGTH);
	}

	@Override
	public byte[] requestNoveltyInformation() throws ServerException {
		return simpleMessage(SBMessageEncoder.requestNoveltyInformation(), REQUEST_NOVELTY_INFORMATION_RESPONSE_LENGTH);
	}

	@Override
	public byte[] selectNextLevel() throws ServerException {
		return simpleMessage(SBMessageEncoder.selectNextLevel(), SELECT_NEXT_LEVEL_RESPONSE_LENGTH);
	}

	@Override
	public int selectNextLevelInt() throws ServerException {
		return ByteUtil.bytesToInt(selectNextLevel());
	}

	@Override
	public byte shootFast(final byte[] fx, final byte[] fy, final byte[] dx, final byte[] dy, final byte[] t1, final byte[] t2, final boolean polar)
			throws ServerException {

		if (polar) {
			return super.shootFast(fx, fy, dx, dy, t1, t2, polar);
		} else {
			return simpleMessage(SBMessageEncoder.cFastshoot(dx, dy, t1, t2));
		}
	}

	@Override
	public byte shoot(final byte[] fx, final byte[] fy, final byte[] dx, final byte[] dy, final byte[] t1, final byte[] t2, final boolean polar)
			throws ServerException {

		if (polar) {
			return super.shoot(fx, fy, dx, dy, t1, t2, polar);
		} else {
			return simpleMessage(SBMessageEncoder.cshoot(dx, dy, t1, t2));
		}
	}

	@Override
	public boolean shootSafe(final Shot shot) throws ServerException {
		int rx = shot.getSlingX() + shot.getDragX();
		int ry = shot.getSlingY() + shot.getDragY();
		return shoot(ByteUtil.intToByteArray(shot.getSlingX())/* fx */, ByteUtil.intToByteArray(shot.getSlingY())/* fy */,
				ByteUtil.intToByteArray(rx)/* dx */, ByteUtil.intToByteArray(ry)/* dy */,
				ByteUtil.intToByteArray((int) shot.getShotTime())/* t1 */, ByteUtil.intToByteArray((int) shot.getTapTime())/* t2 */,
				false) == 1;
	}

	@Override
	public boolean shootFast(final Shot shot) throws ServerException {
		int rx = shot.getSlingX() + shot.getDragX();
		int ry = shot.getSlingY() + shot.getDragY();
		return shootFast(ByteUtil.intToByteArray(shot.getSlingX())/* fx */, ByteUtil.intToByteArray(shot.getSlingY())/* fy */,
				ByteUtil.intToByteArray(rx)/* dx */, ByteUtil.intToByteArray(ry)/* dy */,
				ByteUtil.intToByteArray((int) shot.getShotTime())/* t1 */, ByteUtil.intToByteArray((int) shot.getTapTime())/* t2 */,
				false) == 1;
	}

	@Override
	public ServerType getServerType() {
		return ServerType.SCIENCE_BIRDS;
	}

}
