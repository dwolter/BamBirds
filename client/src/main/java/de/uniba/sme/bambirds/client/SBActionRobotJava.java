package de.uniba.sme.bambirds.client;

import org.apache.commons.lang3.NotImplementedException;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import de.uniba.sme.bambirds.common.exceptions.ServerException;
import de.uniba.sme.bambirds.common.objects.Shot;
import de.uniba.sme.bambirds.common.utils.ByteUtil;
import de.uniba.sme.bambirds.common.utils.Settings.ServerType;

/**
 * SBActionRobot
 */
public class SBActionRobotJava extends ABActionRobotJava {
	private static final Logger log = LogManager.getLogger();

	public SBActionRobotJava(String ip) throws ServerException {
		super(ip);
	}

	@Override
	public byte[] configure(byte[] team_id) throws ServerException {
		return simpleMessage(SBMessageEncoder.configure(team_id), 4);
	}

	public byte setSimulationSpeed(byte[] speed) throws ServerException {
		throw new NotImplementedException("Not yet implemented");
	}

	@Override
	public byte[] getCurrentLevel() throws ServerException {
		return simpleMessage(ClientMessageEncoder.getCurrentLevel(), 4);
	}

	@Override
	public byte[] getNumberOfLevels() throws ServerException {
		return simpleMessage(ClientMessageEncoder.getNumberOfLevels(), 4);
	}

	@Override
	public byte[] getGroundTruth(boolean withScreenshot, boolean noisy) throws ServerException {
		throw new NotImplementedException("Not yet implemented");
	}

	@Override
	public byte[] getCurrentLevelScore() throws ServerException {
		return simpleMessage(SBMessageEncoder.getCurrentLevelScore(), 4);
	}

	@Override
	public byte restartLevel() throws ServerException {
		// return super.restartLevel();
		log.warn("ScienceBirds server currently does not support restarting levels. Loading the next level instead");
		log.warn("Next selected level is: {}", selectNextLevelInt());
		return 1;
	}

	@Override
	public byte loadLevel(byte... i) throws ServerException {
		// return simpleMessage(SBMessageEncoder.loadLevel(i));
		log.warn("ScienceBirds server currently does not support loading individual levels. Loading the next level instead");
		log.warn("Next selected level is: {}", selectNextLevelInt());
		return 1;
	}

	@Override
	public byte loadLevel(int i) throws ServerException {
		return loadLevel(ByteUtil.intToByteArray(i));
	}

	@Override
	public byte reportNoveltyLikelihood(byte... info) throws ServerException {
		return simpleMessage(info);
	}

	@Override
	public byte reportNoveltyLikelihood(float noveltyLikelihood, float nonNoveltyLikelihood, int[] novelObjectIDs,
			int noveltyLevel, String noveltyDescription) throws ServerException {
		// TODO Auto-generated method stub
	  byte[] info = SBMessageEncoder.reportNoveltyLikelihood(noveltyLikelihood, nonNoveltyLikelihood, novelObjectIDs, noveltyLevel, noveltyDescription);
		return reportNoveltyLikelihood(info);
	}

	@Override
	public byte[] readyForNewSet() throws ServerException {
		return simpleMessage(SBMessageEncoder.readyForNewSet(), 19);
	}

	@Override
	public byte[] requestNoveltyInformation() throws ServerException {
		return simpleMessage(SBMessageEncoder.requestNoveltyInformation(), 4);
	}

	@Override
	public byte[] selectNextLevel() throws ServerException {
		return simpleMessage(SBMessageEncoder.selectNextLevel(), 4);
	}

	@Override
	public int selectNextLevelInt() throws ServerException {
		return ByteUtil.bytesToInt(selectNextLevel());
	}

	@Override
	public byte shootFast(byte[] fx, byte[] fy, byte[] dx, byte[] dy, byte[] t1, byte[] t2, boolean polar)
			throws ServerException {

		if (polar) {
			return super.shootFast(fx, fy, dx, dy, t1, t2, polar);
		} else {
			return simpleMessage(SBMessageEncoder.cFastshoot(dx, dy, t1, t2));
		}
	}

	@Override
	public byte shoot(byte[] fx, byte[] fy, byte[] dx, byte[] dy, byte[] t1, byte[] t2, boolean polar)
			throws ServerException {

		if (polar) {
			return super.shoot(fx, fy, dx, dy, t1, t2, polar);
		} else {
			return simpleMessage(SBMessageEncoder.cshoot(dx, dy, t1, t2));
		}
	}

	@Override
	public byte shootSafe(Shot shot) throws ServerException {
		int rx = shot.getSlingX() + shot.getDragX();
		int ry = shot.getSlingY() + shot.getDragY();
		return shoot(ByteUtil.intToByteArray(shot.getSlingX())/* fx */, ByteUtil.intToByteArray(shot.getSlingY())/* fy */,
				ByteUtil.intToByteArray(rx)/* dx */, ByteUtil.intToByteArray(ry)/* dy */,
				ByteUtil.intToByteArray((int) shot.getShotTime())/* t1 */, ByteUtil.intToByteArray((int) shot.getTapTime())/* t2 */,
				false);
	}

	@Override
	public byte shootFast(Shot shot) throws ServerException {
		int rx = shot.getSlingX() + shot.getDragX();
		int ry = shot.getSlingY() + shot.getDragY();
		return shootFast(ByteUtil.intToByteArray(shot.getSlingX())/* fx */, ByteUtil.intToByteArray(shot.getSlingY())/* fy */,
				ByteUtil.intToByteArray(rx)/* dx */, ByteUtil.intToByteArray(ry)/* dy */,
				ByteUtil.intToByteArray((int) shot.getShotTime())/* t1 */, ByteUtil.intToByteArray((int) shot.getTapTime())/* t2 */,
				false);
	}

	@Override
	public ServerType getServerType() {
		return ServerType.ANGRY_BIRDS;
	}

}