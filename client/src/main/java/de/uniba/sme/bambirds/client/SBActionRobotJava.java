package de.uniba.sme.bambirds.client;

import org.apache.commons.lang3.NotImplementedException;

import de.uniba.sme.bambirds.common.exceptions.ServerException;
import de.uniba.sme.bambirds.common.utils.ByteUtil;

/**
 * SBActionRobot
 */
public class SBActionRobotJava extends ABActionRobotJava {

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
		return simpleMessage(SBMessageEncoder.getCurrentLevelScore(),4);
	}

	@Override
	public byte loadLevel(byte... i) throws ServerException{
		return simpleMessage(SBMessageEncoder.loadLevel(i));
	}

	@Override
	public byte loadLevel(int i) throws ServerException {
		return loadLevel(ByteUtil.intToByteArray(i));
	}

}