package de.uniba.sme.bambirds.client;

import de.uniba.sme.bambirds.common.exceptions.ServerException;
import de.uniba.sme.bambirds.common.utils.ActionRobotJava;
import de.uniba.sme.bambirds.common.utils.Settings;
import de.uniba.sme.bambirds.common.utils.Settings.ServerType;

/**
 * Client
 */
public class Client {

	private static Client INSTANCE;
	private final ActionRobotJava ar;

	public Client(String ip, ServerType serverType) throws ServerException {
		if (serverType == ServerType.ANGRY_BIRDS)
			ar = new ABActionRobotJava(ip);
		else
			ar = new SBActionRobotJava(ip);
	}

	public static ActionRobotJava get() throws ServerException {
		if (INSTANCE == null)
			init();
		return INSTANCE.ar;
	}

	public static void init() throws ServerException {
		INSTANCE = new Client(Settings.serverHost, Settings.serverType);
	}

	public static void shutdown() {
		INSTANCE.ar.close();
		INSTANCE = null;
	}

}