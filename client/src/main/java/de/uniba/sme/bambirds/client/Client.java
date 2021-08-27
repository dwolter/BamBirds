package de.uniba.sme.bambirds.client;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import de.uniba.sme.bambirds.common.exceptions.ServerException;
import de.uniba.sme.bambirds.common.utils.ActionRobotJava;
import de.uniba.sme.bambirds.common.utils.Settings;
import de.uniba.sme.bambirds.common.utils.Settings.ServerType;

/**
 * Client
 */
public class Client {
	private static final Logger log = LogManager.getLogger();

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
		if (INSTANCE != null) {
			shutdown();
		}
		INSTANCE = new Client(Settings.SERVER_HOST, Settings.SERVER_TYPE);
	}

	public static void shutdown() {
		if (INSTANCE != null) {
			log.info("Shutting down connection to {}", Settings.SERVER_TYPE);
			INSTANCE.ar.close();
		}
		INSTANCE = null;
	}

}