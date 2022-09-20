package de.uniba.sme.bambirds.client;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import de.uniba.sme.bambirds.common.exceptions.ServerException;
import de.uniba.sme.bambirds.common.utils.ActionRobotJava;
import de.uniba.sme.bambirds.common.utils.Settings;
import de.uniba.sme.bambirds.common.utils.Settings.ServerType;

/** Client wrapping the connection to a AngryBirds or ScienceBirds server.
 */
public class Client {
	private static final Logger LOG = LogManager.getLogger();

	private static Client instance;
	private final ActionRobotJava ar;

	public Client(final String ip, final ServerType serverType) throws ServerException {
		if (serverType == ServerType.ANGRY_BIRDS) {
			ar = new ABActionRobotJava(ip);
		}	else {
			ar = new SBActionRobotJava(ip);
		}
	}

	public static ActionRobotJava get() throws ServerException {
		if (instance == null) {
			init();
		}
		return instance.ar;
	}

	public static void init() throws ServerException {
		if (instance != null) {
			shutdown();
		}
		instance = new Client(Settings.SERVER_HOST, Settings.SERVER_TYPE);
	}

	public static void shutdown() {
		if (instance != null) {
			LOG.info("Shutting down connection to {}", Settings.SERVER_TYPE);
			instance.ar.close();
		}
		instance = null;
	}

}
