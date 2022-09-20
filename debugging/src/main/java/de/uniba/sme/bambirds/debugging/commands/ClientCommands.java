package de.uniba.sme.bambirds.debugging.commands;

import de.uniba.sme.bambirds.common.utils.ConfigureResponse;
import org.springframework.shell.standard.ShellMethod;
import org.springframework.shell.standard.ShellOption;

import de.uniba.sme.bambirds.client.Client;
import de.uniba.sme.bambirds.common.exceptions.ServerException;
import de.uniba.sme.bambirds.common.utils.Settings;
import de.uniba.sme.bambirds.common.utils.Settings.ServerType;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.shell.standard.ShellComponent;

@ShellComponent
public class ClientCommands {
	private static final Logger LOG = LogManager.getLogger(ClientCommands.class);

	@ShellMethod("Establish a connection to the server")
	public void connect(final @ShellOption(defaultValue = "localhost") String host, final @ShellOption(defaultValue = "ANGRY_BIRDS") String serverType) {
		Settings.SERVER_HOST = host;
		Settings.SERVER_TYPE = ServerType.valueOf(serverType);
		try {
			Client.init();
			ConfigureResponse info = Client.get().configure(Settings.TEAM_ID);

			LOG.info("Server response: roundInfo = " + info.getRound() + ", timeLimit = " + info.getTimeLimit() + ", numOfLevels = " + info.getNumberOfLevels());
		} catch (ServerException e) {
			LOG.error("Could not connect to Server", e);
		}
	}

}
