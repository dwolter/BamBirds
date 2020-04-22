package de.uniba.sme.bambirds.debugging.commands;

import org.springframework.shell.standard.ShellMethod;
import org.springframework.shell.standard.ShellOption;

import de.uniba.sme.bambirds.client.Client;
import de.uniba.sme.bambirds.common.exceptions.ServerException;
import de.uniba.sme.bambirds.common.utils.ByteUtil;
import de.uniba.sme.bambirds.common.utils.Settings;
import de.uniba.sme.bambirds.common.utils.Settings.ServerType;
import de.uniba.sme.bambirds.debugging.Values;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.shell.standard.ShellComponent;

@ShellComponent
public class ClientCommands {
	private static final Logger log = LogManager.getLogger(ClientCommands.class);

	@ShellMethod("Establish a connection to the server")
	public void connect(@ShellOption(defaultValue="localhost") String host, @ShellOption(defaultValue="ANGRY_BIRDS") String serverType) {
		Settings.serverHost = host;
		Settings.serverType = ServerType.valueOf(serverType);
		try {
			Client.init();
			byte[] info = Client.get().configure(Settings.teamID);

			Values.ROUND_INFO = info[0];
			Values.TIME_LIMIT = info[1];
			Values.NUMBER_OF_LEVELS = info[2];

			if (Settings.serverType == ServerType.SCIENCE_BIRDS){
				Values.NUMBER_OF_LEVELS = ByteUtil.bytesToInt(Client.get().getNumberOfLevels());
			}
			log.info("Server response: roundInfo = " + Values.ROUND_INFO + ", timeLimit = " + Values.TIME_LIMIT + ", numOfLevels = " + Values.NUMBER_OF_LEVELS);
		} catch (ServerException e) {
			log.error("Could not connect to Server",e);
		}
	}

}