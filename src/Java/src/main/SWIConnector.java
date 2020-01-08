package main;

import helper.CustomLogger;

import java.io.*;
import java.util.concurrent.TimeUnit;

import static main.BamBirdPaths.replacePathDelimiters;

/**
 * The {@code SWIConnector} class provides a simple interface to a
 * <i>SWI Prolog</i> process.
 * <p>
 * Example: Sending a filename containing a knowledge base to be evaluated
 * by Prolog
 * <blockquote><pre>
 * SWIConnector connector = new SWIConnector("/usr/bin/swipl", "Prolog/functions.pl");
 * (new Thread(connector)).start();
 * connector.sendCommand("'situation5.pl'.");
 * String result = connector.getResult(5000);
 * </pre></blockquote>
 * Note that the filename has to be a Prolog string literal with single
 * quotes and the command must end with a period.
 */
public class SWIConnector implements Runnable {
	private InputStream in;
	private OutputStream out;
	private InputStream err;
	private String pathToSwipl;
	private String pathToFunctionsPL;
	private Process process = null;
	private String result = "";
	private boolean easyMain = false;

	public SWIConnector(String pathToSwipl, String pathToFunctionsPL) {
		this.pathToFunctionsPL = pathToFunctionsPL;
		this.pathToSwipl = pathToSwipl;
	}

	private void startProcess() {
		CustomLogger.info("[PLCon] Starting SWIPL (" + pathToSwipl + ") ...");
		String command = pathToSwipl + " -O -g consult('" + replacePathDelimiters(pathToFunctionsPL) + "').";
		CustomLogger.info("[PLCon] " + command);
		try {
			process = Runtime.getRuntime().exec(command);
		} catch (IOException e) {
			CustomLogger.severe("[PLCon] Failed to start SWIPL: " + e.getMessage());
			return;
		}

		try {
			Thread.sleep(1000);
		} catch (InterruptedException ignored) {
		}

		in = process.getInputStream();
		out = process.getOutputStream();
		err = process.getErrorStream();

		Thread thread = new Thread(() -> {
			String line;
			try (BufferedReader r = new BufferedReader(new InputStreamReader(err))) {
				while ((line = r.readLine()) != null) {
					CustomLogger.severe("[PL] " + line);
				}
			} catch (IOException e) {
				CustomLogger.severe("[PLCon] Failed to read error line from SWIPL: " + e.getMessage());
			}
		});
		thread.start();

		if (easyMain) {
			sendCommand("easyMain; halt.");
		} else {
			sendCommand("main; halt.");
		}
		easyMain = false;

		try (BufferedReader br = new BufferedReader(new InputStreamReader(in))) {
			String line;
			while (process.isAlive()) {
				line = br.readLine();
				if (line != null && !line.trim().isEmpty()) {
					CustomLogger.info("[PL] " + line);
					synchronized (this) {
						result = line;
					}
				}
			}
		} catch (IOException e) {
			CustomLogger.severe("[PLCon] Failed to read line from SWIPL: " + e.getMessage());
		}
	}

	@Override
	public void run() {
		while (true) {
            CustomLogger.info("[PLCon] Initiating Prolog start...");
			startProcess();
		}
	}

	/**
	 * Sends a command to the currently running Prolog process.
	 * Does nothing if no process exists.
	 *
	 * @param command A string containing a Prolog term
	 */
	public void sendCommand(String command) {
		if (out == null) {
			CustomLogger.severe("[PLCon] No connection to SWIPL; failed to send command: " + command);
			return;
		}
		synchronized (this) {
			result = "";
		}

		command = replacePathDelimiters(command);
		try {
			out.write((command + System.lineSeparator()).getBytes());
			out.flush();
			CustomLogger.info("[PLCon] Sent command to SWIPL: " + command);
		} catch (IOException e) {
			CustomLogger.severe("[PLCon] Failed to send command to SWIPL: " + command);
			CustomLogger.severe(e.getMessage());
		}
	}

	/**
	 * Returns the result which has been returned by a Prolog process
	 * after sending it a command. If a result does not become available
	 * within {@code timeOut} milliseconds the running process is
	 * destroyed and an empty string returned.
	 *
	 * @param timeOut The number of milliseconds to wait before aborting
	 * @return A string containing the result (empty if there is no result)
	 */
	public String getResult(long timeOut) {
		long start = System.currentTimeMillis();

		while (true) {
			synchronized (this) {
				if (!result.isEmpty()) {
					return result;
				}
			}
			if (System.currentTimeMillis() - start > timeOut) {
				CustomLogger.severe("[PLCon] No return from SWIPL after " + timeOut + " milliseconds");
				synchronized (this) {
					if (process != null) {
						process.destroyForcibly();
						easyMain = true;
						try {
							process.waitFor(3000, TimeUnit.MILLISECONDS);
						} catch (InterruptedException ignored) {
						}
					}
					return "";
				}
			}
		}
	}

}
