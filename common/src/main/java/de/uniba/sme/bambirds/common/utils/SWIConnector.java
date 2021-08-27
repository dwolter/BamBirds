package de.uniba.sme.bambirds.common.utils;

import java.io.*;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.TimeUnit;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import static de.uniba.sme.bambirds.common.utils.FileUtil.replacePathDelimiters;

/**
 * The {@code SWIConnector} class provides a simple interface to a <i>SWI
 * Prolog</i> process.
 * <p>
 * Example: Sending a filename containing a knowledge base to be evaluated by
 * Prolog <blockquote>
 * 
 * <pre>
 * SWIConnector connector = new SWIConnector("/usr/bin/swipl", "Prolog/functions.pl");
 * (new Thread(connector)).start();
 * connector.sendCommand("'situation5.pl'.");
 * String result = connector.getResult(5000);
 * </pre>
 * 
 * </blockquote> Note that the filename has to be a Prolog string literal with
 * single quotes and the command must end with a period.
 */
public class SWIConnector implements Connector {
	private static final Logger log = LogManager.getLogger(SWIConnector.class);

	enum State {
		CREATED, STARTING, RUNNING, STOPPED, CANCELLED
	}

	private final Object lock = new Object();

	private InputStream in;
	private OutputStream out;
	private InputStream err;
	private final String pathToSwipl;
	private final String pathToMain;
	private final String pathToExecutable;
	private Process process = null;
	private final BlockingQueue<String> resultQueue;
	private State state = State.CREATED;

	public SWIConnector(String pathToSwipl, String pathToMain) {
		this(pathToSwipl, pathToMain, null);
	}

	public SWIConnector(String pathToSwipl, String pathToMain, String pathToExecutable) {
		this.pathToMain = pathToMain;
		this.pathToSwipl = pathToSwipl;
		this.pathToExecutable = pathToExecutable;
		this.resultQueue = new LinkedBlockingQueue<>();
	}

	private void startProcess() {
		state = State.STARTING;
		synchronized (lock) {
			Map<String, String> env = new HashMap<>(System.getenv());

			String[] command;
			if (pathToExecutable != null) {
				log.info("Starting SWIPL Executable {}", pathToExecutable);
				command = new String[] { pathToExecutable };
			} else {
				log.info("Starting SWIPL ({}): {}", pathToSwipl, pathToMain);
				command = new String[] { pathToSwipl, "-p", "lib="+Settings.PLANNER_LIB_DIR, pathToMain };
			}

			if (Settings.GAME_MODE == Settings.GameMode.DEMO) {
				env.put("DEMO_MODE", "true");
			}

			log.debug("Command: {}", Arrays.toString(command));
			try {
				process = Runtime.getRuntime().exec(command, SystemUtil.envMapToArray(env));
			} catch (IOException e) {
				log.error("Failed to start SWIPL", e);
				return;
			}

			try {
				Thread.sleep(1000);
			} catch (InterruptedException e) {
				log.error("Waiting got interrupted", e);
				return;
			}

			in = process.getInputStream();
			out = process.getOutputStream();
			err = process.getErrorStream();

			Thread thread = new Thread(() -> {
				String line;
				try (BufferedReader r = new BufferedReader(new InputStreamReader(err))) {
					while ((line = r.readLine()) != null) {
						log.error("[PL] " + line);
					}
				} catch (IOException e) {
					// Only log the error if we cannot read the line unexpectedly
					if (!(completedNormally() || state == State.CANCELLED)) {
						log.error("Failed to read error line from SWIPL", e);
					}
				}
			});
			thread.start();
			state = State.RUNNING;
			lock.notifyAll();
		}

		try (BufferedReader br = new BufferedReader(new InputStreamReader(in))) {
			String line;
			while (process.isAlive()) {
				line = br.readLine();
				if (line != null && !line.trim().isEmpty()) {
					log.trace("[PL] " + line);
					resultQueue.add(line);
				}
			}
		} catch (IOException e) {
			// Only log the error if we cannot read the line unexpectedly
			if (!(completedNormally() || state == State.CANCELLED)) {
				log.error("Failed to read line from SWIPL: " + e.getMessage());
			}
		} finally {
			// Add Empty List to resultQueue since it might have not returned anything
			resultQueue.add("[]");
		}
	}

	@Override
	public void run() {
		log.debug("Starting...");
		log.info("Initiating Prolog start...");
		startProcess();
		shutdown();
	}

	public void shutdown() {
		synchronized (lock) {
			log.debug("Current state is {}", state);
			while (state == State.STARTING) {
				lock.notifyAll();
				try {
				log.debug("Waiting with shutdown until connection is fully created");
					lock.wait(100);
				} catch (InterruptedException e) {
				}
			}
			if (state == State.RUNNING) {
				log.debug("Received Shutdown");
				state = State.CANCELLED;
			}
			if (process != null && process.isAlive()) {
				log.info("Destroying swipl process");
				process.destroy();
			}
		}
	}

	/**
	 * Sends a command to the currently running Prolog process. Waits maximum 500ms
	 * in 100ms intervals if no process exists on method call (because thread has
	 * not started executing yet)
	 *
	 * @param command A string containing a Prolog term
	 */
	public void sendCommand(String command) {
		synchronized (lock) {
			int tries = 0;
			// Try to wait if the out stream does not exits. This may be possible when this
			// method is called before the thread could initialize
			while (out == null) {
				tries++;
				if (tries > 5) {
					log.error("No connection to SWIPL; failed to send command: " + command);
					return;
				}
				lock.notifyAll();
				try {
					lock.wait(100);
				} catch (InterruptedException e) {
				}
			}

			resultQueue.clear();
			command = replacePathDelimiters(command);
			try {
				out.write((command + System.lineSeparator()).getBytes());
				out.flush();
				log.debug("Sent command to SWIPL: " + command);
			} catch (IOException e) {
				log.error("Failed to send command to SWIPL: " + command);
				log.error(e.getMessage());
			}
		}
	}

	/**
	 * Returns the result which has been returned by a Prolog process after sending
	 * it a command. If a result does not become available within {@code timeOut}
	 * milliseconds the running process is destroyed and an empty string returned.
	 *
	 * @param timeOut The number of milliseconds to wait before aborting
	 * @return A string containing the result (empty if there is no result)
	 * @throws IOException if no connection or no result left
	 */
	public String getResult(long timeOut) throws IOException, InterruptedException {
		synchronized (lock) {
			lock.notifyAll();
			if (process == null || (!process.isAlive() && resultQueue.isEmpty())) {
				throw new IOException("No connection to swipl and queue is empty");
			}
		}
		String result;
		result = resultQueue.poll(timeOut, TimeUnit.MILLISECONDS);
		if (result == null) {
			log.error("No return from SWIPL after " + timeOut + " milliseconds");
			result = "";
		}
		return result;
	}

	/**
	 * Returns {@code true} if the process this connector is connected to is running
	 * and commands can be sent to it
	 *
	 * @return {@code true} if the process this connector is connected to is running
	 *         and commands can be sent to it
	 */
	public boolean isRunning() {
		return state == State.RUNNING && process != null && process.isAlive();
	}

	/**
	 * Returns {@code true} if the process this connector is connected has
	 * terminated with exit code 0
	 *
	 * @return {@code true} if the process this connector is connected has
	 *         terminated with exit code 0
	 */
	public boolean completedNormally() {
		return (!process.isAlive() && process.exitValue() == 0);
	}

}
