package de.uniba.sme.bambirds.common.utils;

import java.io.BufferedReader;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.concurrent.ArrayBlockingQueue;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.TimeUnit;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class PythonConnector implements Connector {
  private static final Logger log = LogManager.getLogger();

  private InputStream in;
  private InputStream err;
  private boolean running = true;
  private Process process = null;
  private BlockingQueue<String> resultQueue = new ArrayBlockingQueue<>(10);

  public static final int VISION_CONNECTOR = 0;

  private static final String[] PYTHON_MAIN = new String[] { "python", "main.py" };

  private final String[] command;

  public PythonConnector(String... args) {
    List<String> command = new ArrayList<>(Arrays.asList(PYTHON_MAIN));
    File main = new File(command.get(1));
    if (!main.exists()) {
      if (new File("../main.py").exists()) {
        command.set(1, "../main.py");
      } else {
        throw new RuntimeException("File main.py could not be found");
      }
    }
    List<String> argsList = Arrays.asList(args);
    command.addAll(argsList);

    this.command = command.toArray(new String[0]);
  }

  @Override
  public void run() {
    log.debug("starting python process");

    log.debug(Arrays.toString(command));
    try {
      process = Runtime.getRuntime().exec(command);
    } catch (IOException e) {
      log.error("Failed to start Python", e);
      return;
    }

    try {
      Thread.sleep(1000);
    } catch (InterruptedException e) {
      log.error("Waiting got interrupted", e);
      return;
    }

    in = process.getInputStream();
    err = process.getErrorStream();

    Thread thread = new Thread(() -> {
      String line;
      try (BufferedReader r = new BufferedReader(new InputStreamReader(err))) {
        while ((line = r.readLine()) != null) {
          log.error("[PY] " + line);
        }
      } catch (IOException e) {
        // Only log the error if we cannot read the line unexpectedly
        if (!completedNormally()) {
          log.error("Failed to read error line from Python", e);
        }
      }
    });
    thread.start();

    try (BufferedReader br = new BufferedReader(new InputStreamReader(in))) {
      String line;
      while (process.isAlive()) {
        line = br.readLine();
        if (line != null && !line.trim().isEmpty()) {
          log.debug("[PY] " + line);
          resultQueue.add(line);
        }
      }
    } catch (IOException e) {
      // Only log the error if we cannot read the line unexpectedly
      if (!completedNormally()) {
        log.error("Failed to read line from Python: " + e.getMessage());
      }
    }
    running = false;
  }

  @Override
  public void shutdown() {
    // TODO Auto-generated method stub
    if (process != null) {
      process.destroy();
    }
  }

  @Override
  public String getResult(long timeOut) throws IOException, InterruptedException {
    return resultQueue.poll(timeOut, TimeUnit.MILLISECONDS);
  }

  @Override
  public boolean isRunning() {
    return running;
  }

  @Override
  public boolean completedNormally() {
    return !running && process != null && process.exitValue() == 0;
  }

}
