package de.uniba.sme.bambirds.common.utils;

import java.io.IOException;

public interface Connector extends Runnable {

  /**
   * Shutdown the connected Process
   */
  public void shutdown();

  /**
   * Returns the result which has been returned by the connected Process. If a
   * result does not become available within {@code timeOut} milliseconds the
   * running process is destroyed and an empty string returned.
   *
   * @param timeOut The number of milliseconds to wait before aborting
   * @return A string containing the result (empty if there is no result)
   * @throws IOException if no connection or no result left
   */
  public String getResult(long timeOut) throws IOException, InterruptedException;

  /**
   * Returns {@code true} if the process this connector is connected to is running
   * and commands can be sent to it
   * 
   * @return {@code true} if the process this connector is connected to is running
   *         and commands can be sent to it
   */
  public boolean isRunning();

  /**
   * Returns {@code true} if the process this connector is connected has
   * terminated with exit code 0
   * 
   * @return {@code true} if the process this connector is connected has
   *         terminated with exit code 0
   */
  public boolean completedNormally();

}
