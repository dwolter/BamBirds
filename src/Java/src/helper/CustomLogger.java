package helper;

import java.io.IOException;
import java.util.Date;
import java.util.logging.*;

/**
 * This is a custom-made logger, it uses java.utils.logging.
 * It can be used in any class via static call "CustomLogger.info(msg)".
 * The calling class will automatically be derived by
 * the log methods (the line with "exception"). Call CustomLogger.info(), ~.warning() or ~.severe()
 * for different logging levels. Default log level is "info", but this can be changed in any class
 * via "setLogLevel(String level)" with "info", "warning" or "severe".
 * (recommended is of course the main entry point for setting the log level).
 */

public class CustomLogger {
	public enum LogLevel { NONE, SEVERE, WARNING, INFO };
	static Logger logger;
	static LogLevel logLevel = LogLevel.SEVERE;
	static String className = "unknown";

	private CustomLogger() throws IOException {
		//instantiate the logger
		logger = Logger.getLogger(CustomLogger.class.getName());
		logger.setUseParentHandlers(false);
		ConsoleHandler consoleHandler = new ConsoleHandler();
		consoleHandler.setFormatter(createSimpleConsoleFormatter());
		logger.addHandler(consoleHandler);
	}

	private static SimpleFormatter createSimpleConsoleFormatter(){
		return (new SimpleFormatter() {
			private static final String format = "[%1$tT] [%3$s] %2$s | %4$s %n";

			@Override public synchronized String format(LogRecord logRecord) {
				return String.format(format, new Date(logRecord.getMillis()), className,
						logRecord.getLevel().getLocalizedName(), logRecord.getMessage());
			}
		});
	}

	private static SimpleFormatter createSimpleLogfileFormatter(){
		return (new SimpleFormatter() {
			private static final String format = "[%1$tF %1$tT] [%3$s] %2$s | %4$s %n";

			@Override public synchronized String format(LogRecord logRecord) {
				return String.format(format, new Date(logRecord.getMillis()), className,
						logRecord.getLevel().getLocalizedName(), logRecord.getMessage());
			}
		});
	}

	public static void saveLogsToFile(){
		try{
			FileHandler fh = new FileHandler("logfile.log", true);
			fh.setFormatter(createSimpleLogfileFormatter());
			getLogger().addHandler(fh);
			// this is for oversight purposes
			info(CustomLogger.class.getName(), "-------- starting a new run, saving logs to logfile.log --------");
		} catch (Exception e){
			e.printStackTrace();
		}
	}

	/**
	 * sets the loglevel. Set to "info", "warning" or "severe"
	 * @param level
	 */
	public static void setLogLevel(LogLevel level){
		logLevel = level;
		if (logLevel == LogLevel.INFO) {
			saveLogsToFile();
			info("Agent started in DEBUG mode. All logs will be output and saved to logfile.log.");
		}
	}

	/**
	 * default logging method - not recommended to use.
	 * @param level
	 * @param msg
	 */
	public static void log(Level level, String msg){
		getLogger().log(level, msg);
	}

	// the following three methods automatically derives its calling class name
	public static void warning(String msg){
		if (logLevel.ordinal() >= LogLevel.WARNING.ordinal()) {
			className = new Exception().getStackTrace()[1].getClassName();
			getLogger().log(Level.WARNING, msg);
		}
	}
	public static void info(String msg){
		if (logLevel.ordinal() >= LogLevel.INFO.ordinal()) {
			className = new Exception().getStackTrace()[1].getClassName();
			getLogger().log(Level.INFO, msg);
		}
	}
	public static void severe(String msg){
		if (logLevel.ordinal() >= LogLevel.SEVERE.ordinal()) {
			className = new Exception().getStackTrace()[1].getClassName();
			getLogger().log(Level.SEVERE, msg);
		}
	}

	// the following three methods need the calling class as a given parameter. Is less cpu demanding
	// than automatically deriving the calling class.
	public static void warning(String cName, String msg){
		if (logLevel.ordinal() >= LogLevel.WARNING.ordinal()) {
			className = cName;
			getLogger().log(Level.WARNING, msg);
		}
	}
	public static void info(String cName, String msg){
		if (logLevel.ordinal() >= LogLevel.INFO.ordinal()) {
			className = cName;
			getLogger().log(Level.INFO, msg);
		}
	}
	public static void severe(String cName, String msg){
		if (logLevel.ordinal() >= LogLevel.SEVERE.ordinal()) {
			className = cName;
			getLogger().log(Level.SEVERE, msg);
		}
	}

	/**
	 * singleton pattern: gets the same instance of the logger
	 * @return
	 */
	private static Logger getLogger(){
		if(logger == null){
			try {
				new CustomLogger();
			} catch (IOException e) {
				e.printStackTrace();
			}
		}
		return logger;
	}
}