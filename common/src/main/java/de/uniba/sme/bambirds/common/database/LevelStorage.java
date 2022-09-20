package de.uniba.sme.bambirds.common.database;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.io.OutputStream;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Properties;
import java.util.Set;

public final class LevelStorage {
	private static final Logger LOG = LogManager.getLogger(LevelStorage.class);
	// Singleton
	private static LevelStorage ourInstance = new LevelStorage();

	public static LevelStorage getInstance() {
		return ourInstance;
	}

	private LevelStorage() {
	}

	private final Map<Integer, Level> levelDataStorage = new LinkedHashMap<>();

	public Level getLevelById(final int lvlId) {
		return levelDataStorage.getOrDefault(lvlId, null);
	}

	public void addLevelToStorage(final Level playedLevel) {
		if (levelDataStorage.containsKey(playedLevel.levelId)) {
			return;
		}
		levelDataStorage.put(playedLevel.levelId, playedLevel);
		LOG.info("Level " + playedLevel.levelId + " added into database with score " + playedLevel.getBestScore());
	}

	public Set<Integer> getListOfIDs() {
		return levelDataStorage.keySet();
	}

	public int getStorageSize() {
		return this.levelDataStorage.size();
	}

	public void storeToFile() {
		try {
			LOG.info("Writing storage to disk");
			FileOutputStream file = new FileOutputStream("levelStorage.dat");
			ObjectOutputStream out = new ObjectOutputStream(file);
			out.writeObject(this);
			out.close();
			file.close();
			LOG.info("done.");
		} catch (Exception e) {
			LOG.error(" Error writing storage to disk");
			e.printStackTrace();
		}
	}

	public void restoreFromFile() {
		try {
			if (new java.io.File("levelStorage.dat").exists()) {
				LOG.info("Loading storage from disk");
				FileInputStream file = new FileInputStream("levelStorage.dat");
				ObjectInputStream in = new ObjectInputStream(file);
				ourInstance = (LevelStorage) in.readObject();
				LOG.info("done.");
				in.close();
				file.close();
			}
		} catch (Exception e) {
			LOG.error("Error reading storage from disk");
			e.printStackTrace();
		}
	}

	/**
	 * Store the results in Property format.
	 *
	 * @param filename The file the results should be saved to
	 * @param stdout   If the results should also be displayed in stdout
	 */
	public void storeResults(final String filename, final boolean stdout) {
		Properties p = new Properties();
		levelDataStorage.forEach((id, level) -> p.put(id.toString(), level.getScores().toString()));
		try (OutputStream os = new FileOutputStream(filename)) {
			if (stdout) {
				p.store(System.out, "Scores for each level");
			}
			p.store(os, "Scores for each level");
		} catch (IOException e) {
			LOG.error(e);
		}
	}
}
