package de.uniba.sme.bambirds.common.database;

import de.uniba.sme.bambirds.common.objects.Level;

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

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class LevelStorage {
	private static final Logger log = LogManager.getLogger(LevelStorage.class);
	// Singleton
	private static LevelStorage ourInstance = new LevelStorage();
	public static LevelStorage getInstance() { return ourInstance; }
	private LevelStorage() { }

	private Map<Integer, Level> levelDataStorage = new LinkedHashMap<>();

	public Level getLevelById(int lvlId) {
		return levelDataStorage.getOrDefault(lvlId, null);
	}

	public void addLeveltoStorage(Level playedLevel) {
		if (levelDataStorage.containsKey(playedLevel.levelId))
			return;
		levelDataStorage.put(playedLevel.levelId, playedLevel);
		log.info("Level " + playedLevel.levelId + " added into database with score " + playedLevel.getBestScore());
	}

	public Set<Integer> getListOfIDs() {
		return levelDataStorage.keySet();
	}

	public int getStorageSize()  {
		return this.levelDataStorage.size();
	}

	public void storeToFile() {
		try {
			log.info("Writing storage to disk");
			FileOutputStream file = new FileOutputStream("levelStorage.dat");
			ObjectOutputStream out = new ObjectOutputStream(file);
			out.writeObject(this);
			out.close();
			file.close();
			log.info("done.");
		} catch(Exception e) {
			log.error(" Error writing storage to disk");
			e.printStackTrace();
		}
	}

	public void restoreFromFile() {
		try {
			if (new java.io.File("levelStorage.dat").exists()) {
				log.info("Loading storage from disk");
				FileInputStream file = new FileInputStream("levelStorage.dat");
				ObjectInputStream in = new ObjectInputStream(file);
				LevelStorage fileInstance = (LevelStorage) in.readObject();
				ourInstance = fileInstance;
				log.info("done.");
				in.close();
				file.close();
			}
		} catch(Exception e) {
			log.error("Error reading storage from disk");
			e.printStackTrace();
		}
	}
	
	/**
	 * Store the results in Property format
	 * @param filename The file the results should be saved to
	 * @param stdout If the results should also be displayed in stdout
	 */
	public void storeResults(String filename, boolean stdout) {
		Properties p = new Properties();
		levelDataStorage.forEach((id, level) -> {
			p.put(id.toString(), level.getScores().toString());
		});
		try (OutputStream os = new FileOutputStream(filename)) {
			if (stdout) {
				p.store(System.out, "Scores for each level");
			}
			p.store(os, "Scores for each level");
		} catch (IOException e) {
			log.error(e);
		}
	}
}
