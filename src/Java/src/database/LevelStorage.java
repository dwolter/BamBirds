package database;

import helper.CustomLogger;
import meta.Level;

import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Set;

public class LevelStorage {
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
		CustomLogger.info("[Meta] Level " + playedLevel.levelId + " added into database with score " + playedLevel.getBestScore());
	}

	public Set<Integer> getListOfIDs() {
		return levelDataStorage.keySet();
	}

	public int getStorageSize()  {
		return this.levelDataStorage.size();
	}

	public void storeToFile() {
		try {
			CustomLogger.info("[LevelStorage] Writing storage to disk");
			FileOutputStream file = new FileOutputStream("levelStorage.dat");
			ObjectOutputStream out = new ObjectOutputStream(file);
			out.writeObject(this);
			out.close();
			file.close();
			CustomLogger.info("[LevelStorage] done.");
		} catch(Exception e) {
			CustomLogger.severe("[LevelStorage] Error writing storage to disk");
			e.printStackTrace();
		}
	}

	public void restoreFromFile() {
		try {
			if (new java.io.File("levelStorage.dat").exists()) {
				CustomLogger.info("[LevelStorage] Loading storage from disk");
				FileInputStream file = new FileInputStream("levelStorage.dat");
				ObjectInputStream in = new ObjectInputStream(file);
				LevelStorage fileInstance = (LevelStorage) in.readObject();
				ourInstance = fileInstance;
				CustomLogger.info("[LevelStorage] done.");
				in.close();
				file.close();
			}
		} catch(Exception e) {
			CustomLogger.severe("[LevelStorage] Error reading storage from disk");
			e.printStackTrace();
		}
	}
}
