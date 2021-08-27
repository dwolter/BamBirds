package de.uniba.sme.bambirds.planner.physicssimulation;

import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;

import org.apache.log4j.LogManager;
import org.apache.log4j.Logger;
  
import de.uniba.sme.bambirds.planner.physicssimulation.scene.SerializableScene;

/**A class for collecting any static Utility-Methods which may be useful in multiple places of the physics-simulation.*/
public class SimulationUtils {

    private static final Logger log = LogManager.getLogger(SimulationUtils.class);

	public static void saveSerializableScene(SerializableScene scene, String path) throws IOException {
        try (FileOutputStream fos = new FileOutputStream(path);
            ObjectOutputStream oos = new ObjectOutputStream(fos)) {
                oos.writeObject(scene);
                log.info("Serialized Scene as SerializableScene to " + path);
        }
    }

    public static SerializableScene loadSerializableScene(String path) throws IOException, ClassNotFoundException {
        try (FileInputStream fis = new FileInputStream(path);
            ObjectInputStream ois = new ObjectInputStream(fis)) {
                return (SerializableScene) ois.readObject();
            }
    }



	// String-Manipualtion Functions
	public static String padLeft(String inputString,  int length) {
	   return SimulationUtils.padLeft(inputString, length, " ");
	}

	public static String padLeft(String inputString,  int length, String paddingSymbol) {
	    if (inputString.length() >= length) {
	        return inputString;
	    }
	    StringBuilder sb = new StringBuilder();
	    while (sb.length() < length - inputString.length()) {
	        sb.append(paddingSymbol);
	    }
	    sb.append(inputString);
	
	    return sb.toString();
	}

	public static String padRight(String inputString,  int length) {
	    return SimulationUtils.padRight(inputString, length, " ");
	 }

	public static String padRight(String inputString, int length, String paddingSymbol) {
	    
	    if (inputString.length() >= length) {
	        return inputString;
	    }
	    StringBuilder sb = new StringBuilder();
	
	    sb.append(inputString);
	    while (sb.length() < length) {
	        sb.append(paddingSymbol);
	    }
	    return sb.toString();
	}


}