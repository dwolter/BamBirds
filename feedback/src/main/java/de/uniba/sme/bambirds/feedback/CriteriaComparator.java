package de.uniba.sme.bambirds.feedback;

import java.io.PrintWriter;
import java.io.StringWriter;
import java.util.ArrayList;
import java.nio.charset.StandardCharsets;
import java.io.BufferedWriter;
import java.io.IOException;
import java.util.LinkedList;
import java.util.List;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import java.nio.file.*;
import java.io.BufferedReader;

/**
 * Evaluates the Criterias executed in {@link de.uniba.sme.bambirds.feedback.UnsureEvaluation} and writes the results in the main repo in a .txt file called Evaluation.txt.
 * the format of the file is the following:
 * 
 * ------------------------------------------------------
 * "Levels:"
 * "LevelID pointsbeforecriterias pointsaftercriterias"
 * ... (list of levels)
 * "Criterias:"
 * "criteria: timesCriteriaInvokedSuccessfully/timesCriteriaInvoked/timesCriteriaInvokedtogetherwithothercriterias"
 * ... (list of criterias)
 * "size of the failedShotList" (this contains all shots found by {@link de.uniba.sme.bambirds.feedback.Evaluation} and {@link de.uniba.sme.bambirds.feedback.UnsureEvaluation})
 * "the number of shots actually removed in {@link de.uniba.sme.bambirds.planner.ShotSelection}"
 * ------------------------------------------------------
 * 
 * to add a new criteria to this class, you need to: higher the variable criteriaCount, add the criteria to the enum {@link de.uniba.sme.bambirds.feedback.EvaluationCriteria} and add them to the switch statements in 
 * {@code getCriteriaNumber} and {@code getCriteriaStringFromNumber}.
 * 
 * The "Evaluation.txt" file only gets created or updated after the first shot of a new level has been shot with the data of the previous level
 *
 * @author Hannes Altmann
 * 
 */

public class CriteriaComparator {
	
		private int notEnoughComparisonData;
		private int criteriaCount = 7;
		private boolean useUnsureCriterias;
		private int currentLevel;
		private int pointsForCurrentLevel;
		private List<EvaluationCriteria> invokedCriteriasThisLevel;
		private static String filePathString = "Evaluation.txt";
		private List<String> stringText;
		private static final Logger log = LogManager.getLogger(CriteriaComparator.class);
		//info for text display
		private int[] criteriaInvokedNumbers;
		private int[] criteriaInvokedSuccessfullyNumbers;
		private int[] criteriaInvokedWithOtherUnsureCriterias;
		private List<LevelValuePair> pointsForEachLevelWithoutCriterias;
		private List<LevelValuePair> pointsForEachLevelWithCriterias;
		private int failedShotSize;
		private int removedShotsCounter;

	public CriteriaComparator(boolean useUnsureCriterias){
		this.notEnoughComparisonData = 0;
		this.currentLevel = -1;
		this.pointsForCurrentLevel = 0;
		this.stringText = initialStringText();
		this.useUnsureCriterias = useUnsureCriterias;
		this.criteriaInvokedNumbers = new int[criteriaCount];
		this.criteriaInvokedSuccessfullyNumbers = new int[criteriaCount];
		this.criteriaInvokedWithOtherUnsureCriterias = new int[criteriaCount];
		this.invokedCriteriasThisLevel = new ArrayList<EvaluationCriteria>();
		this.pointsForEachLevelWithoutCriterias = new ArrayList<LevelValuePair>();
		this.pointsForEachLevelWithCriterias = new ArrayList<LevelValuePair>();
		this.failedShotSize = 0;
		this.removedShotsCounter = 0;
		readExistingFile();
		updateStringText();
	}

	public void setUseUnsureCriterias(boolean useUnsureCriterias){
		this.useUnsureCriterias = useUnsureCriterias;
	}

	public boolean getUseUnsureCriterias(){
		return useUnsureCriterias;
	}

	public List<String> getStringText(){
		return stringText;
	}

	public static void resetSaveFile(){
		try{
			Files.deleteIfExists(Paths.get(filePathString));
			log.info("evaluation.txt was deleted");
		} catch (IOException e){
			log.info("Evaluation.txt could not be deleted");
		}
	}

	public List<String> getFileText(){
		List<String> lines = new ArrayList<String>();
		try(BufferedReader reader = Files.newBufferedReader(Paths.get(filePathString), StandardCharsets.UTF_8)){
				String line = reader.readLine();
				while (line != null){
					lines.add(line);
					line = reader.readLine();
				}
			} catch (IOException e) {
				log.info("Error while reading evaluation file:"+e.getMessage());
			}
			return lines;
	}

	private void readExistingFile(){
		if(Files.exists(Paths.get(filePathString))){
			try(BufferedReader reader = Files.newBufferedReader(Paths.get(filePathString), StandardCharsets.UTF_8)){
				String line = reader.readLine();
				while (line != null){
					if(line.equals("Levels:")){
						line = readLevels(reader, line);
					} else if(line.equals("Criterias:")) {
						line = readCriterias(reader, line);
						break;
					} else {
						line = reader.readLine();
					}
				}
				return;
			} catch (IOException e) {
				log.info("Error while reading evaluation file:"+e.getMessage());
			}
		}
		return;
	}

	private String readLevels(BufferedReader reader, String line) throws IOException{
		line = reader.readLine();
		line = reader.readLine();//read past example line
		while(line!=null){
			if(line.equals("Criterias:")||line.equals("")){
				return line;
			}
			String[] data = line.split("/");
			int parsedPointsWithoutCriterias;
			try{
				parsedPointsWithoutCriterias = Integer.parseInt(data[1]);
			} catch (NumberFormatException e){
				line = reader.readLine();
				continue;
			}

			pointsForEachLevelWithoutCriterias.add(new LevelValuePair(Integer.parseInt(data[0]),parsedPointsWithoutCriterias));
			pointsForEachLevelWithCriterias.add(new LevelValuePair(Integer.parseInt(data[0]), Integer.parseInt(data[2])));
			line = reader.readLine();
		}
		return line;
	}

	private String readCriterias(BufferedReader reader, String line) throws IOException{
		line = reader.readLine();
		line = reader.readLine();//read past example line
		int count = 0;
		while(line!=null){
			if(line.startsWith("current size of FailedShot: ")){
				break;
			}
			String[] data = line.split("/");
			if(!line.contains("/")){
				line = reader.readLine();
				continue;
			}
			criteriaInvokedSuccessfullyNumbers[count] = Integer.parseInt(data[1]);
			criteriaInvokedNumbers[count] = Integer.parseInt(data[2]);
			criteriaInvokedWithOtherUnsureCriterias[count] = Integer.parseInt(data[3]);
			count++;
			line = reader.readLine();
		}
		return line;
	}

	private void writeFile(){
		resetSaveFile();
		try(BufferedWriter writer = Files.newBufferedWriter(Paths.get(filePathString), StandardCharsets.UTF_8,StandardOpenOption.CREATE)){
			for(String line: stringText){
				writer.write(line);
				writer.newLine();
			}
			log.info("Evaluation has been written! check evaluation.txt for details");
			for(String string:stringText){
				log.info(string);
			}
		} catch (IOException e){
			log.info("[CriteriaComparator]: the logging file could not be written");
		}
	}
	private void updateStringText(){
		List<String> lines = new ArrayList<String>();
		lines.add("Evaluation of Criterias:");
		lines.add("");
		lines.add("Levels:");
		lines.add("LevelID/Pointsbeforecriterias/Pointsaftercriterias");
		for(LevelValuePair pair : pointsForEachLevelWithCriterias){
			LevelValuePair correspondingPair = null;
			for(LevelValuePair pair2 : pointsForEachLevelWithoutCriterias){
				if(pair.getLevel()==pair2.getLevel()){
					correspondingPair = pair2;
				}
			}
			if(correspondingPair==null){
				lines.add(pair.getLevel()+"/"+"notExecuted"+"/"+pair.getValue());
			} else {
				lines.add(pair.getLevel()+"/"+correspondingPair.getValue()+"/"+pair.getValue());
			}
		}
		lines.add("");
		lines.add("Criterias:");
		lines.add("criteria/timesCriteriaInvokedSuccessfully/timesCriteriaInvoked/timesCriteriaInvokedTogetherWithOtherCriterias");
		for(int i = 0; i<criteriaCount; i++){
			lines.add(getCriteriaStringFromNumber(i)+"/"+ criteriaInvokedSuccessfullyNumbers[i] +"/"+ criteriaInvokedNumbers[i]+"/"+criteriaInvokedWithOtherUnsureCriterias[i]);
		}

		lines.add("");
		lines.add("current size of FailedShot: "+failedShotSize);
		lines.add("shots deleted ShotSelection: "+removedShotsCounter);
		stringText = lines;

	}

	private List<String> initialStringText(){
		List<String> initialText = new LinkedList<String>();
		initialText.add("Evaluation of the Criterias:");
		initialText.add(System.lineSeparator());
		initialText.add("No data present yet");
		return initialText;
	}

	/**
	 * Adds the criterias found at the current shot to the currently saved shots. If the shot is the first of a new level, a new "Evaluation.txt" gets created.
	 *
	 * @param criterias A list of the criterias found this level. May be Empty but not null.
	 * @param pointsGained The points gained with the current shot
	 * @param newLevel The number of the current level
	 * @param failedShotSize The current size of the failedShotList from {@link de.uniba.sme.bambirds.feedback.FeedbackManager}
	 * @param removedShotsCounter the number of shots actually removed in {@link de.uniba.sme.bambirds.planner.ShotSelection}
	 *
	 */
	 
	public void compareNewInvokedCriterias(List<EvaluationCriteria> criterias, int pointsGained, int newLevel, int failedShotSize, int removedShotsCounter){
		this.failedShotSize = failedShotSize;
		this.removedShotsCounter = this.removedShotsCounter + removedShotsCounter;
		if(currentLevel==-1){
			currentLevel = newLevel;
		}

		if(currentLevel!=newLevel){
			if(useUnsureCriterias){ //geht so nicht weil wenn es einmal durchgelaufen ist wird einfach wieder an die liste hinten angefügt
				addToListOnLevelSlot(pointsForEachLevelWithCriterias, pointsForCurrentLevel);

			//get criteria invoked count
			for(EvaluationCriteria criteria: invokedCriteriasThisLevel){
				criteriaInvokedNumbers[getCriteriaNumber(criteria)]++;
			}

			//get successfully invoked count

			if(hasLevelChangedSuccessfully()){
				for(EvaluationCriteria criteria: invokedCriteriasThisLevel){
					criteriaInvokedSuccessfullyNumbers[getCriteriaNumber(criteria)]++;
				}
			}
			} else {
				addToListOnLevelSlot(pointsForEachLevelWithoutCriterias, pointsForCurrentLevel);
			}




			currentLevel = newLevel;
			pointsForCurrentLevel = 0;
			updateStringText();
			writeFile();
			invokedCriteriasThisLevel = new ArrayList<EvaluationCriteria>();
		}
		pointsForCurrentLevel = pointsForCurrentLevel + pointsGained;
		for(EvaluationCriteria criteria : criterias){
			invokedCriteriasThisLevel.add(criteria);
		}

		if(criterias.size()>1){
			for(EvaluationCriteria criteria: criterias){
				criteriaInvokedWithOtherUnsureCriterias[getCriteriaNumber(criteria)]++;
			}
		}
	}

	private int getCriteriaNumber(EvaluationCriteria criteria){
		switch(criteria){
			case POINTSGAINEDTOOLOW: return 0;
			case PIGSHITBUTNOTENOUGHPOINTS: return 1;
			case POINTDISTRIBUTIONNOTGOOD: return 2;
			case NOTMUCHMOVEDANDNOPIGS: return 3;
			case NOTMUCHMOVEDBUTPIGS: return 4;
			case OBJECTSMOVEDDISTRIBUTIONNOTGOOD: return 5;
			case NEARMISSLIKELY: return 6;
			default: return -1;
		}
	}

	private String getCriteriaStringFromNumber(int i){
		switch(i){
			case 0: return "NotEnoughPoints";
			case 1: return "PigsHitButVeryLowPoints";
			case 2: return "PointDistributionOverLevelIsNotGood";
			case 3: return "NotMuchHasMovedAndNoPigWereHit";
			case 4: return "NotMuchHasMovedButPigsWereHit";
			case 5: return "MuchLessObjectsWereMovedThanLevelAverage";
			case 6: return "TheShotWasVeryLikelyANearMiss";
			default: return "No criteria for this number found";
		}
	}


	private void addToListOnLevelSlot(List<LevelValuePair> list, int value){
		for(LevelValuePair pair : list){
			if(pair.getLevel()==currentLevel){
				pair.setValue(value);
				return;
			}
		}
		list.add(new LevelValuePair(currentLevel, value));
		return;
	}

	private boolean hasLevelChangedSuccessfully(){
		int withCriteriaLevelPoints = -1;
		int withoutCriteriaLevelPoints = -1;

		for(LevelValuePair pair : pointsForEachLevelWithCriterias){
			if(pair.getLevel() == currentLevel){
				withCriteriaLevelPoints = pair.getValue();
			}
		}

		for(LevelValuePair pair : pointsForEachLevelWithoutCriterias){
			if(pair.getLevel() == currentLevel){
				withoutCriteriaLevelPoints = pair.getValue();
			}
		}

		if((withCriteriaLevelPoints==-1)||(withoutCriteriaLevelPoints==-1)){
			notEnoughComparisonData++;
			return false;
		}

		if(withCriteriaLevelPoints>withoutCriteriaLevelPoints){
			return true;
		}else{
			return false;
		}

	}
}
