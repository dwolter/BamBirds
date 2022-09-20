package de.uniba.sme.bambirds.feedback;

import org.junit.jupiter.api.Test;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;

public class CriteriaComparatorTest {
//TODO: with multiple cirterias, with unused, with error values/Exceptions, with different values/file reading
	private List<String> addStandardLevelText(List<String> stringText){
		stringText.add("Evaluation of Criterias:");
		stringText.add("");
		stringText.add("Levels:");
		stringText.add("LevelID/Pointsbeforecriterias/Pointsaftercriterias");
		return stringText;
	}

	private List<String> addStandardCriteriaText(List<String> stringText){
		stringText.add("");
		stringText.add("Criterias:");
		stringText.add("criteria/timesCriteriaInvokedSuccessfully/timesCriteriaInvoked/timesCriteriaInvokedTogetherWithOtherCriterias");
		return stringText;
	}

	private List<String> oneCorrectInputText(){
		List<String> result = new ArrayList<String>();
		addStandardLevelText(result);
		result.add("1/notExecuted/999");
		addStandardCriteriaText(result);
		result.add("NotEnoughPoints/0/1/0");
		result.add("PigsHitButVeryLowPoints/0/0/0");
		result.add("PointDistributionOverLevelIsNotGood/0/0/0");
		result.add("NotMuchHasMovedAndNoPigWereHit/0/0/0");
		result.add("NotMuchHasMovedButPigsWereHit/0/0/0");
		result.add("MuchLessObjectsWereMovedThanLevelAverage/0/0/0");
		result.add("TheShotWasVeryLikelyANearMiss/0/0/0");
		result.add("");
		result.add("current size of FailedShot: "+1);
		result.add("shots deleted ShotSelection: "+0);
		return result;
	}

	private List<String> multipleCorrectInputsOnDifferentLevelsText(){
		List<String> result = new ArrayList<String>();
		addStandardLevelText(result);
		result.add("1/notExecuted/51104");
		result.add("5/notExecuted/4566");
		addStandardCriteriaText(result);
		result.add("NotEnoughPoints/0/3/0");
		result.add("PigsHitButVeryLowPoints/0/0/0");
		result.add("PointDistributionOverLevelIsNotGood/0/0/0");
		result.add("NotMuchHasMovedAndNoPigWereHit/0/0/0");
		result.add("NotMuchHasMovedButPigsWereHit/0/0/0");
		result.add("MuchLessObjectsWereMovedThanLevelAverage/0/0/0");
		result.add("TheShotWasVeryLikelyANearMiss/0/0/0");
		result.add("");
		result.add("current size of FailedShot: "+1);
		result.add("shots deleted ShotSelection: "+0);
		return result;
	}

	private List<String> differentCriteriaInputText(){
		List<String> result = new ArrayList<String>();
		addStandardLevelText(result);
		result.add("1/notExecuted/23042");
		result.add("2/notExecuted/3800");
		result.add("3/notExecuted/5400");
		result.add("4/notExecuted/4994");
		addStandardCriteriaText(result);
		result.add("NotEnoughPoints/0/2/2");
		result.add("PigsHitButVeryLowPoints/0/3/2");
		result.add("PointDistributionOverLevelIsNotGood/0/1/1");
		result.add("NotMuchHasMovedAndNoPigWereHit/0/3/3");
		result.add("NotMuchHasMovedButPigsWereHit/0/0/0");
		result.add("MuchLessObjectsWereMovedThanLevelAverage/0/0/0");
		result.add("TheShotWasVeryLikelyANearMiss/0/1/1");
		result.add("");
		result.add("current size of FailedShot: "+1);
		result.add("shots deleted ShotSelection: "+0);
		return result;
	}

	private List<String> useWithoutCriteriaText(){
		List<String> result = new ArrayList<String>();
		addStandardLevelText(result);
		result.add("1/1023/2003");
		result.add("2/535/1003");
		addStandardCriteriaText(result);
		result.add("NotEnoughPoints/0/1/0");
		result.add("PigsHitButVeryLowPoints/0/0/0");
		result.add("PointDistributionOverLevelIsNotGood/0/0/0");
		result.add("NotMuchHasMovedAndNoPigWereHit/0/2/0");
		result.add("NotMuchHasMovedButPigsWereHit/0/0/0");
		result.add("MuchLessObjectsWereMovedThanLevelAverage/0/0/0");
		result.add("TheShotWasVeryLikelyANearMiss/0/0/0");
		result.add("");
		result.add("current size of FailedShot: "+0);
		result.add("shots deleted ShotSelection: "+0);
		return result;
	}

	@Test
	public void oneCorrectInputGivesCorrectResult(){
		CriteriaComparator.resetSaveFile();
		CriteriaComparator comparator = new CriteriaComparator(true);
		List<EvaluationCriteria> envokedCriterias = new ArrayList<EvaluationCriteria>();
		envokedCriterias.add(EvaluationCriteria.POINTSGAINEDTOOLOW);
		comparator.compareNewInvokedCriterias(envokedCriterias, 999, 1, 1, 0);
		comparator.compareNewInvokedCriterias(envokedCriterias, 999, 2, 1, 0);
		assertEquals(oneCorrectInputText(),comparator.getStringText());
	}

	@Test
	public void multipleCorrectInputsOnDifferentLevelsGiveCorrectResult(){
		CriteriaComparator.resetSaveFile();
		CriteriaComparator comparator = new CriteriaComparator(true);
		List<EvaluationCriteria> envokedCriterias1 = new ArrayList<EvaluationCriteria>();
		List<EvaluationCriteria> envokedCriterias2 = new ArrayList<EvaluationCriteria>();
		List<EvaluationCriteria> envokedCriterias3 = new ArrayList<EvaluationCriteria>();
		List<EvaluationCriteria> envokedCriterias4 = new ArrayList<EvaluationCriteria>();
		List<EvaluationCriteria> envokedCriterias5 = new ArrayList<EvaluationCriteria>();
		List<EvaluationCriteria> envokedCriterias6 = new ArrayList<EvaluationCriteria>();
		envokedCriterias1.add(EvaluationCriteria.POINTSGAINEDTOOLOW);
		envokedCriterias2.add(EvaluationCriteria.POINTSGAINEDTOOLOW);
		envokedCriterias3.add(EvaluationCriteria.POINTSGAINEDTOOLOW);
		envokedCriterias6.add(EvaluationCriteria.POINTSGAINEDTOOLOW);
		comparator.compareNewInvokedCriterias(envokedCriterias1, 999, 1, 1, 0);
		comparator.compareNewInvokedCriterias(envokedCriterias2, 5, 1, 1, 0);
		comparator.compareNewInvokedCriterias(envokedCriterias3, 100, 1, 1, 0);
		comparator.compareNewInvokedCriterias(envokedCriterias4, 50000, 1, 1, 0);
		comparator.compareNewInvokedCriterias(envokedCriterias5, 4566, 5, 1, 0);
		comparator.compareNewInvokedCriterias(envokedCriterias6, 0, 6, 1, 0);
		assertEquals(multipleCorrectInputsOnDifferentLevelsText(), comparator.getStringText());
	}

	@Test
	public void differentCriteriaInputGivesCorrectResult(){
		CriteriaComparator.resetSaveFile();
		CriteriaComparator comparator = new CriteriaComparator(true);
		List<EvaluationCriteria> envokedCriterias1 = new ArrayList<EvaluationCriteria>();
		List<EvaluationCriteria> envokedCriterias2 = new ArrayList<EvaluationCriteria>();
		List<EvaluationCriteria> envokedCriterias3 = new ArrayList<EvaluationCriteria>();
		List<EvaluationCriteria> envokedCriterias4 = new ArrayList<EvaluationCriteria>();
		List<EvaluationCriteria> envokedCriterias5 = new ArrayList<EvaluationCriteria>();
		List<EvaluationCriteria> envokedCriterias6 = new ArrayList<EvaluationCriteria>();
		List<EvaluationCriteria> envokedCriterias7 = new ArrayList<EvaluationCriteria>();
		envokedCriterias2.add(EvaluationCriteria.POINTSGAINEDTOOLOW);
		envokedCriterias2.add(EvaluationCriteria.PIGSHITBUTNOTENOUGHPOINTS);
		envokedCriterias2.add(EvaluationCriteria.NEARMISSLIKELY);
		envokedCriterias3.add(EvaluationCriteria.PIGSHITBUTNOTENOUGHPOINTS);
		envokedCriterias4.add(EvaluationCriteria.POINTDISTRIBUTIONNOTGOOD);
		envokedCriterias4.add(EvaluationCriteria.NOTMUCHMOVEDANDNOPIGS);
		envokedCriterias5.add(EvaluationCriteria.NOTMUCHMOVEDANDNOPIGS);
		envokedCriterias5.add(EvaluationCriteria.POINTSGAINEDTOOLOW);
		envokedCriterias7.add(EvaluationCriteria.PIGSHITBUTNOTENOUGHPOINTS);
		envokedCriterias7.add(EvaluationCriteria.NOTMUCHMOVEDANDNOPIGS);
		comparator.compareNewInvokedCriterias(envokedCriterias1,3040,1, 1, 0);
		comparator.compareNewInvokedCriterias(envokedCriterias2,10001,1, 1, 0);
		comparator.compareNewInvokedCriterias(envokedCriterias3,10001,1, 1, 0);
		comparator.compareNewInvokedCriterias(envokedCriterias4,400,2, 1, 0);
		comparator.compareNewInvokedCriterias(envokedCriterias5,3400,2, 1, 0);
		comparator.compareNewInvokedCriterias(envokedCriterias6,5400,3, 1, 0);
		comparator.compareNewInvokedCriterias(envokedCriterias7,4994,4, 1, 0);
		//new dummy level so everything gets printed
		comparator.compareNewInvokedCriterias(new ArrayList<EvaluationCriteria>(), -2, -2, 1, 0);
		comparator.resetSaveFile();
		assertEquals(differentCriteriaInputText(), comparator.getStringText());
	}

	@Test
	public void useWithoutCriteriaWorks(){
		CriteriaComparator.resetSaveFile();
		CriteriaComparator comparator = new CriteriaComparator(true);
		List<EvaluationCriteria> envokedCriterias1 = new ArrayList<EvaluationCriteria>();
		List<EvaluationCriteria> envokedCriterias2 = new ArrayList<EvaluationCriteria>();
		List<EvaluationCriteria> envokedCriterias3 = new ArrayList<EvaluationCriteria>();
		envokedCriterias1.add(EvaluationCriteria.POINTSGAINEDTOOLOW);
		envokedCriterias2.add(EvaluationCriteria.NOTMUCHMOVEDANDNOPIGS);
		envokedCriterias3.add(EvaluationCriteria.NOTMUCHMOVEDANDNOPIGS);
		comparator.compareNewInvokedCriterias(envokedCriterias1,1001,1, 0, 0);
		comparator.compareNewInvokedCriterias(envokedCriterias2,1002,1, 0, 0);
		comparator.compareNewInvokedCriterias(envokedCriterias3,1003,2, 0, 0);
		//new dummy level so everything gets printed
		comparator.compareNewInvokedCriterias(new ArrayList<EvaluationCriteria>(), -2, -2, 0, 0);

		comparator.setUseUnsureCriterias(false);
		comparator.compareNewInvokedCriterias(new ArrayList<EvaluationCriteria>(),20, 1, 0, 0);
		comparator.compareNewInvokedCriterias(new ArrayList<EvaluationCriteria>(),1003, 1, 0, 0);
		comparator.compareNewInvokedCriterias(new ArrayList<EvaluationCriteria>(),535, 2, 0, 0);
		//new dummy level so everything gets printed
		comparator.compareNewInvokedCriterias(new ArrayList<EvaluationCriteria>(), -2, -2, 0, 0);
		comparator.resetSaveFile();
		assertEquals(useWithoutCriteriaText(), comparator.getStringText());
	}

	@Test
	public void savingDataWithFileWorks() throws IOException{
		CriteriaComparator.resetSaveFile();
		//data from previous test
		CriteriaComparator comparator = new CriteriaComparator(true);
		List<EvaluationCriteria> envokedCriterias1 = new ArrayList<EvaluationCriteria>();
		List<EvaluationCriteria> envokedCriterias2 = new ArrayList<EvaluationCriteria>();
		List<EvaluationCriteria> envokedCriterias3 = new ArrayList<EvaluationCriteria>();
		envokedCriterias1.add(EvaluationCriteria.POINTSGAINEDTOOLOW);
		envokedCriterias2.add(EvaluationCriteria.NOTMUCHMOVEDANDNOPIGS);
		envokedCriterias3.add(EvaluationCriteria.NOTMUCHMOVEDANDNOPIGS);
		comparator.compareNewInvokedCriterias(envokedCriterias1,1001,1, 1, 0);
		comparator.compareNewInvokedCriterias(envokedCriterias2,1002,1, 1, 0);
		comparator.compareNewInvokedCriterias(envokedCriterias3,1003,2, 1, 0);
		//new dummy level so everything gets printed
		comparator.compareNewInvokedCriterias(new ArrayList<EvaluationCriteria>(), -2, -2, 1, 0);

		comparator.setUseUnsureCriterias(false);
		comparator.compareNewInvokedCriterias(new ArrayList<EvaluationCriteria>(),20, 1, 1, 0);
		comparator.compareNewInvokedCriterias(new ArrayList<EvaluationCriteria>(),1003, 1, 1, 0);
		comparator.compareNewInvokedCriterias(new ArrayList<EvaluationCriteria>(),535, 2, 1, 0);
		//new dummy level so everything gets printed
		comparator.compareNewInvokedCriterias(new ArrayList<EvaluationCriteria>(), -2, -2, 1, 0);
		CriteriaComparator comparator2 = new CriteriaComparator(true);
		assertEquals(useWithoutCriteriaText(), comparator2.getStringText());

	}
}
