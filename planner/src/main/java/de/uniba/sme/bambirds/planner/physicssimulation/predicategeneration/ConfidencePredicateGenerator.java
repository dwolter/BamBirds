package de.uniba.sme.bambirds.planner.physicssimulation.predicategeneration;

import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;

import de.uniba.sme.bambirds.planner.physicssimulation.exceptions.PredicateGenerationException;
import de.uniba.sme.bambirds.planner.predicates.IPredicateGenerator;
import de.uniba.sme.bambirds.planner.predicates.Predicate;


/**
 * Given a List of Lists of predicates (of similar predidcate types), counts the occurence of each individual predicate.
 * Outputs eachs individual predicate as well as a conf() variant determined by how often the predicate occured in each list.
 * 
 * I.e. suppose 5 lists such as [predicate(value1), predicate(value5), ...] are given,
 * the ConfidencePredicateGenerator returns
 * 
 * [predicate(value1), conf(predicate(value1), 0.8), predicate(value5), conf(predicate(value5), 1.0), ...],
 * where the confidence value is determined by how often the respective predicate occurs in the different List,
 * i.e predicate(value1) occurs in 4 of the 5 List (--> 0.8), predicate(value5) in all of them (--> 1.0)
 * 
 * This is usefel when perform multiple similar simulation for the same predicate in order to evaluate its robustness.
 */
public class ConfidencePredicateGenerator implements IPredicateGenerator {

    private List<List<Predicate>> predicateLists;
    private int sampleSize;
    private HashMap<Predicate, Integer> predicateCounts;

    private boolean outputOnlyConfidencePredicates = false;

    public ConfidencePredicateGenerator(List<List<Predicate>> predicateLists) {
        this.predicateLists = predicateLists;
        sampleSize = predicateLists.size();
        predicateCounts = new HashMap<>();
    }
    
    public ConfidencePredicateGenerator(List<List<Predicate>> predicateLists, boolean onlyConfidencePredicates) {
        this.predicateLists = predicateLists;
        sampleSize = predicateLists.size();
        predicateCounts = new HashMap<>();
        outputOnlyConfidencePredicates = onlyConfidencePredicates;
    }
    

	@Override
	public List<Predicate> call() throws PredicateGenerationException {
        for (List<Predicate> predicateList : predicateLists) {
            for (Predicate predicate : predicateList) {
                if(!predicateCounts.containsKey(predicate)){
                    predicateCounts.put(predicate, 0);
                }
                predicateCounts.put(predicate,  predicateCounts.get(predicate) +1);
            }
        }
        List<Predicate> resultingPredicates = new LinkedList<>();
        if(sampleSize > 0){
            for (Predicate predicate : predicateCounts.keySet()) {
                if (!outputOnlyConfidencePredicates) {
                    resultingPredicates.add(predicate);
                }
                float confidence = predicateCounts.get(predicate) / (float) sampleSize;
                String eval = predicate.toString().trim();
                eval = eval.substring(0, eval.length() - 1);

                resultingPredicates.add(new Predicate("conf", eval, confidence ));
            }
        }
		return resultingPredicates;
	}

	@Override
	public void addSimulationsToVisualSimulationDebugger(String testbedNamePrefix) {
		// TODO Auto-generated method stub
	}
	
}
