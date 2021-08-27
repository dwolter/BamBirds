package de.uniba.sme.bambirds.planner.predicates;

import java.util.List;
import java.util.concurrent.Callable;

import de.uniba.sme.bambirds.planner.physicssimulation.exceptions.PredicateGenerationException;


/** Implement these Interface to create classes that can produce Predicates. How they generate them is up to you, but you have all the Tools of the implemented Framework at hand  to use. Look at the "SupportPredicateGenerator" to see a fully working example-implementation.*/
public interface IPredicateGenerator extends Callable<List<Predicate>> {

    /**getPredicates will potentially be called by the Bambirds-Agent after the Computer-vision-step and before Shot-Planning.
     * @return return the List of Predicates in String-form
     * @throws PredicateGenerationException if predicate generation fails
     */
    @Override
    List<Predicate> call() throws PredicateGenerationException;

    /**
     * You may decide to implement this Method so you will have an easier Time at development-time when setting up and debugging your predicateGenerators.
     * @param testbedNamePrefix use this to pass a Prefix which then can be appended to the appearing names in the VisualSimulation-Debugger. ( e.g "Level1").
     */
    void addSimulationsToVisualSimulationDebugger(String testbedNamePrefix);

}