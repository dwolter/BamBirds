package de.uniba.sme.bambirds.planner.predicates;

import java.io.BufferedWriter;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.*;
import java.util.concurrent.*;


import de.uniba.sme.bambirds.common.objects.Level;
import de.uniba.sme.bambirds.common.utils.Settings;
import de.uniba.sme.bambirds.planner.behind_the_corner.BehindTheCornerPredicateGenerator;
import de.uniba.sme.bambirds.planner.knowledge.Knowledge;
import de.uniba.sme.bambirds.planner.physicssimulation.exceptions.PredicateGenerationException;
import de.uniba.sme.bambirds.planner.physicssimulation.predicategeneration.SupportPredicateGenerator;
import de.uniba.sme.bambirds.planner.physicssimulation.scene.Scene;
import de.uniba.sme.bambirds.planner.physicssimulation.scene.preprocessing.DefaultPreprocessor;
import org.apache.logging.log4j.Logger;
import org.apache.logging.log4j.LogManager;

/** Manages High Level Execution of Predicate Generators - it is the entry point
 * for the Physics-Simulation-Basded generation of predicates for the Agent by
 * utilizing JBox2D as a physics engine */
public class PredicateGeneratorManager {

    private static final Logger log = LogManager.getLogger(PredicateGeneratorManager.class);

    private List<IPredicateGenerator> generators;

    private final String prologFileName;
    
    
    /** Manages High Level Execution of Predicate Generators - it is the entry point
     * for the Physics-Simulation-Basded generation of predicates for the Agent by
     * utilizing JBox2D as a physics engine
     * @param prologFileName the filname where the results will be written into.
     */
    public PredicateGeneratorManager(String prologFileName) {
        this.generators = new ArrayList<>();
        this.prologFileName = prologFileName;
    }

    /**
     * Builder Method for adding multiple predicate generators
     * @param generator A new Generator
     * @return this manager for chaining
     */
    public PredicateGeneratorManager addPredicateGenerator(IPredicateGenerator generator) {
        this.generators.add(generator);
        return this;
    }


    /**
     * The Main Method for organizing Predicate-Generation of the Bambirds-Agent at runtime.
     * You can modify this method to fit your requirements.
     * @return returns the Filepath to the output-PrologFile.
     */
    public Path buildModel() {

        log.info("Building Simulation Knowledge ...");

        // Prevent duplicates
        Set<Predicate> predicates = new HashSet<>();
        ExecutorService threadPool = Executors.newFixedThreadPool(3);
        Map<IPredicateGenerator, Future<List<Predicate>>> futures = new HashMap<>();
        for (IPredicateGenerator predicateGenerator : generators) {
            log.debug("Starting PredicateGenerator '{}'", predicateGenerator.getClass().getSimpleName());
            futures.put(predicateGenerator, threadPool.submit(predicateGenerator));
        }
        try {
            // Wait a while for existing tasks to terminate
            threadPool.shutdown();
            if (!threadPool.awaitTermination(5, TimeUnit.SECONDS)) {
                threadPool.shutdownNow(); // Cancel currently executing tasks
                // Wait a while for tasks to respond to being cancelled
                if (!threadPool.awaitTermination(500, TimeUnit.MILLISECONDS))
                    log.error("Pool did not terminate");
            }
            for (Map.Entry<IPredicateGenerator,Future<List<Predicate>>> generatorFutureEntry : futures.entrySet()) {
                try {
                    List<Predicate> generatedPredicates = generatorFutureEntry.getValue().get(0,TimeUnit.SECONDS);
                    int sizeBefore = predicates.size();
                    long numberOfSupportPredicates = generatedPredicates.stream().filter(p -> p.getPredicateName().equals("supports")).count();
                    predicates.addAll(generatedPredicates);
                    log.debug("{} of {} predicates were added", predicates.size() - sizeBefore, generatedPredicates.size());
                    log.debug("number of support predicates: {}", numberOfSupportPredicates);
                } catch (ExecutionException e) {
                    log.error("{} failed: ", generatorFutureEntry.getKey().getClass().getName(), e.getCause());
                } catch (TimeoutException e) {
                    log.error("{} did not terminate", generatorFutureEntry.getKey().getClass().getName());
                    generatorFutureEntry.getValue().cancel(true);
                }
            }
        } catch (InterruptedException ie) {
            // (Re-)Cancel if current thread also interrupted
            threadPool.shutdownNow();
            // Preserve interrupt status
            Thread.currentThread().interrupt();
        }
        predicates.add(new Predicate("situation_name", "'" + prologFileName + "'"));
        List<Predicate> result = new ArrayList<>(predicates);
        Collections.sort(result);

        log.info("Done. Writing Knowledge to file ...");
        return writeProlog(result, prologFileName);
    }

    /**
     * Copied from Knowledge.java
     * writes the generated results
     */
    private Path writeProlog(List<Predicate> predicates, String filename) {
		Path filepath = Paths.get(filename + Settings.PROLOG_FILE_EXTENSION).toAbsolutePath().normalize();

		try (BufferedWriter bw = Files.newBufferedWriter(filepath)) {
			for (Predicate predicate : predicates) {
				bw.write(predicate.toString());
			}
		} catch (IOException e) {
			log.error("Couldn't write Prolog: " + e.getMessage(), e);
		}

		return filepath;
	}

	public static PredicateGeneratorManager defaultConfiguration(Level level, String prologFileName) {

        PredicateGeneratorManager manager = new PredicateGeneratorManager(prologFileName);

        // The supports predicate generator performs badly if there are too many objects
        if (Settings.SIMULATION_COMPONENT_ENABLED && level.currentScene.getObjectCount() < 30) {
            Scene scene = new Scene(level.currentScene);
            new DefaultPreprocessor().apply(scene);

            //BE SURE TO GIVE COPIES OF THE SCENE OBJECT TO DIFFERENT
            //PREDICATE GENERATORS
            SupportPredicateGenerator supportPredicateGenerator = new SupportPredicateGenerator(new Scene(scene));
            manager.addPredicateGenerator(supportPredicateGenerator);
        }

        // The old Knowledge generation. Remove once all relevant predicates are generated in extra classes
        Knowledge knowledge = new Knowledge(level);

        return manager
                .addPredicateGenerator(knowledge)
                .addPredicateGenerator(new BehindTheCornerPredicateGenerator(level));
    }

}