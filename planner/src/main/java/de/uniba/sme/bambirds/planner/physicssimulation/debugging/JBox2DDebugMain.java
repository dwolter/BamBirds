package de.uniba.sme.bambirds.planner.physicssimulation.debugging;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.LinkedList;
import java.util.List;

import de.uniba.sme.bambirds.planner.physicssimulation.SimulationUtils;
import de.uniba.sme.bambirds.planner.physicssimulation.physics.Simulation;
import de.uniba.sme.bambirds.planner.physicssimulation.physics.modification.stability.AdvancedStabilizerV2;
import de.uniba.sme.bambirds.planner.physicssimulation.predicategeneration.IsReachablePredicateGenerator;
import de.uniba.sme.bambirds.planner.physicssimulation.predicategeneration.SupportPredicateGenerator;
import de.uniba.sme.bambirds.planner.physicssimulation.scene.Scene;
import de.uniba.sme.bambirds.planner.physicssimulation.scene.SceneGenerator;
import de.uniba.sme.bambirds.planner.physicssimulation.scene.entities.SceneCircle;
import de.uniba.sme.bambirds.planner.physicssimulation.scene.modification.ISceneModifier;
import de.uniba.sme.bambirds.planner.physicssimulation.scene.preprocessing.DefaultPreprocessor;

import de.uniba.sme.bambirds.planner.predicates.Predicate;
import org.apache.log4j.Level;
import org.apache.log4j.LogManager;
import org.apache.log4j.Logger;

public class JBox2DDebugMain {
    private static final Logger log = LogManager.getLogger(JBox2DDebugMain.class);


    // BodenPixelhoehe = 384 ( zum Screenshot zeitpunkt )
    // >> 383 = niedrigstes Pixel von Objekten die auf dem Boden stehen


    public static void main(String[] args) {
        LogManager.getRootLogger().setLevel(Level.ALL);

        //------------------------------------------------------------------------------------------------------
        addAllScenesFromCV(true, false);
        //------------------------------------------------------------------------------------------------------


    //    Scene testScene1 = SceneGenerator.createSceneUnderstandingTestScene2();
    //    SceneCircle bird = testScene1.getBirds().get(0);
    //    bird.setLinearVelocityX(45f);
    //    bird.setLinearVelocityY(18f);

    //    Simulation simulation = new Simulation("testscene",testScene1);
    //    simulation.run();
    //    simulation.addToVisualSimulationDebugger(true);


        //------------------------------------------------------------------------------------------------------

//		 try {
//             Scene scene = loadScene("./sim_scenes/situation1-1");
//
//             scene = RunExample( scene, "situation1-1", true);
//
//             SupportPredicateGenerator supportPredicateGenerator = new SupportPredicateGenerator(scene);
//
//             List<Predicate> predicates = new LinkedList<>();
//                 predicates = supportPredicateGenerator.getPredicates();
//                 log.debug("Found Predicates:");
//                 for (Predicate predicate : predicates) {
//                     log.debug(predicate);
//                 }
//                 supportPredicateGenerator.addSimulationsToVisualSimulationDebugger("situation1-1/support/ ");
//		 } catch (Exception e) {
//             e.printStackTrace();
//		 }

        //------------------------------------------------------------------------------------------------------

        // Scene myScene = SceneGenerator.createIsReachableExampleScene();
        // IsReachablePredicateGenerator isReachablePredicateGenerator = new IsReachablePredicateGenerator(myScene, false);
        // try{
        //      List<Predicate> predicates = isReachablePredicateGenerator.getPredicates();
        //          log.debug("Found Predicates:");
        //          for (Predicate predicate : predicates) {
        //              log.debug(predicate);
        //          }
        //     isReachablePredicateGenerator.addSimulationsToVisualSimulationDebugger("situation1-1/support/ ");
        //  } catch (Exception e) {
        //      e.printStackTrace();
        //  }



        //------------------------------------------------------------------------------------------------------
        VisualSimulationDebugger.drawCoordinateSystem = true;
        VisualSimulationDebugger.visualize();

    }



    /**
     * Load all scenes that were stored from computervision and are being stored in the subfolder /sim_scenes/
     * @param includeIntermediateScenes include any scene-snapshots after the initial setup of the scene
     * @param includeScenesAfterFirstShot include any snapshots which may have been created after the first shot has happened
     * @return returns the List of all the loaded Scenes
     */
    private static List<Scene> addAllScenesFromCV(boolean includeIntermediateScenes, boolean includeScenesAfterFirstShot) {

        String rootPath = "./sim_scenes/";
        File folder = new File(rootPath);
        log.debug("path for cv-scenes:" + folder .getAbsolutePath());
        log.debug("Files amount = " + folder.listFiles().length);
        int index = 0;

        List<Scene> loadedScenes = new ArrayList<>();

        while (index <= 20) {
            for (File scene_file: folder.listFiles()) {
                String name = scene_file.getName();
                log.debug("Load " + name);
                if ( (includeScenesAfterFirstShot && name.startsWith("situation" + index + "-")) || (name.startsWith("situation" + index + "-1")) ) {
                    try {
                        Scene scene = RunExample( loadScene(scene_file), name, includeIntermediateScenes);
                        loadedScenes.add(scene);
                    } catch (ClassNotFoundException e) {
                        e.printStackTrace();
                    } catch (IOException e) {
                        e.printStackTrace();
                    }
                }
            }
            index++;
        }
        return loadedScenes;
    }


    /**
     * Load a Scene from a FilePath
     * @param scene_file_path the path to the scene_file
     * @return the loaded scene-object
     */
    private static Scene loadScene( String scene_file_path) {
        Scene loadedScene = null;
        try {
            File scene_file = new File(scene_file_path);
            if(scene_file.exists()){
                loadedScene = loadScene( scene_file);
            }else{
                log.debug("Filepath '" + scene_file_path + "' could not be loaded as valid scenefilepath ");
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
        return loadedScene;
    }



    /**
     * Load a Scene from a File
     * @param scene_file the scene_file
     * @return the loaded scene-object
    */
    private static Scene loadScene( File scene_file)
            throws IOException, ClassNotFoundException {
        Scene scene = new Scene(SimulationUtils.loadSerializableScene(scene_file.getPath()));
        return scene;
    }


    /**
     * Example for Loading a Scene, adding a prepocessor, adding worldModifiers and putting it into the VisualDebugger
     * @param scene the scene you want to run as example
     * @param name give the Scene a name
     * @param includeIntermediateScenes  include any scene-snapshots after the initial setup of the scene
     * @return
     * @throws IOException
     * @throws ClassNotFoundException
     */
    private static Scene RunExample( Scene scene, String name, boolean includeIntermediateScenes)
        throws IOException, ClassNotFoundException {
        ISceneModifier preprocessor = new DefaultPreprocessor();
        preprocessor.apply(scene);

        Simulation sim = new Simulation(name, scene);
        sim.addPreparationPhaseModifier(new AdvancedStabilizerV2());
        sim.run();
        if (!sim.isSuccessful()) {
            log.warn("Simulation did not complete successfully");
        }

        sim.addToVisualSimulationDebugger(true);

        log.debug("LOGGING FINAL ENTITES");
        sim.getFinalSnapshot().logAllEntities();

        return sim.getFinalSnapshot();

    }


}