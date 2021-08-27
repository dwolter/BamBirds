package de.uniba.sme.bambirds.planner.physicssimulation.scene;
import java.awt.Point;
import java.util.ArrayList;
import java.util.LinkedList;
import java.util.List;
import java.util.Random;

import org.apache.log4j.LogManager;
import org.apache.log4j.Logger;
import org.jbox2d.common.Vec2;

import de.uniba.sme.bambirds.common.objects.ab.ABType;
import de.uniba.sme.bambirds.common.objects.ab.LineSegment;
import de.uniba.sme.bambirds.common.objects.ab.shape.Circle;
import de.uniba.sme.bambirds.common.objects.ab.shape.Poly;
import de.uniba.sme.bambirds.common.objects.ab.shape.Rect;
import de.uniba.sme.bambirds.planner.physicssimulation.debugging.VisualSimulationDebugger;
import de.uniba.sme.bambirds.planner.physicssimulation.physics.Simulation;
import de.uniba.sme.bambirds.planner.physicssimulation.physics.modification.stability.AdvancedStabilizerV2;
import de.uniba.sme.bambirds.planner.physicssimulation.scene.entities.SceneCircle;
import de.uniba.sme.bambirds.planner.physicssimulation.scene.entities.SceneEntity;
import de.uniba.sme.bambirds.planner.physicssimulation.scene.entities.ScenePolygon;
import de.uniba.sme.bambirds.planner.physicssimulation.scene.entities.SceneRectangle;
import de.uniba.sme.bambirds.planner.physicssimulation.scene.modification.insertion.AddGround;



/** This may be used for artifically generating Scenes ourselves, instead of only relying on scenes imported by the agent.
 * The created Scenes can be used for  evaluation-; development- or debugging-purposes.
*/
public class SceneGenerator {
    private static final Logger log = LogManager.getLogger(SceneGenerator.class);

    private static int _id = 0;
    private static void resetID(){
        _id=0;
    }
    private static int getNextID(){
        _id++;
        return _id;
    }

    public static Scene createIsReachableExampleScene(){
        /** This serves as an example of creating and loading an arbitrary Scene. */
        List<SceneEntity> sceneEntities = new ArrayList<>();
        resetID();

        sceneEntities.add(SceneRectangle.createSceneRectangle("stone1", getNextID(), ABType.Stone, 3f, 0.5f, 28, 26.5f, 0f ));
        sceneEntities.add(SceneRectangle.createSceneRectangle("stone2", getNextID(), ABType.Stone, 3f, 0.5f, 29, 26.5f, 0f ));

        sceneEntities.add(SceneRectangle.createSceneRectangle("stone4", getNextID(), ABType.Stone, 6f, 0.5f, 36, 18, 0f ));
        sceneEntities.add(SceneRectangle.createSceneRectangle("stone5", getNextID(), ABType.Stone, 6f, 0.5f, 39.5f, 18, 0f ));
        sceneEntities.add(SceneRectangle.createSceneRectangle("stone6", getNextID(), ABType.Stone, 0.5f, 4f, 37.75f, 21.25f, 0f));
        sceneEntities.add(SceneRectangle.createSceneRectangle("stone_left", getNextID(), ABType.Stone, 1f, 1f,12f, 10.5f, 0f));
        sceneEntities.add(SceneRectangle.createSceneRectangle("stone_right", getNextID(), ABType.Stone, 1f, 1f,58, 20.5f, 0f));

        sceneEntities.add(SceneCircle.createSceneCircle("Pig 1", getNextID(), ABType.Stone, 0.6f,  30f, 25.6f));
        sceneEntities.add(SceneCircle.createSceneCircle("Pig 2", getNextID(), ABType.Stone, 0.6f,  37f, 22.1f));
        sceneEntities.add(SceneCircle.createSceneCircle("Pig 3", getNextID(), ABType.Stone, 0.6f,  48f, 15.6f));

        ScenePolygon scenePolygon1 = ScenePolygon.createScenePolygon("poly_bnottom", getNextID(), ABType.Hill, 0f, 0f, 0f, new Vec2[]{
            new Vec2(0,0),
            new Vec2(0,10),
            new Vec2(20,10),
            new Vec2(24,15),
            // new Vec2(31,15),
            new Vec2(27,25),
            new Vec2(32,25),
            new Vec2(35,15),
            new Vec2(35,15),
            new Vec2(40,15),
            new Vec2(45,10),
            new Vec2(50,10),
            new Vec2(50,14),
            new Vec2(46,14),
            new Vec2(46,15),
            new Vec2(50,15),
            new Vec2(50,20),
            new Vec2(60,20),
            new Vec2(70,0)
        });
        sceneEntities.add(scenePolygon1);

        ScenePolygon scenePolygon2 = ScenePolygon.createScenePolygon("poly_top", getNextID(), ABType.Hill, 0f, 0f, 0f, new Vec2[]{
            new Vec2(20,30),
            new Vec2(20,40),
            new Vec2(100,50),
            new Vec2(100,30),
            new Vec2(50,30),
            new Vec2(50,35),
            new Vec2(40,35),
            new Vec2(40,25),
            new Vec2(38,25),
            new Vec2(38,35),
            new Vec2(35,35),
            new Vec2(30,35),
            // new Vec2(25,25),
            // new Vec2(25,20)

        });
        sceneEntities.add(scenePolygon2);


        Scene scene = new Scene(sceneEntities);
        scene.slingShotCenterPosition = new Vec2(20f,20f);
        AddGround addGroundModifier =  new AddGround();
        addGroundModifier.apply(scene);
        // scene.logAllEntities();
        Simulation testSimulation = new Simulation("My Own Scene 01", scene);
        VisualSimulationDebugger.addScene(testSimulation.getName(), scene, testSimulation.getSettings());
        return scene;

    }

    public static Scene createExampleScene02(){
        List<SceneEntity> sceneEntities = new ArrayList<>();
        resetID();

        for (int i = 0; i < 25; i++) {
            sceneEntities.add(SceneRectangle.createSceneRectangle("stone_"+i, getNextID(), ABType.Stone, 1f + (i %10), 3f + (i%13), 11f + 0.25f*(i % 17), 1f + 2.5f*i, 0f + 0.25f*i ));

        }
        Scene scene = new Scene(sceneEntities);
        scene.slingShotCenterPosition = new Vec2(2f, 2f);
        AddGround addGroundModifier =  new AddGround();
        addGroundModifier.apply(scene);
        // scene.logAllEntities();
        Simulation testSimulation = new Simulation("My Own Scene 01", scene);
        VisualSimulationDebugger.addScene(testSimulation.getName(), scene, testSimulation.getSettings());
        return scene;
    }

    public static Scene createStabilityEvaluationScene() {
        //NOTE Position and Size Values should be close to value given by vision BEFORE preprocessing!
        //Rule of Thumb: 10x target size:
        //--> Wood of length 1 in simulation, construct as wood of length 10

        List<SceneEntity> sceneEntities = new LinkedList<SceneEntity>();
        resetID();

        Vec2[] vertices1 = new Vec2[]{
            new Vec2(0, 0),
            new Vec2(200, 0),
            new Vec2(200, -100),
            new Vec2(100, -300),
            new Vec2(0, -300),
        };
        sceneEntities.add(ScenePolygon.createScenePolygon("GroundPoly1", getNextID(), ABType.Ground, 0f, 0f, 0f, vertices1));

        Vec2[] vertices2 = new Vec2[]{
            new Vec2(200, 0),
            new Vec2(600, 0),
            new Vec2(500, -100),
            new Vec2(200, -100),
        };
        sceneEntities.add(ScenePolygon.createScenePolygon("GroundPoly2", getNextID(), ABType.Ground, 0f, 0f, 0f, vertices2));

        Vec2[] vertices3 = new Vec2[]{
            new Vec2(-300, 50),
            new Vec2(900, 50),
            new Vec2(900, 0),
            new Vec2(-250, 0),
            new Vec2(-250, -50),
            new Vec2(-300, -50),
        };
        sceneEntities.add(ScenePolygon.createScenePolygon("GroundPoly3", getNextID(), ABType.Ground, 0f, 0f, 0f, vertices3));

        sceneEntities.add(SceneRectangle.createSceneRectangle("Rect1", getNextID(), ABType.Stone, 7f, 7f, 3.5f, -303.5f, 0));
        sceneEntities.add(SceneRectangle.createSceneRectangle("Rect2", getNextID(), ABType.Stone, 7f, 7f, 96.5f, -303.5f, 0));

        sceneEntities.add(SceneCircle.createSceneCircle("Ball1", getNextID(), ABType.Stone, 25f, 25f, -325f));
        sceneEntities.add(SceneCircle.createSceneCircle("Ball2", getNextID(), ABType.Stone, 25f, 75f, -325f));
        sceneEntities.add(SceneCircle.createSceneCircle("Ball3", getNextID(), ABType.Stone, 25f, 50f, -369f));

        sceneEntities.add(SceneRectangle.createSceneRectangle("Rect3", getNextID(), ABType.Wood, 150f, 20f, 210f, -175f, 0));
        sceneEntities.add(SceneRectangle.createSceneRectangle("Rect4", getNextID(), ABType.Wood, 150f, 20f, 310f, -175f, 0));
        sceneEntities.add(SceneRectangle.createSceneRectangle("Rect5", getNextID(), ABType.Wood, 150f, 20f, 390f, -175f, 0));
        sceneEntities.add(SceneRectangle.createSceneRectangle("Rect6", getNextID(), ABType.Wood, 150f, 20f, 490f, -175f, 0));

        sceneEntities.add(SceneRectangle.createSceneRectangle("Rect7", getNextID(), ABType.Wood, 20f, 120f, 260f, -260f, 0));
        sceneEntities.add(SceneRectangle.createSceneRectangle("Rect8", getNextID(), ABType.Wood, 20f, 120f, 440f, -260f, 0));

        sceneEntities.add(SceneRectangle.createSceneRectangle("Rect9", getNextID(), ABType.Wood, 20f, 100f, 300f, -280f, 0));
        sceneEntities.add(SceneRectangle.createSceneRectangle("Rect10", getNextID(), ABType.Wood, 20f, 100f, 400f, -280f, 0));

        sceneEntities.add(SceneRectangle.createSceneRectangle("Rect11", getNextID(), ABType.Wood, 100f, 20f, 310f, -340f, 0));
        sceneEntities.add(SceneRectangle.createSceneRectangle("Rect12", getNextID(), ABType.Wood, 100f, 20f, 390f, -340f, 0));

        sceneEntities.add(SceneRectangle.createSceneRectangle("Rect13", getNextID(), ABType.Wood, 20f, 100f, 350f, -400f, 0));

        sceneEntities.add(SceneRectangle.createSceneRectangle("Rect14", getNextID(), ABType.Ice, 30f, 30f, 350f, -425f, 0));
        sceneEntities.add(SceneRectangle.createSceneRectangle("Rect15", getNextID(), ABType.Ice, 30f, 30f, 380f, -425f, 0));
        sceneEntities.add(SceneRectangle.createSceneRectangle("Rect16", getNextID(), ABType.Ice, 30f, 30f, 320f, -425f, 0));

        sceneEntities.add(SceneRectangle.createSceneRectangle("Rect17", getNextID(), ABType.Ice, 30f, 30f, 335f, -455f, 0));
        sceneEntities.add(SceneRectangle.createSceneRectangle("Rect18", getNextID(), ABType.Ice, 30f, 30f, 365f, -455f, 0));

        sceneEntities.add(SceneRectangle.createSceneRectangle("Rect19", getNextID(), ABType.Ice, 30f, 30f, 350f, -485f, 0));

        sceneEntities.add(SceneCircle.createSceneCircle("Pig1", getNextID(), ABType.Pig, 20f, 260f, -120f));
        sceneEntities.add(SceneCircle.createSceneCircle("Pig2", getNextID(), ABType.Pig, 20f, 440f, -120f));
        sceneEntities.add(SceneCircle.createSceneCircle("Pig3", getNextID(), ABType.Pig, 20f, 350f, -310f));

        Scene scene = new Scene(sceneEntities);
        return scene;
   }

    public static Scene createSupportEvaluationScene() {
        // also values in "pixel size" as it was adapted from stability scene

        List<SceneEntity> sceneEntities = new LinkedList<SceneEntity>();
        resetID();

        Vec2[] vertices1 = new Vec2[]{
            new Vec2(0, 0),
            new Vec2(200, 0),
            new Vec2(200, -100),
            new Vec2(100, -300),
            new Vec2(0, -300),
        };
        sceneEntities.add(ScenePolygon.createScenePolygon("GroundPoly1", getNextID(), ABType.Ground, 0f, 0f, 0f, vertices1));

        Vec2[] vertices2 = new Vec2[]{
            new Vec2(200, 0),
            new Vec2(600, 0),
            new Vec2(500, -100),
            new Vec2(200, -100),
        };
        sceneEntities.add(ScenePolygon.createScenePolygon("GroundPoly2", getNextID(), ABType.Ground, 0f, 0f, 0f, vertices2));

        Vec2[] vertices3 = new Vec2[]{
            new Vec2(-300, 50),
            new Vec2(900, 50),
            new Vec2(900, 0),
            new Vec2(-250, 0),
            new Vec2(-250, -50),
            new Vec2(-300, -50),
        };
        sceneEntities.add(ScenePolygon.createScenePolygon("GroundPoly3", getNextID(), ABType.Ground, 0f, 0f, 0f, vertices3));

        sceneEntities.add(SceneRectangle.createSceneRectangle("Rect1", getNextID(), ABType.Stone, 7f, 7f, 3.5f, -303.5f, 0));
        sceneEntities.add(SceneRectangle.createSceneRectangle("Rect2", getNextID(), ABType.Stone, 7f, 7f, 96.5f, -303.5f, 0));

        sceneEntities.add(SceneCircle.createSceneCircle("Ball1", getNextID(), ABType.Stone, 25f, 25f, -325f));
        sceneEntities.add(SceneCircle.createSceneCircle("Ball2", getNextID(), ABType.Stone, 25f, 75f, -325f));
        sceneEntities.add(SceneCircle.createSceneCircle("Ball3", getNextID(), ABType.Stone, 25f, 50f, -369f));

        sceneEntities.add(SceneRectangle.createSceneRectangle("Rect3", getNextID(), ABType.Wood, 150f, 20f, 210f, -175f, 0));
        sceneEntities.add(SceneRectangle.createSceneRectangle("Rect4", getNextID(), ABType.Wood, 150f, 20f, 310f, -175f, 0));

        sceneEntities.add(SceneRectangle.createSceneRectangle("Rect5", getNextID(), ABType.Wood, 20f, 120f, 260f, -260f, 0));

        sceneEntities.add(SceneRectangle.createSceneRectangle("Rect6", getNextID(), ABType.Wood, 100f, 20f, 220f, -320f, 0));
        sceneEntities.add(SceneRectangle.createSceneRectangle("Rect7", getNextID(), ABType.Wood, 100f, 20f, 300f, -320f, 0));
        sceneEntities.add(SceneRectangle.createSceneRectangle("Rect8", getNextID(), ABType.Wood, 20f, 100f, 260f, -380f, 0));

        sceneEntities.add(SceneRectangle.createSceneRectangle("Rect9", getNextID(), ABType.Ice, 30f, 30f, 275f, -405f, 0));
        sceneEntities.add(SceneRectangle.createSceneRectangle("Rect10", getNextID(), ABType.Ice, 30f, 30f, 245f, -405f, 0));
        sceneEntities.add(SceneRectangle.createSceneRectangle("Rect11", getNextID(), ABType.Ice, 30f, 30f, 260f, -435f, 0));

        sceneEntities.add(SceneCircle.createSceneCircle("Pig1", getNextID(), ABType.Pig, 20f, 260f, -120f));
        sceneEntities.add(SceneCircle.createSceneCircle("Pig2", getNextID(), ABType.Pig, 20f, 260f, -290f));

        Scene scene = new Scene(sceneEntities);
        return scene;
    }

    public static Scene createSceneUnderstandingTestScene() {
        List<SceneEntity> sceneEntities = new LinkedList<SceneEntity>();
        resetID();

        sceneEntities.add(SceneRectangle.createSceneRectangle("Ground1", getNextID(), ABType.Ground, 2f, 250f, 25f, -1f, 0));
        sceneEntities.add(SceneRectangle.createSceneRectangle("Ground1", getNextID(), ABType.Ground, 2f, 250f, 25f, 78.5f, 0));
        sceneEntities.add(SceneRectangle.createSceneRectangle("Ground2", getNextID(), ABType.Ground, 150f, 2f, -1f, 2.5f, 0));
        sceneEntities.add(SceneRectangle.createSceneRectangle("Ground3", getNextID(), ABType.Ground, 150, 2f, 101f, 2.5f, 0));

        sceneEntities.add(SceneRectangle.createSceneRectangle("Rect1", getNextID(), ABType.Wood, 4f, 1f, 35f, 2f, 0));
        sceneEntities.add(SceneRectangle.createSceneRectangle("Rect2", getNextID(), ABType.Wood, 4f, 1f, 40f, 2f, 0));
        sceneEntities.add(SceneRectangle.createSceneRectangle("Rect3", getNextID(), ABType.Wood, 1f, 6f, 37.5f, 4.5f, 0));

        sceneEntities.add(SceneCircle.createSceneCircle("Pig1", getNextID(), ABType.Pig, 1, 37.5f, 1));

        sceneEntities.add(SceneRectangle.createSceneRectangle("Rect4", getNextID(), ABType.Wood, 4f, 1f, 36f, 7f, 0));
        sceneEntities.add(SceneRectangle.createSceneRectangle("Rect5", getNextID(), ABType.Wood, 4f, 1f, 39f, 7f, 0));
        sceneEntities.add(SceneRectangle.createSceneRectangle("Rect6", getNextID(), ABType.Wood, 1f, 4f, 37.5f, 9.5f, 0));

        sceneEntities.add(SceneCircle.createSceneCircle("Pig2", getNextID(), ABType.Pig, 0.5f, 37.5f, 5.5f));

        sceneEntities.add(SceneRectangle.createSceneRectangle("Rect7", getNextID(), ABType.Wood, 1f, 1f, 37f, 10.5f, 0));
        sceneEntities.add(SceneRectangle.createSceneRectangle("Rect8", getNextID(), ABType.Wood, 1f, 1f, 38f, 10.5f, 0));
        sceneEntities.add(SceneRectangle.createSceneRectangle("Rect9", getNextID(), ABType.Wood, 1f, 1f, 37.5f, 11.5f, 0));

        sceneEntities.add(SceneCircle.createSceneCircle("Bird1", getNextID(), ABType.RedBird, 0.5f, 3f, 3f));

        Scene scene = new Scene(sceneEntities);
        scene.slingShotCenterPosition = new Vec2(3, 3);
        return scene;
    }

    public static Scene createSceneUnderstandingTestScene2() {
        List<SceneEntity> sceneEntities = new LinkedList<SceneEntity>();
        resetID();

        sceneEntities.add(SceneRectangle.createSceneRectangle("Ground1", getNextID(), ABType.Ground, 2f, 250f, 25f, -1f, 0));
        sceneEntities.add(SceneRectangle.createSceneRectangle("Ground1", getNextID(), ABType.Ground, 2f, 250f, 25f, 78.5f, 0));
        sceneEntities.add(SceneRectangle.createSceneRectangle("Ground2", getNextID(), ABType.Ground, 150f, 2f, -1f, 2.5f, 0));
        sceneEntities.add(SceneRectangle.createSceneRectangle("Ground3", getNextID(), ABType.Ground, 150, 2f, 101f, 2.5f, 0));

        sceneEntities.add(SceneRectangle.createSceneRectangle("Rect1", getNextID(), ABType.Wood, 4f, 1f, 35f, 2f, 0));
        sceneEntities.add(SceneRectangle.createSceneRectangle("Rect2", getNextID(), ABType.Wood, 4f, 1f, 40f, 2f, 0));
        sceneEntities.add(SceneRectangle.createSceneRectangle("Rect3", getNextID(), ABType.Wood, 1f, 6f, 37.5f, 4.5f, 0));

        sceneEntities.add(SceneCircle.createSceneCircle("Pig1", getNextID(), ABType.Pig, 1, 37.5f, 1));

        sceneEntities.add(SceneRectangle.createSceneRectangle("Rect4", getNextID(), ABType.Wood, 4f, 1f, 36f, 7f, 0));
        sceneEntities.add(SceneRectangle.createSceneRectangle("Rect5", getNextID(), ABType.Wood, 4f, 1f, 39f, 7f, 0));
        sceneEntities.add(SceneRectangle.createSceneRectangle("Rect6", getNextID(), ABType.Wood, 1f, 4f, 37.5f, 9.5f, 0));

        sceneEntities.add(SceneCircle.createSceneCircle("Pig2", getNextID(), ABType.Pig, 0.5f, 37.5f, 5.5f));

        sceneEntities.add(SceneRectangle.createSceneRectangle("Rect7", getNextID(), ABType.Wood, 1f, 1f, 37f, 10.5f, 0));
        sceneEntities.add(SceneRectangle.createSceneRectangle("Rect8", getNextID(), ABType.Wood, 1f, 1f, 38f, 10.5f, 0));
        sceneEntities.add(SceneRectangle.createSceneRectangle("Rect9", getNextID(), ABType.Wood, 1f, 1f, 37.5f, 11.5f, 0));




        float offsetX = -10;
        sceneEntities.add(SceneRectangle.createSceneRectangle("Rect10", getNextID(), ABType.Wood, 4f, 1f, 35f+offsetX, 2f, 0));
        sceneEntities.add(SceneRectangle.createSceneRectangle("Rect11", getNextID(), ABType.Wood, 4f, 1f, 40f+offsetX, 2f, 0));
        sceneEntities.add(SceneRectangle.createSceneRectangle("Rect12", getNextID(), ABType.Wood, 1f, 6f, 37.5f+offsetX, 4.5f, 0));

        sceneEntities.add(SceneCircle.createSceneCircle("Pig3", getNextID(), ABType.Pig, 1, 37.5f+offsetX, 1));

        sceneEntities.add(SceneRectangle.createSceneRectangle("Rect13", getNextID(), ABType.Wood, 4f, 1f, 36f+offsetX, 7f, 0));
        sceneEntities.add(SceneRectangle.createSceneRectangle("Rect14", getNextID(), ABType.Wood, 4f, 1f, 39f+offsetX, 7f, 0));
        sceneEntities.add(SceneRectangle.createSceneRectangle("Rect15", getNextID(), ABType.Wood, 1f, 4f, 37.5f+offsetX, 9.5f, 0));

        sceneEntities.add(SceneCircle.createSceneCircle("Pig4", getNextID(), ABType.Pig, 0.5f, 37.5f+offsetX, 5.5f));

        sceneEntities.add(SceneRectangle.createSceneRectangle("Rect16", getNextID(), ABType.Wood, 1f, 1f, 37f+offsetX, 10.5f, 0));
        sceneEntities.add(SceneRectangle.createSceneRectangle("Rect17", getNextID(), ABType.Wood, 1f, 1f, 38f+offsetX, 10.5f, 0));
        sceneEntities.add(SceneRectangle.createSceneRectangle("Rect18", getNextID(), ABType.Wood, 1f, 1f, 37.5f+offsetX, 11.5f, 0));




        sceneEntities.add(SceneRectangle.createSceneRectangle("Rect19", getNextID(), ABType.Wood, 1f, 11f, 32.5f, 12.5f, 0));
        sceneEntities.add(SceneRectangle.createSceneRectangle("Rect20", getNextID(), ABType.Wood, 6, 1, 32.5f, 16f, 0));
        sceneEntities.add(SceneRectangle.createSceneRectangle("Rect21", getNextID(), ABType.Wood, 6, 1, 28.5f, 16f, 0));
        sceneEntities.add(SceneRectangle.createSceneRectangle("Rect22", getNextID(), ABType.Wood, 6, 1, 36.5f, 16f, 0));

        sceneEntities.add(SceneRectangle.createSceneRectangle("Rect23", getNextID(), ABType.Wood, 2, 1, 32.5f, 20f, 0));
        sceneEntities.add(SceneRectangle.createSceneRectangle("Rect24", getNextID(), ABType.Wood, 2, 1, 28.5f, 20f, 0));
        sceneEntities.add(SceneRectangle.createSceneRectangle("Rect25", getNextID(), ABType.Wood, 2, 1, 36.5f, 20f, 0));

        sceneEntities.add(SceneCircle.createSceneCircle("Pig5", getNextID(), ABType.Pig, 1f,  32.5f, 22f));
        sceneEntities.add(SceneCircle.createSceneCircle("Pig6", getNextID(), ABType.Pig, 0.5f,  28.5f, 21.5f));
        sceneEntities.add(SceneCircle.createSceneCircle("Pig7", getNextID(), ABType.Pig, 0.5f,  36.5f, 21.5f));







        sceneEntities.add(SceneCircle.createSceneCircle("Bird1", getNextID(), ABType.RedBird, 0.5f, 3f, 3f));

        Scene scene = new Scene(sceneEntities);
        scene.slingShotCenterPosition = new Vec2(8, 5);
        return scene;
    }


    public static Scene createRandomSceneSmall(){
        return createRandomScene(50,50,3,8,15,30,5,10);
    }
    public static Scene createRandomSceneLarge(){
        return createRandomScene(100f,100f,15,30,25,80,10,25);
    }

    public static Scene createRandomScene(float width,float height, int minRandomStaticBlocks, int maxRandomStaticBlocks, int minRandomWoodObjects, int maxRandomWoodObjects, int minRandomPigs, int maxRandomPigs) {
        List<SceneEntity> sceneEntities = new LinkedList<SceneEntity>();
        resetID();

        Random random = new Random();

        float lowerBoundY = 0f;
        float upperBoundY = height;
        float leftBoundX = 0f;
        float rightBoundX = width;



        sceneEntities.add(SceneRectangle.createSceneRectangle("GroundLEFT", getNextID(), ABType.Ground, 10f, upperBoundY+20, leftBoundX-5, upperBoundY/2, (float)Math.toRadians(90)));
        sceneEntities.add(SceneRectangle.createSceneRectangle("GroundRIGHT", getNextID(), ABType.Ground, 10f, upperBoundY+20, rightBoundX+5, upperBoundY/2, (float)Math.toRadians(90)));
        sceneEntities.add(SceneRectangle.createSceneRectangle("GroundBOTTOM", getNextID(), ABType.Ground, 10f, rightBoundX+20, rightBoundX/2, lowerBoundY-5, 0));
        sceneEntities.add(SceneRectangle.createSceneRectangle("GroundTOP", getNextID(), ABType.Ground, 10f, rightBoundX+20, rightBoundX/2, upperBoundY+5, 0));

        sceneEntities.add(SceneCircle.createSceneCircle("Bird1", getNextID(), ABType.RedBird, 0.5f, 10f, 10f));

        int amountStaticBlocks = minRandomStaticBlocks + random.nextInt(maxRandomStaticBlocks - minRandomStaticBlocks);
        for (int i = 0; i < amountStaticBlocks; i++) {
            float positionX =  leftBoundX + 10 +  random.nextFloat() *(rightBoundX -10 -(leftBoundX+10));
            float positionY =  lowerBoundY + 10 + random.nextFloat() * (upperBoundY -10 -(lowerBoundY+10));
            sceneEntities.add(SceneRectangle.createSceneRectangle("HILL"+i, getNextID(), ABType.Ground, 1+random.nextInt(4), 1+random.nextInt(25), positionX, positionY, (float)Math.toRadians(random.nextBoolean()?random.nextBoolean()?0:30:random.nextBoolean()?60:90)));
        }

        int amountWoodObjects = minRandomWoodObjects + random.nextInt(maxRandomWoodObjects - minRandomWoodObjects);
		for (int i = 0; i < amountWoodObjects; i++) {
            float positionX =  leftBoundX + 10 +  random.nextFloat() *(rightBoundX -10 -(leftBoundX+10));
            float positionY =  lowerBoundY + 10 + random.nextFloat() * (upperBoundY -10 -(lowerBoundY+10));
			sceneEntities.add(SceneRectangle.createSceneRectangle("Rect"+i, getNextID(), ABType.Wood, 1+random.nextInt(4), 1+random.nextInt(4), positionX, positionY, random.nextBoolean()?0:90));
		}

        int amountPigs = minRandomPigs + random.nextInt(maxRandomPigs - minRandomPigs);
		for (int i = 0; i < amountPigs; i++) {
			float positionX =  leftBoundX + 10 +  random.nextFloat() *(rightBoundX -10 -(leftBoundX+10));
			float positionY =  lowerBoundY + 10 + random.nextInt(2) * (upperBoundY -10 -(lowerBoundY+10));
			float radius = 0.5f + random.nextFloat();
			sceneEntities.add(SceneCircle.createSceneCircle("Pig"+i, getNextID(), ABType.Pig,radius , positionX, positionY));

		}
        Scene scene = new Scene(sceneEntities);
        return scene;
    }
}