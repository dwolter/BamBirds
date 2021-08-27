package de.uniba.sme.bambirds.planner.physicssimulation.debugging;

import org.apache.log4j.LogManager;
import org.apache.log4j.Logger;
import org.jbox2d.callbacks.DebugDraw;
import org.jbox2d.collision.shapes.CircleShape;
import org.jbox2d.common.Color3f;
import org.jbox2d.common.Vec2;
import org.jbox2d.dynamics.Body;
import org.jbox2d.dynamics.BodyDef;
import org.jbox2d.dynamics.BodyType;
import org.jbox2d.dynamics.FixtureDef;
import org.jbox2d.dynamics.World;
import org.jbox2d.testbed.framework.TestbedSettings;
import org.jbox2d.testbed.framework.TestbedTest;

import de.uniba.sme.bambirds.common.objects.ab.ABType;
import de.uniba.sme.bambirds.planner.GlobalSimulationSettings;
import de.uniba.sme.bambirds.planner.physicssimulation.JWorld;
import de.uniba.sme.bambirds.planner.physicssimulation.physics.SimulationSettings;
import de.uniba.sme.bambirds.planner.physicssimulation.scene.Scene;
import de.uniba.sme.bambirds.planner.physicssimulation.scene.entities.SceneCircle;
import de.uniba.sme.bambirds.planner.physicssimulation.scene.entities.SceneEntityBase;
import de.uniba.sme.bambirds.planner.physicssimulation.scene.entities.ScenePolygon;



/** Our Main TestbedTest-Subclass - it is what will be loaded by the VisualSimulationDebugger for each Scene that is being loaded into it.*/
public class JBox2DDebugTestbedWorld extends TestbedTest{
    private static final Logger log = LogManager.getLogger(JBox2DDebugTestbedWorld.class);
    
    private Scene scene;
    private SimulationSettings simulationSettings;
    private String name;

    private boolean drawCoordinateSystem = false;
    private boolean drawSlingShotCross = true;
    
    /** the bomb will be spawned when you hold shift + left mouse button in the VisualSimulationDebugger */
    private Body mybomb = null;


    /**
     * 
     * @param scene
     */
    public JBox2DDebugTestbedWorld(Scene scene) {
        this.scene = scene; 
    }
    
    /**
     * Create a new Testbed for the VisualSimulationDebugger
     * @param name Will be displayed in Dropdown from the VisualSimulationDebugger
     * @param scene provide the Scene which the Debugger will simulate
     * @param simulationSettings provide Settings for the Physicsengine on how to execute the simulation
     */
    public JBox2DDebugTestbedWorld(String name, Scene scene, SimulationSettings simulationSettings) {
        this.name = name;
        this.scene = scene; 
        this.simulationSettings = simulationSettings;
    }




    @Override
    public void initTest(boolean deserialized) {
        
        log.debug("INIT TEST");
        World world = getWorld();
        // world = new JWorld(gravity)
        // before manupalting the sceneentities we make a copy of the scene
        this.scene = new Scene(scene); 
        scene.populateWorld(getWorld());
        // scene.logAllEntities();
    
        if (simulationSettings != null){
            world.setGravity(simulationSettings.getGravity());
        }
        
        SetupCameraPositionAndZoom();
       
        
        // IMPORTANT - Currentl in the VisualDebugger we always activate Damage and Destruction right here. You can change that if you want (add your own logic or comment/uncomment it here).
        // This setting only applies for the Visual Debugger. 
        // In order to already activate it for/during the simulation (before visual debuggging) you have to apply the ActivateContactDamageModiefier to your simulation. 
        // world.setContactListener(new AngryBirdsContactListener());
    }

    /** we overwrite this method to manually implement drawing of the id-names onto the objects, a coordinatesystem and the slingshot-center-position */
    @Override
    public synchronized void step(TestbedSettings settings) {
        // TODO > Although the copied values are  not displayed correctly in the debugGUI-Panel, the settings will be applied and have effect anyways
        settings.getSetting("Hz").value = Math.round(1f/ simulationSettings.getDeltaTime());
        settings.getSetting("Pos Iters").value = simulationSettings.getPositionIterations();
        settings.getSetting("Vel Iters").value = simulationSettings.getVelocityIterations();
        // log.debug("PosIters= " + settings.getSetting("Pos Iters").value + "VelIters= " + settings.getSetting("Vel Iters").value +"Hz= " + settings.getSetting("Hz").value );
        super.step(settings);
        DebugDraw debugdraw =  getDebugDraw();
        final Vec2 textVec = new Vec2();
        
        Body body = getWorld().getBodyList();
        while (body != null) {

            if (body.getUserData() != null) {
                Vec2 position = body.getPosition();
                String id = ((SceneEntityBase) body.getUserData()).getGlobalID();
                
                debugdraw.getViewportTranform().getWorldToScreen(position, textVec);
                debugdraw.drawString(textVec.x, textVec.y, id , new Color3f(0.0f, 1.0f ,0.0f));
            }
            body= body.getNext();
        }

        if(drawCoordinateSystem){
            DrawCoordinateSystem();
        }
        if(drawSlingShotCross){
            DrawSlingshotCross();
        }
        if(GlobalSimulationSettings.VISUAL_DEBUGGER_DEMOLITION_AND_DESTRUCTION_ENABLED){
            JWorld.destroyDamagedBodies(getWorld());
        }
    }
    
    void DrawCoordinateSystem(){
        // Settings for the Coordinate System
        float length = 1000;
        float segmentHeight = 1.5f;
        float segmentInterval = 10;
        Color3f lineColor = new Color3f(0.6f, 0.6f, 0f);
        //----------------------------------
        
        DebugDraw debugdraw =  getDebugDraw();
        final Vec2 textVec = new Vec2();
        
        debugdraw.drawSegment(new Vec2(0,0), new Vec2(length,0),lineColor);
    
        for (float x = segmentInterval; x <= length; x+= segmentInterval) {
            debugdraw.drawSegment(new Vec2(x,0), new Vec2(x,-segmentHeight), lineColor );
            debugdraw.getViewportTranform().getWorldToScreen(new Vec2(x ,-.5f), textVec);
            debugdraw.drawString(textVec.x, textVec.y, Float.toString (x) , lineColor);
        }
        debugdraw.drawSegment(new Vec2(0,-0.5f * length), new Vec2(0,0.5f*length),lineColor);
        
        for (float y = -0.5f* length; y <= 0.5f* length; y+= segmentInterval) {
            debugdraw.drawSegment(new Vec2(0,y), new Vec2(-segmentHeight, y), lineColor );
            debugdraw.getViewportTranform().getWorldToScreen(new Vec2(-.5f, y), textVec);
            debugdraw.drawString(textVec.x, textVec.y, Float.toString (y) , lineColor);
        }
    }
    
    void DrawSlingshotCross(){
        // draw slingshot shooting point
        Color3f crossColor = new Color3f(1f, 0.2f, 0f);
        
        DebugDraw debugdraw =  getDebugDraw();
        debugdraw.drawSegment(new Vec2(scene.slingShotCenterPosition.x -3f,scene.slingShotCenterPosition.y), new Vec2(scene.slingShotCenterPosition.x +3f,scene.slingShotCenterPosition.y),crossColor);
        debugdraw.drawSegment(new Vec2(scene.slingShotCenterPosition.x ,scene.slingShotCenterPosition.y -3f), new Vec2(scene.slingShotCenterPosition.x,scene.slingShotCenterPosition.y + 3f),crossColor);
    }

    void SetupCameraPositionAndZoom(){
        {
            // set the camera to reasonable settings
            Body nextbody = getWorld().getBodyList();
            // Body nextbody = world.getBodyList();
            Vec2 minXY = null; 
            Vec2 maxXY = null;
            while(nextbody != null ){
                SceneEntityBase sceneEntity = (SceneEntityBase) nextbody.getUserData();
                if(sceneEntity == null || (sceneEntity.getAbType() == ABType.Ground) || sceneEntity instanceof ScenePolygon){
                    nextbody = nextbody.getNext();
                    continue;
                }
                else if (nextbody.getPosition().equals(new Vec2(0,0))){
                    nextbody = nextbody.getNext();
                    continue;
                }
                if (minXY == null || maxXY == null){
                    minXY = new Vec2(nextbody.getPosition());
                    maxXY = new Vec2(nextbody.getPosition());
                }else{

                    minXY.x =  Math.min(minXY.x, nextbody.getPosition().x);
                    minXY.y =  Math.min(minXY.y, nextbody.getPosition().y);
                    maxXY.x =  Math.max(maxXY.x, nextbody.getPosition().x);
                    maxXY.y =  Math.max(maxXY.y, nextbody.getPosition().y);
                }
            
                nextbody = nextbody.getNext();
            }

            Vec2 worldStartSize = new Vec2(maxXY.x - minXY.x, maxXY.y - minXY.y);
            Vec2 worldCenter = new Vec2((minXY.x + 0.5f*(maxXY.x - minXY.x)), (minXY.y + 0.5f*(maxXY.y - minXY.y)));
            
            float cameraZoom = 40;
            if(worldStartSize.x > 0){
                cameraZoom = 1250f / worldStartSize.x;
            }
            this.setCamera(worldCenter, cameraZoom);     
        }
    }


    @Override
    public String getTestName() {
        if (this.name == null){
            return "My Default Testbed";
        }else{
            return name;
        }
    }

	public boolean isDrawCoordinateSystem() {
		return drawCoordinateSystem;
	}

	public void setDrawCoordinateSystem(boolean drawCoordinateSystem) {
		this.drawCoordinateSystem = drawCoordinateSystem;
	}

	public boolean isDrawSlingShotCross() {
		return drawSlingShotCross;
	}

	public void setDrawSlingShotCross(boolean drawSlingShotCross) {
		this.drawSlingShotCross = drawSlingShotCross;
	}

    


    //----- In order to make the spawning of birds with the mouse possible, we overwrite some methods to to adapt an already existing functionality from jbox2d -------------
	
    protected void  resetMyBomb(){
        if (mybomb != null) {
            m_world.destroyBody(mybomb);
            mybomb = null;
        }
    }


	@Override
	public synchronized void spawnBomb(Vec2 worldPt) {
        super.spawnBomb(worldPt);
        log.debug("spawnBomb");
        
    }
    

	@Override
	public synchronized void launchBomb(Vec2 position, Vec2 velocity) {
        // We  reimplement the bomb spawning method so it better mimics the shooting of a bird in the angry birds game
        // super.launchBomb(position, velocity);

        log.debug("launchBomb");

        //TODO > here we reset the position with the Slingshotcenterposition > I would like to make this optional / toggable. But currently its tricky to change the source.
        // In order to change source I think we would need to provide jbox2d directly in the project.
        position = scene.slingShotCenterPosition;

        velocity = new Vec2(velocity.x *0.5f, velocity.y *0.5f);
        float maxVelocity = 22f; // found out at https://www.wired.com/2011/05/is-the-launch-speed-in-angry-birds-constant/
        if(velocity.length() > maxVelocity){
            velocity = new Vec2( maxVelocity * velocity.x / velocity.length(), maxVelocity * velocity.y/ velocity.length());
        }
        resetMyBomb();

        
        BodyDef bd = new BodyDef();
        bd.type = BodyType.DYNAMIC;
        bd.position.set(position);
        bd.bullet = true;
        mybomb = m_world.createBody(bd);
        mybomb.setLinearVelocity(velocity);
        mybomb.setAngularDamping(1f); // birds dont roll forever > rather they stop quite soon.


        CircleShape circle = new CircleShape();
        // circle.m_radius = 0.50f; // pigs are ~ 0.61635
        circle.m_radius = 0.425f; // pigs are ~ 0.61635, computervision returns a value of 0.425 for redbirds
    
        FixtureDef fd = new FixtureDef();
        fd.shape = circle;
        // fd.density = 1f; // control the debug-birds weight here
        fd.density = 3f; // control the debug-birds weight here
        fd.restitution = 0.40f; // birds need to bounce a little bit
        fd.friction = 0.25f;
    
      
        mybomb.createFixture(fd);

        SceneCircle sceneCircle = SceneCircle.createSceneCircle("MShooter", 1000, ABType.RedBird, 0.425f, bd.position.x, bd.position.y);
        mybomb.setUserData(sceneCircle);

        /* It would have been convinient to just apply a birds entity-properties here (e.g. redbird-entityproperties) instead of the setup above, but for some surprising reason the following does not work > it will get executed but has no effect on the body ( need to investigate why )
        // RedBirdProerties redBirdProerties = new RedBirdProerties();
        // redBirdProerties.applyValuesToBody(mybomb);
        */
    }
    @Override
	public void lanchBomb() {
        // this drops a bomp from above ( like an airstrike )
        super.lanchBomb();
        log.debug("lanchBomb");
        
    }
    @Override
	public synchronized void completeBombSpawn(Vec2 p) {
        super.completeBombSpawn(p);
        log.debug("completeBombSpawn");
    }

	@Override
	public void reset() {
        resetMyBomb();
		super.reset();
	}
    

    @Override
	public void exit() {
        resetMyBomb();
		super.exit();
	}

	@Override
	public void shiftMouseDown(Vec2 p) {
        // we need to override the position of the mousecursor to the position of the slingshotcrosscenter, so that the shot will be executed from that point
        if(drawSlingShotCross){
            p = scene.slingShotCenterPosition;
        }
		super.shiftMouseDown(p);
	}


}