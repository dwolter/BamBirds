package de.uniba.sme.bambirds.planner.physicssimulation.physics.modification.shooting;

import de.uniba.sme.bambirds.planner.physicssimulation.physics.Simulation;
import de.uniba.sme.bambirds.planner.physicssimulation.physics.modification.IWorldModifier;

import de.uniba.sme.bambirds.planner.physicssimulation.scene.entities.properties.EntityProperties;
import org.jbox2d.collision.shapes.CircleShape;
import org.jbox2d.common.Vec2;
import org.jbox2d.dynamics.Body;
import org.jbox2d.dynamics.BodyDef;
import org.jbox2d.dynamics.BodyType;
import org.jbox2d.dynamics.FixtureDef;
import de.uniba.sme.bambirds.planner.physicssimulation.JWorld;

import de.uniba.sme.bambirds.common.objects.ab.ABType;
import de.uniba.sme.bambirds.planner.physicssimulation.physics.SimulationSettings;
import de.uniba.sme.bambirds.planner.physicssimulation.scene.entities.SceneCircle;

import org.apache.log4j.LogManager;
import org.apache.log4j.Logger;

/**
 * Adds a Bird (with velocity) at the slingshot position to simulate a shot
 */
public class Shooter implements IWorldModifier {
    private static final Logger log = LogManager.getLogger(Shooter.class);
	private ABType birdtype;
	private float angleInDegrees;
	private float strength;
	private float secondTriggerDelay;
	private float radius;
    private String birdName;



    public Shooter(ABType birdtype,String birdName, float angleInDegrees, float strength, float secondTriggerDelay) {
		this.birdtype = birdtype;
        this.birdName = birdName;
        this.angleInDegrees = angleInDegrees;
        this.angleInDegrees = Math.max(0f, angleInDegrees);
        this.angleInDegrees = Math.min(90f, angleInDegrees);

        this.strength = strength;
        this.strength = Math.min(strength, 1f);
        this.strength = Math.max(strength, 0f);

        // empirically it has been found out, that if the slingshot is pullled to its maximum, then birds have an initial velocity of 22 . independent of their type (source : // https://www.wired.com/2011/05/is-the-launch-speed-in-angry-birds-constant/)
        this.strength = strength * 22f; 

        this.secondTriggerDelay = secondTriggerDelay;


        switch(birdtype){
            case RedBird:
                this.radius = 0.425f; // pigs are ~ 0.61635, computervision returns a value of 0.425 for redbirds
                break;
            case BlackBird:
                this.radius = 0.425f; // ?? these are just guessed values so far
                break;
            case WhiteBird:
                this.radius = 0.425f; // ?? these are just guessed values so far
                break;
            case YellowBird:
                this.radius = 0.475f; // ?? these are just guessed values so far 
                break;
            case BlueBird:
                this.radius = 0.325f; // ?? these are just guessed values so far
                break;
            default:
                this.birdtype = ABType.RedBird;
                this.radius = 0.425f;
        }
        
    }

    private Body InstantiateFlyingBird(EntityProperties birdEntityProperties, JWorld world){
        BodyDef bd = new BodyDef();
        bd.type = BodyType.DYNAMIC;
        bd.position.set(world.getSlingshotcrosscenter());
        bd.bullet = true;
       
        Body body = world.createBody(bd);



        CircleShape circle = new CircleShape();
        // circle.m_radius = 0.50f; // pigs are ~ 0.61635
        circle.m_radius = this.radius; 
    
        FixtureDef fd = new FixtureDef();
        fd.shape = circle;
        body.createFixture(fd);
        birdEntityProperties.applyValuesToBody(body);;
        // fd.density = 3f; // control the debug-birds weight here
        // fd.restitution = 0.40f; // birds need to bounce a little bit
        // fd.friction = 0.25f;
    

       
      
        float angleInRadians = (this.angleInDegrees/360f) * 2f * (float) Math.PI;
        float xVel =  (float) Math.cos(angleInRadians);
        float yVel =  (float) Math.sin(angleInRadians);
        
        
        Vec2 velocity = new Vec2(xVel, yVel);
        velocity = velocity.mul(strength); 
        
        body.setLinearVelocity(velocity);
        body.setAngularDamping(1f); // birds dont roll forever > rather they stop quite soon.


        // If we want to be able to save the bird throughout further scene-snapshots, we have to create a corresponding SceneEntity
        SceneCircle sceneCircle = SceneCircle.createSceneCircle(birdName, 100, this.birdtype, radius, bd.position.x, bd.position.y);
        sceneCircle.storeStateFromPhysicsBodySnapshot(body);
        body.setUserData(sceneCircle);
        return body;
      
    }


    @Override
    public void apply(JWorld world, Simulation simulation, SimulationSettings settings)  {
        Body bullet = InstantiateFlyingBird(simulation.getScene().getEntityPropertiesLUT().getEntityProperties(birdtype),world);

    }

    @Override
    public String getName() {
        return "Shooter";
    }
    
}
