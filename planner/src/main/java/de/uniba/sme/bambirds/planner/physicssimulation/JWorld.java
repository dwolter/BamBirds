package de.uniba.sme.bambirds.planner.physicssimulation;

import org.apache.log4j.LogManager;
import org.apache.log4j.Logger;
import org.jbox2d.common.Vec2;
import org.jbox2d.dynamics.Body;
import org.jbox2d.dynamics.BodyType;
import org.jbox2d.dynamics.World;

import de.uniba.sme.bambirds.planner.physicssimulation.physics.contactListener.AngryBirdsContactListener;
import de.uniba.sme.bambirds.planner.physicssimulation.physics.contactListener.DefaultContactListener;
import de.uniba.sme.bambirds.planner.physicssimulation.scene.entities.SceneEntityBase;

/**
 * We use this direct subclass of "World" ( the jbox2d-World ) to allow us to attach some metadata to the world instance, e.g the position of the slingshotcross.
 * 
 */
public class JWorld extends World {
    private static final Logger log = LogManager.getLogger(JWorld.class);

	private Vec2 slingshotcrosscenter = new Vec2();
	private boolean isObjectDamageAndDestructionActive = false;

	public AngryBirdsContactListener demolitionActiveContactListener = new AngryBirdsContactListener();
	public DefaultContactListener demolitionDeactiveContactListener = new DefaultContactListener();


	public JWorld(Vec2 gravity) {
		super(gravity);
		// this.setContactListener(new AngryBirdsContactListener());
		SetObjectDamageAndDestructionActive(true);
    }
	
	public JWorld(Vec2 gravity, Vec2 slingshotCrossCenter){
		super(gravity);
		this.setSlingshotcrosscenter(slingshotCrossCenter);
		// this.setContactListener(new AngryBirdsContactListener());
		SetObjectDamageAndDestructionActive(true);
	}

	public void SetObjectDamageAndDestructionActive(Boolean setActive){
		if(setActive){
			this.setContactListener(new AngryBirdsContactListener());
			isObjectDamageAndDestructionActive = true;
		}else{
			this.setContactListener(new DefaultContactListener());
			isObjectDamageAndDestructionActive = false;
		}
	}
	public boolean IsObjectDamageAndDestructionActive() {
		return isObjectDamageAndDestructionActive;
	}

	public Vec2 getSlingshotcrosscenter() {
		return slingshotcrosscenter;
	}

	public void setSlingshotcrosscenter(Vec2 slingshotcrosscenter) {
		this.slingshotcrosscenter = slingshotcrosscenter;
	}

		



	// public void setObjectDemolitionActive(boolean isObjectDemolitionActive) {
	// 	this.isObjectDemolitionActive = isObjectDemolitionActive;
	// }

	@Override
	public void step(float dt, int velocityIterations, int positionIterations) {
		super.step(dt, velocityIterations, positionIterations);
		
	}

	public static void destroyDamagedBodies(World world){
		Body body = world.getBodyList();
        while (body != null) {
			if(body.m_type != BodyType.STATIC){ // we dont want static objects to be destroyed

				if (body.m_userData instanceof SceneEntityBase) {
					SceneEntityBase entity = (SceneEntityBase) body.m_userData;
					if(entity.getTotalDamage() > 0f){
						
						float damageLimit = body.getMass() * entity.getProperties().getMaximumDamagePerMass();
						
						if (entity.getTotalDamage() > damageLimit) {
							world.destroyBody(body);
							log.debug("Destroying:  " + entity.getGlobalID() + " >> Damagelimit= " + damageLimit + " | Damage= " + entity.getTotalDamage());
							return;
						}
					}
				}
			}
            body = body.getNext();
        }
	}



}
