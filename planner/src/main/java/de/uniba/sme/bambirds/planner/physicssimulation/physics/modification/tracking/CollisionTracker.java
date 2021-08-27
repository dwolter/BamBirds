package de.uniba.sme.bambirds.planner.physicssimulation.physics.modification.tracking;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;

import de.uniba.sme.bambirds.planner.physicssimulation.physics.Simulation;
import org.jbox2d.collision.WorldManifold;
import org.jbox2d.common.Vec2;
import org.jbox2d.dynamics.Body;
import org.jbox2d.dynamics.BodyType;
import org.jbox2d.dynamics.contacts.Contact;

import de.uniba.sme.bambirds.planner.physicssimulation.JWorld;
import de.uniba.sme.bambirds.planner.physicssimulation.exceptions.ModificationException;
import de.uniba.sme.bambirds.planner.physicssimulation.physics.SimulationSettings;
import de.uniba.sme.bambirds.planner.physicssimulation.physics.modification.IWorldModifier;
import de.uniba.sme.bambirds.planner.physicssimulation.scene.entities.SceneEntityBase;
import org.apache.log4j.*;


public class CollisionTracker implements IWorldModifier {
    private static final Logger log = LogManager.getLogger(CollisionTracker.class);

    
    HashSet<String> targets = new HashSet<>();
    ArrayList<HashMap<String, HashMap<String, CollisionInfo>>> trackedCollisions = new  ArrayList<HashMap<String, HashMap<String, CollisionInfo>>> (); // frameNumber, entity1Name, entity2Name, CollisionInfo
    int frameCounter = 0;

    public CollisionTracker(List<String> targetObjectsGlobalIDs) {
        targets = new HashSet<String>();
        for (String globalID : targetObjectsGlobalIDs) {
            targets.add(globalID);
        }
    }
    public CollisionTracker(String targetObjectsGlobalID) {
        targets = new HashSet<String>();
        targets.add(targetObjectsGlobalID);
    }

	@Override
	public void apply(JWorld world, Simulation simulation, SimulationSettings settings) throws ModificationException {
        trackedCollisions.add(new HashMap<>());

        Contact contact =  world.getContactList();
        while(contact!= null){
            if(contact.isTouching()){
                
                Body body1 = contact.getFixtureA().getBody();
                Body body2 = contact.getFixtureB().getBody();
                if(body1.getType() == BodyType.DYNAMIC && body2.getType() == BodyType.DYNAMIC){

                    for (int i = 0; i < 2; i++) {
                        
                        SceneEntityBase entity1 = (SceneEntityBase) body1.getUserData();
                        SceneEntityBase entity2 = (SceneEntityBase) body2.getUserData();
                        if(targets.contains(entity1.getGlobalID()) || targets.contains(entity2.getGlobalID())) {
                            // log.debug("coll " + frameCounter + " : " +entity1.getGlobalID() + " " + body1.getPosition() + " > " + entity2.getGlobalID() + " " + body2.getPosition());
                            
                            //Thoughts/Logic derived from http://www.iforce2d.net/b2dtut/collision-anatomy
                            WorldManifold worldManifold = new WorldManifold();
                            contact.getWorldManifold(worldManifold);
                            Vec2 vel1 = body1.getLinearVelocityFromWorldPoint( worldManifold.points[0] );
                            Vec2 vel2 = body2.getLinearVelocityFromWorldPoint( worldManifold.points[0] );
                            Vec2 impactVelocity = vel1.sub(vel2);
                            CollisionInfo collisionInfo = new CollisionInfo(worldManifold.points[0], impactVelocity,entity2.getGlobalID(), entity1.getGlobalID(), frameCounter);
                            trackedCollisions.get(frameCounter).put(entity1.getGlobalID(), new HashMap<>());
                            trackedCollisions.get(frameCounter).get(entity1.getGlobalID()).put(entity2.getGlobalID(), collisionInfo);
                        }
                        
                        // now switch the bodies and repeat once in the other direction
                        Body temp = body2;
                        body2 = body1;
                        body1 = temp;
                    }
                }
            }   
            contact = contact.getNext();
        }
        frameCounter++;
    }
  





	@Override
	public String getName() {
		return "CollisionTracker";
	}
	public ArrayList<CollisionInfo> getTrackedCollisions() {

        // now we want to filter the collisions so that only first-impacts between objects are tracked. (If Object A is laying for multiple frames on Object B, then it is not counted as multiple collisions but only as 1 collision )
        ArrayList<CollisionInfo> collisions = new ArrayList<>();
        // log.debug("trackedCollisions.size()=" + trackedCollisions.size());
        for (int frame = trackedCollisions.size() -1; frame >= 1; frame--) {
            
            for (String target : targets) {
                
                if(trackedCollisions.get(frame).get(target) != null){
                    // log.debug("trackedCollisions.get(frame).get(target) != null" );
                    
                    for (String key2 : trackedCollisions.get(frame).get(target).keySet()) {
                        if(trackedCollisions.get(frame-1).get(target)!= null &&   trackedCollisions.get(frame-1).get(target).get(key2)!= null){
                            trackedCollisions.get(frame).get(target).remove(key2);
                            // log.debug("REMOVING" );
                        }else{
                            collisions.add( trackedCollisions.get(frame).get(target).get(key2));
                            // log.debug("Adding" );
                        }
                    }
                }else{
                    // for (String name : trackedCollisions.get(frame).keySet()) {
                        // log.debug("trackedCollisions.get(frame).get(target) == NULL " + name    );
                        
                    // }

                }
            }
        }

		return collisions;
	}

	public ArrayList<CollisionInfo> getCurrentFrameCollisions() {
        ArrayList<CollisionInfo> collisions = new ArrayList<>();
        for (String key1 : trackedCollisions.get(frameCounter-1).keySet()) {
            for(String key2 :  trackedCollisions.get(frameCounter-1).get(key1).keySet()){
                collisions.add(trackedCollisions.get(frameCounter-1).get(key1).get(key2));
            }
        }
        return collisions;
    }


}
