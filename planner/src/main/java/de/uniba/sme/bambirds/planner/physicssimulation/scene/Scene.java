package de.uniba.sme.bambirds.planner.physicssimulation.scene;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;

import org.jbox2d.common.Vec2;
import org.jbox2d.dynamics.Body;
import org.jbox2d.dynamics.World;

import de.uniba.sme.bambirds.common.database.AbstractScene;
import de.uniba.sme.bambirds.common.objects.ab.ABObject;
import de.uniba.sme.bambirds.common.objects.ab.ABShape;
import de.uniba.sme.bambirds.common.objects.ab.ABType;
import de.uniba.sme.bambirds.common.objects.ab.shape.Circle;
import de.uniba.sme.bambirds.common.objects.ab.shape.Poly;
import de.uniba.sme.bambirds.common.objects.ab.shape.Rect;
import de.uniba.sme.bambirds.planner.physicssimulation.JWorld;
import de.uniba.sme.bambirds.planner.physicssimulation.scene.entities.SceneCircle;
import de.uniba.sme.bambirds.planner.physicssimulation.scene.entities.SceneEntity;
import de.uniba.sme.bambirds.planner.physicssimulation.scene.entities.SceneEntityBase;
import de.uniba.sme.bambirds.planner.physicssimulation.scene.entities.ScenePolygon;
import de.uniba.sme.bambirds.planner.physicssimulation.scene.entities.SceneRectangle;
import de.uniba.sme.bambirds.planner.physicssimulation.scene.entities.properties.EntityProperties;
import de.uniba.sme.bambirds.planner.physicssimulation.scene.entities.properties.EntityPropertiesLUT;
import org.apache.log4j.LogManager;
import org.apache.log4j.Logger;
 

/**
 * The Scene-object is a central element of the physics simulation package. It gives a static representation of the current world in form of a list of SceneEntities. Any Scene-object can be used to instantiate a jbox2d-world in order to run a physicssimulation. It servves as the missing link between the dynamically simulated world and a static representation of it which can be easily stored, transformed or evaluated.
 There are different ways to construct a Scene in order to suit different usecases.
*/
public class Scene implements Serializable{
    private static final Logger log = LogManager.getLogger(Scene.class);
    /**
	 * Change the UID when the class changes.
	 */
    private static final long serialVersionUID = 1L;

    private HashMap<Integer, SceneEntityBase> allEntitiesMap  = new HashMap<>(); // Key = ENtityId



    private EntityPropertiesLUT entityPropertiesLUT = new EntityPropertiesLUT();

    private String lastAppliedModifierName  = "";
    public Vec2 slingShotCenterPosition = new Vec2();


    public List<SceneEntityBase> getAllEntities(){
        return new ArrayList<>(allEntitiesMap.values());
    }



    public String getLastAppliedModifierName() {
		return lastAppliedModifierName;
	}

	public void setLastAppliedModifierName(String lastAppliedModifier) {
		this.lastAppliedModifierName = lastAppliedModifier;
	}

	public List<SceneRectangle> getAllSceneRectangles(){
        List<SceneRectangle> result = new ArrayList<>();
        for (SceneEntity sceneEntity : allEntitiesMap.values()) {
            if(sceneEntity instanceof SceneRectangle){
                result.add((SceneRectangle) sceneEntity);
            }
        }
        return result;
    }
    
    public List<SceneCircle> getAllSceneCircles(){
        List<SceneCircle> result = new ArrayList<>();
        for (SceneEntity sceneEntity : allEntitiesMap.values()) {
            if(sceneEntity instanceof SceneCircle){
                result.add((SceneCircle) sceneEntity);
            }
        }
        return result;
    }
    
    public List<ScenePolygon> getAllScenePolygons(){
        List<ScenePolygon> result = new ArrayList<>();
        for (SceneEntity sceneEntity : allEntitiesMap.values()) {
            if(sceneEntity instanceof ScenePolygon){
                result.add((ScenePolygon) sceneEntity);
            }
        }
        return result;
    }
    
    public List<ScenePolygon> getHills(){
        List<ScenePolygon> hillEntities = new ArrayList<>();
        for (ScenePolygon scenePolygon : getAllScenePolygons()) {
            if(scenePolygon.getAbType() == ABType.Hill){
                hillEntities.add(scenePolygon);
            }
        }
        return hillEntities;
    } 
    
    public List<SceneCircle> getPigs(){
        List<SceneCircle> pigEntities= new ArrayList<>();
        for (SceneCircle sceneCircle : getAllSceneCircles()) {
            if(sceneCircle.getAbType() == ABType.Pig){
                pigEntities.add(sceneCircle);
            }
        }
        return pigEntities;
    } 

    public List<SceneCircle> getBirds(){
        List<SceneCircle> birdEntities= new ArrayList<>();
        for (SceneCircle sceneCircle : getAllSceneCircles()) {
            switch(sceneCircle.getAbType()){
                case BlackBird:
                case BlueBird:
                case RedBird:
                case WhiteBird:
                case YellowBird:
                    birdEntities.add(sceneCircle);
                    break;
                default:
                    break;
            }
        }
        return birdEntities;
    } 

    /**
     *  We try to create a copy of the Scene entities with all state that are being simulated in the given world.
        Therefore  we take all physicalbodies, fetch their attached userdata = Sceneentites, Make copies of those entities
        and then on the new entites we store the current state( snapshot) of the physicsbody into their membervariables.
        Finally we store the newly created entites into this scenes' lists - constituting a new scene which is representing a snapshot of the physics world
        The new scene can then be further manipulated or instantiated into a simulation for example...
     * @param JBox2DWorld
     */
    public Scene(JWorld JBox2DWorld){
        this.slingShotCenterPosition = JBox2DWorld.getSlingshotcrosscenter();

        Body jBody = JBox2DWorld.getBodyList();
        while(jBody != null){
            if (jBody.getUserData() != null){
                if( jBody.getUserData() != null && jBody.getUserData() instanceof SceneEntity){
                    SceneEntity currentSceneEntity =  (SceneEntity) jBody.getUserData();
                    // TODO we need a to make sure this works correct/ the way we intend it to
                    // TODO Currently we dont handle any newly spawned entities ( broken boxes, splittered birds etc)
                    // because in this loop we only handle those entituies with userdata. 

                    if (  currentSceneEntity instanceof SceneRectangle) {

                        SceneRectangle currentSceneRectangle = (SceneRectangle) currentSceneEntity;
                        SceneRectangle newSceneRectangle = new SceneRectangle(currentSceneRectangle);
                        newSceneRectangle.storeStateFromPhysicsBodySnapshot(jBody);
                        addSceneEntity(newSceneRectangle);
                    }
                    else if (  currentSceneEntity instanceof SceneCircle ) {
                        SceneCircle currentSceneCircle = (SceneCircle) currentSceneEntity; 
                        SceneCircle newSceneCircle = new SceneCircle(currentSceneCircle); // make a Copy of the entity
                        newSceneCircle.storeStateFromPhysicsBodySnapshot(jBody); // 
                        addSceneEntity(newSceneCircle);
                    }
                    else if (  currentSceneEntity instanceof ScenePolygon ) {
                        //TODO ? WHAT TO DO HERE - ARE HILLS POLYGONS or what do we actually get from the computervision ( because polygons are hills & otherway round) ?
                        // polyBlocks.add((ScenePolygon) sceneEntity);

                        ScenePolygon currentScenePolygon = (ScenePolygon) currentSceneEntity; 
                        ScenePolygon newScenePolygon = new ScenePolygon(currentScenePolygon); // make a Copy of the entity
                        newScenePolygon.storeStateFromPhysicsBodySnapshot(jBody); // 
                        addSceneEntity(newScenePolygon);
                    }
                }
            }
            //TODO - what to do with objects from the world that did not have a Scene-Entity attached as userdata ?
            jBody =  jBody.getNext();
        }
    }



    /**
     *  We try to create a copy of the Scene entities with all state that are being simulated in the given world.
     Therefore  we take all physicalbodies, fetch their attached userdata = Sceneentites, Make copies of those entities
     and then on the new entites we store the current state( snapshot) of the physicsbody into their membervariables.
     Finally we store the newly created entites into this scenes' lists - constituting a new scene which is representing a snapshot of the physics world
     The new scene can then be further manipulated or instantiated into a simulation for example...
     * @param JBox2DWorld
     */
    public Scene(JWorld JBox2DWorld, EntityPropertiesLUT entityPropertiesLUT){
        this.slingShotCenterPosition = JBox2DWorld.getSlingshotcrosscenter();
        this.entityPropertiesLUT = entityPropertiesLUT.getCopy();

        Body jBody = JBox2DWorld.getBodyList();
        while(jBody != null){
            if (jBody.getUserData() != null){
                if( jBody.getUserData() != null && jBody.getUserData() instanceof SceneEntity){
                    SceneEntity currentSceneEntity =  (SceneEntity) jBody.getUserData();
                    // TODO we need a to make sure this works correct/ the way we intend it to
                    // TODO Currently we dont handle any newly spawned entities ( broken boxes, splittered birds etc)
                    // because in this loop we only handle those entituies with userdata.

                    if (  currentSceneEntity instanceof SceneRectangle) {

                        SceneRectangle currentSceneRectangle = (SceneRectangle) currentSceneEntity;
                        SceneRectangle newSceneRectangle = new SceneRectangle(currentSceneRectangle);
                        newSceneRectangle.storeStateFromPhysicsBodySnapshot(jBody);
                        addSceneEntity(newSceneRectangle);
                    }
                    else if (  currentSceneEntity instanceof SceneCircle ) {
                        SceneCircle currentSceneCircle = (SceneCircle) currentSceneEntity;
                        SceneCircle newSceneCircle = new SceneCircle(currentSceneCircle); // make a Copy of the entity
                        newSceneCircle.storeStateFromPhysicsBodySnapshot(jBody); //
                        addSceneEntity(newSceneCircle);
                    }
                    else if (  currentSceneEntity instanceof ScenePolygon ) {
                        //TODO ? WHAT TO DO HERE - ARE HILLS POLYGONS or what do we actually get from the computervision ( because polygons are hills & otherway round) ?
                        // polyBlocks.add((ScenePolygon) sceneEntity);

                        ScenePolygon currentScenePolygon = (ScenePolygon) currentSceneEntity;
                        ScenePolygon newScenePolygon = new ScenePolygon(currentScenePolygon); // make a Copy of the entity
                        newScenePolygon.storeStateFromPhysicsBodySnapshot(jBody); //
                        addSceneEntity(newScenePolygon);
                    }
                }
            }
            //TODO - what to do with objects from the world that did not have a Scene-Entity attached as userdata ?
            jBody =  jBody.getNext();
        }
    }
                    
    /**
     * Takes A list of Sceneentities, makes copies of it and creates a fresh scene from those. That scene can then be used to be instantiated into a physics simulation for example
     * @param sceneEntities
     */
    public Scene(List<SceneEntity> sceneEntities){

        // TODO we need a to make sure this works correct/ the way we intend it to
        for (SceneEntity currentSceneEntity : sceneEntities) {
            
            // TODO we need a to make sure this works correct/ the way we intend it to
            // TODO Currently we dont handle any newly spawned entities ( broken boxes, splittered birds etc)
            // because in this loop we only handle those entituies with userdata. 

            if (  currentSceneEntity instanceof SceneRectangle) {

                SceneRectangle currentSceneRectangle = (SceneRectangle) currentSceneEntity;
                SceneRectangle newSceneRectangle = new SceneRectangle(currentSceneRectangle);
                addSceneEntity(newSceneRectangle);
            }
            else if (  currentSceneEntity instanceof SceneCircle ) {
                SceneCircle currentSceneCircle = (SceneCircle) currentSceneEntity; 
                SceneCircle newSceneCircle = new SceneCircle(currentSceneCircle); // make a Copy of the entity
                addSceneEntity(newSceneCircle);

            }
            else if (  currentSceneEntity instanceof ScenePolygon ) {
                //TODO ? WHAT TO DO HERE - ARE HILLS POLYGONS or what do we actually get from the computervision ( because polygons are hills & otherway round) ?

                ScenePolygon currentScenePolygon = (ScenePolygon) currentSceneEntity; 
                ScenePolygon newScenePolygon = new ScenePolygon(currentScenePolygon); // make a Copy of the entity
                addSceneEntity(newScenePolygon);
            }
        }
    }



    /** Create a Scene by providing an Abstract-Scene-object which is created by the agent's vision module at run-time.*/
    public Scene(AbstractScene abstractScene) {
        for (ABObject abObject : abstractScene.getBlocks()) {
            // we want to filter out the stupid ice block in the top right of the screencorner
            if( !(abObject.getType() == ABType.Ice && abObject.getCenterX() > 600 && abObject.getCenterY() < 100)){
                addABObject(abObject);   
            }
        }
        addABObjects(abstractScene.getPigs());
        addABObjects(abstractScene.getHills());
        addABObjects(abstractScene.getTnts());
        for (ABObject abObject : abstractScene.getBirds()) {
            // we are only interested in the first birds starting position ( he is already sitting in the slingshot )
            addABObject(abObject);
            break;
        }
    }
    
    
    /**Create a Scene based on an existing  SerializableScene-object. */
    public Scene(SerializableScene serializableScene) {

        //add Blocks
        for (ABObject abObject : serializableScene.getBlocks()) {
            // we want to filter out the stupid ice block in the top right of the screencorner
            if( !(abObject.getType() == ABType.Ice && abObject.getCenterX() > 600 && abObject.getCenterY() < 100)){
                addABObject(abObject);   
            }
        }
        addABObjects(serializableScene.getPigs());
        addABObjects(serializableScene.getHills());
        addABObjects(serializableScene.getTnts());
        for (ABObject abObject : serializableScene.getBirds()) {
            // we are only interested in the first birds starting position ( he is already sitting in the slingshot )
            addABObject(abObject);
            break;
        }
        this.entityPropertiesLUT = serializableScene.getEntityPropertiesLUT().getCopy();
    }
   
   /**
    * Create a copy of an existing Scene
    * @param sourceScene
    */
    public Scene(Scene sourceScene){
     // TODO we need a to make sure this works correct/ the way we intend it to
        for (SceneEntity currentSceneEntity : sourceScene.getAllEntities()) {
                

            if (  currentSceneEntity instanceof SceneRectangle) {

                SceneRectangle currentSceneRectangle = (SceneRectangle) currentSceneEntity;
                SceneRectangle newSceneRectangle = new SceneRectangle(currentSceneRectangle);
                addSceneEntity(newSceneRectangle);
            }
            else if (  currentSceneEntity instanceof SceneCircle ) {
                SceneCircle currentSceneCircle = (SceneCircle) currentSceneEntity; 
                SceneCircle newSceneCircle = new SceneCircle(currentSceneCircle); // make a Copy of the entity
                addSceneEntity(newSceneCircle);

            }
            else if (  currentSceneEntity instanceof ScenePolygon ) {

                ScenePolygon currentScenePolygon = (ScenePolygon) currentSceneEntity; 
                ScenePolygon newScenePolygon = new ScenePolygon(currentScenePolygon); // make a Copy of the entity
                addSceneEntity(newScenePolygon);
            }

            this.lastAppliedModifierName = sourceScene.lastAppliedModifierName;
            this.slingShotCenterPosition = sourceScene.slingShotCenterPosition;
        }
        this.entityPropertiesLUT = sourceScene.entityPropertiesLUT.getCopy();
    }


    /**
     * Take a single SceneEntityBase-Object and put it into the scene.
     * This is the only method that should be used for populating a Scenedescription out of SceneEntity-Objects
     */
    public void addSceneEntity(SceneEntityBase sceneEntity){
        allEntitiesMap.put(sceneEntity.getId(), sceneEntity);
    }

    public void addABObjects(List<ABObject> abObjects){
        for (ABObject abObject : abObjects) {
            addABObject(abObject);
        }
    }

    /**
     * Take a single ABObject and put it into the scene.
     * This is the only method that should be used for populating a Scenedescription out of ABObjects
     * @param abObject
     */
    public void addABObject(ABObject abObject){
        if(abObject == null){
            log.debug("WARNING - Could not add abObject because it was null\\");
            return;
        }
        
        SceneEntityBase newEntity = null;


        if (abObject.getShape() == ABShape.Poly) {
            Poly poly = (Poly) abObject;
            newEntity = new ScenePolygon(poly);
        }
        else if (abObject.getShape() == ABShape.Circle) {
            Circle circle = (Circle) abObject;
            newEntity = new SceneCircle(circle);

        }
        else if (abObject.getShape() == ABShape.Rect){
            if (abObject instanceof Rect){
                Rect rect = (Rect) abObject;
                newEntity = new SceneRectangle(rect);
            } else {
                newEntity = new SceneRectangle(abObject);
            }
        }
        else if (abObject.getShape() == ABShape.Triangle){
            // TODO (if we needed triangles ?)
        }

        if(newEntity != null){
            allEntitiesMap.put(newEntity.getId(), newEntity);
        }
    }


    public void populateWorld(World world) {
        
        for (SceneEntityBase entity : getAllEntities()) {
            EntityProperties entityProperties = this.entityPropertiesLUT.getEntityProperties(entity.getAbType());
            if(entityProperties != null){
                entity.setProperties(entityProperties);
            }
            entity.addToWorld(world);
        }
    }

    public List<SceneEntityBase> getAllDestroyableObjects() {

        List<SceneEntityBase> allBlocks = new LinkedList<>();
        for (SceneEntityBase sceneEntityBase : getAllEntities()) {
            if(sceneEntityBase.getAbType() == ABType.Ice
                || sceneEntityBase.getAbType() == ABType.Stone
                || sceneEntityBase.getAbType() == ABType.Wood
                || sceneEntityBase.getAbType() == ABType.TNT
                || sceneEntityBase.getAbType() == ABType.Pig){
                allBlocks.add(sceneEntityBase);
            }
        }
        return allBlocks;
    }

    public EntityPropertiesLUT getEntityPropertiesLUT() {
        return entityPropertiesLUT;
    }

    public void setEntityPropertiesLUT(EntityPropertiesLUT entityPropertiesLUT) {
        this.entityPropertiesLUT = entityPropertiesLUT;
    }

    public void logAllEntities(){
        log.debug("Loaded Scene Entities: ");
        for (SceneEntityBase entity : getAllEntities()) {
            log.debug( entity);
        }
    }

	public HashMap<Integer, SceneEntityBase> getAllEntitiesMap() {
		return allEntitiesMap;
	}

	public void setAllEntitiesMap(HashMap<Integer, SceneEntityBase> allEntitiesMap) {
		this.allEntitiesMap = allEntitiesMap;
	}

}