package de.uniba.sme.bambirds.planner.physicssimulation.scene.entities;

import org.jbox2d.dynamics.World;

/**
 * Represents and mirrors an Entity that may at any time be instantiated into a jbox2d-World as a jbox2d-Object.
 * It may be manipulated before being instanced.
 * When making Scene-Snapshots, 
 * When instantiated into a jbox2d-world, the instance will hold a pointer/reference to its scene-entity-object-origin ( stored in body.m_userdata ).
 * Defines basic functionality of an Entity, that can be added to a simulation.
 * Specifically: Instantiation in a JBox2D-world, as well as geometric transformation 
 * such as scaling, rotation an transformation.
 */
public interface SceneEntity {
 

    
    public String getGlobalID() ;

    public void setGlobalID(String globalID);

    public int getId() ;

    public void setId(int id) ;

    public float getTotalDamage();
    // public float setTotalDamage();
    public void addDamage(float additionalDamage);


    /**
     * Adds a new jbox2d-element (e.g. a box) to a given Box2D world. 
     * The jbox2d-Element is instanciated with the parameters currently hold by this
     * {@link SceneEntity}. Manipulating the {@link SceneEntity} 
     * afterwards, does not automatically affect the jbox2d-instance.
     * 
     * @param world The world the {@link SceneEntity} shall be instatiated in
     */
    public void addToWorld(World world);


    /**
     * This takes the current physical stateinformation from a jbox2D-physicsbody, and stores its values into the SceneEntity ( into its membervariables)
     * The method can be used to make a snapshot from simualted physicsentities. Because the Entity-Data lives outside and independently of a concrete simulation, we can at a later point use the data to recreate and simulate a physics-world which will behave exactly like the world in the moment of the snapshot.
     * @param jBox2DBody the JBox2D body from a JBox2D-world
     */
    public void storeStateFromPhysicsBodySnapshot(org.jbox2d.dynamics.Body jBox2DBody);


    /**
     * Scales all relevant Paremters as if the entire coordinate System is scaled
     * (i.e. also scales the {@link SceneEntity}s position relative to the global coordinate-system-origin). 
     * Can be used to scale entire scenes, if applied to all its {@link SceneEntity}s.
     * 
     * @param scalingFactor Factor to scale all Entity parameters with
     */
    public void scaleGlobally(float scalingFactor);

    /**
     * Scales local size of the {@link SceneEntity} i.e. it scales it relative to its own center-point, independently of where the scenenetity is positioned in the global coordinate-system
     * (i.e. does not change the position of the entities' center-point).
     * 
     * @param scalingFactor Factor to scale aall Entity parameter by
     */
    public void scaleLocally(float scalingFactor);

    /**
     * Moves the {@link SceneEntity} by x/y along x/y axis in global space.
     * 
     * @param x offset on X axis
     * @param y offset in Y axis
     */
    public void translate(float x, float y);


    /**
     * Moves the {@link SceneEntity} such that its center is at x/y in global space.
     * 
     * @param x x coordinate
     * @param y y coordinate
     */
    public void setCenterPosition(float x, float y);

    /**
     * Rotates the {@link SceneEntity} by the given angle around its own center-point
     * 
     * @param angle angle to rotate
     */
    public void rotateLocally(float angle);

    /**
     * Sets the {@link SceneEntity}s local rotation to a given angle.
     * 
     * @param angle new rotation (w.r.t X axis)
     */
    public void setLocalRotation(float angle);

    /**
     * Mirrors the {@link SceneEntity} on the X axis.
     */
    public void mirrorOnXAxis();

    /**
     * Mirrors the {@link SceneEntity} on the Y axis.
     */
    public void mirrorOnYAxis();
    
}