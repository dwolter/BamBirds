package de.uniba.sme.bambirds.planner.physicssimulation.physics.modification.tracking;

import org.jbox2d.common.Vec2;

public class CollisionInfo {
    public Vec2 position;
    public Vec2 impulse;
	public String selfGlobalID;
	public String otherGlobalID;
	public int frame;
	public CollisionInfo(Vec2 position, Vec2 impulse, String selfGlobalID,String otherGlobalID, int frame) {
		this.position = position;
		this.impulse = impulse;
		this.selfGlobalID = selfGlobalID;
		this.otherGlobalID = otherGlobalID;
		this.frame = frame;
	}



    
}
