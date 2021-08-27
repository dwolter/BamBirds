/*****************************************************************************
 ** ANGRYBIRDS AI AGENT FRAMEWORK
 ** Copyright (c) 2014,XiaoYu (Gary) Ge, Stephen Gould,Jochen Renz
 **  Sahan Abeyasinghe, Jim Keys,   Andrew Wang, Peng Zhang
 ** All rights reserved.
 **This work is licensed under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
 **To view a copy of this license, visit http://www.gnu.org/licenses/
 *****************************************************************************/
package de.uniba.sme.bambirds.common.objects;

import java.io.Serializable;
import com.google.gson.annotations.SerializedName;

import de.uniba.sme.bambirds.common.gson.JsonRequired;

public class Shot implements Serializable {

	/**
	 *
	 */
	private static final long serialVersionUID = -5997041186996223322L;
	
	// point, where bird spawns
	@JsonRequired
	@SerializedName("sling_x")
	private int slingX;
	@JsonRequired
	@SerializedName("sling_y")
	private int slingY;

	// point, where you are dragging the point at for releasing it in the
	// slingshot
	@JsonRequired
	@SerializedName("drag_x")
	private int dragX;
	@JsonRequired
	@SerializedName("drag_y")
	private int dragY;

	// point, the shot is expected to end.
	// Not strictly required but necessary for adapting the shot to a new scene scale
	@SerializedName("target_x")
	private double targetX;
	@SerializedName("target_y")
	private double targetY;

	// time of shot
	@SerializedName("shot_time")
	private long shotTime;
	// time of tap
	@SerializedName("tap_time")
	private long tapTime;
	
	// how long is the bird going to fly
	private long timeOfFlight;

	private double velocity = 0.0;

	/**
	 * Default constructor. Instantiates all values with 0
	 */
	public Shot() {
		slingX = 0;
		slingY = 0;
		dragX = 0;
		dragY = 0;
		targetX = 0;
		targetY = 0;
		shotTime = 0;
		tapTime = 0;
	}

	/**
	 * 
	 * @param sling_x coordinate where Bird is
	 * @param sling_y coordinate where Bird is
	 * @param drag_x coordinate where to drag the Bird to
	 * @param drag_y coordinate where to drag the Bird to
	 * @param shot_time the release time (0 for immediate)
	 * @param tap_time the gap between the release time and the tap time
	 */
	public Shot(int sling_x, int sling_y, int drag_x, int drag_y, double target_x, double target_y, long shot_time, long tap_time) {
		this();
		this.slingX = sling_x;
		this.slingY = sling_y;
		this.dragX = drag_x;
		this.dragY = drag_y;
		this.targetX = target_x;
		this.targetY = target_y;
		this.shotTime = shot_time;
		this.tapTime = tap_time;
	}

	/**
	 * 
	 * @param sling_x coordinate where Bird is
	 * @param sling_y coordinate where Bird is
	 * @param drag_x coordinate where to drag the Bird to
	 * @param drag_y coordinate where to drag the Bird to
	 * @param shot_time the release time (0 for immediate)
	 */
	public Shot(int sling_x, int sling_y, int drag_x, int drag_y, long shot_time) {
		this();
		this.slingX = sling_x;
		this.slingY = sling_y;
		this.dragX = drag_x;
		this.dragY = drag_y;
		this.shotTime = shot_time;
	}

	/**
	 * 
	 * @param sling_x coordinate where Bird is
	 * @param sling_y coordinate where Bird is
	 * @param shot_time the release time (0 for immediate)
	 * @param tap_time the gap between the release time and the tap time
	 */
	public Shot(int sling_x, int sling_y, int shot_time, int tap_time) {
		this();
		this.slingX = sling_x;
		this.slingY = sling_y;
		this.shotTime = shot_time;
		this.tapTime = tap_time;
	}

	/**
	 * 
	 * @param s Another Shot to recreate
	 */
	public Shot(Shot s) {
		super();
		this.slingX = s.slingX;
		this.slingY = s.slingY;
		this.dragX = s.dragX;
		this.dragY = s.dragY;
		this.targetX = s.targetX;
		this.targetY = s.targetY;
		this.shotTime = s.shotTime;
		this.tapTime = s.tapTime;
	}

	/**
	 *
	 * @param description Shot description of the format "/x/y/dx/dy/tap"
	 *
	 * @throws IllegalArgumentException if values are not integers or the format is otherwise not valid
	 */
	public Shot(String description) {
		super();
		String[] values = description.split("/");
		try {
			this.slingX = Integer.parseInt(values[1]);
			this.slingY = Integer.parseInt(values[2]);
			this.dragX = Integer.parseInt(values[3]);
			this.dragY = Integer.parseInt(values[4]);
			this.tapTime = Integer.parseInt(values[5]);
		} catch (NumberFormatException | IndexOutOfBoundsException e) {
			throw new IllegalArgumentException("Shot description not valid", e);
		}
	}

	public void setTOF(long time){
		this.timeOfFlight = time;
	}
	
	public long getTOF(){
		return timeOfFlight;
	}

	public int getDragX() {
		return dragX;
	}

	public void setDragX(int dragX) {
		this.dragX = dragX;
	}

	public int getDragY() {
		return dragY;
	}

	public void setDragY(int dragY) {
		this.dragY = dragY;
	}

	public int getSlingX() {
		return slingX;
	}

	public void setSlingX(int slingX) {
		this.slingX = slingX;
	}

	public int getSlingY() {
		return slingY;
	}

	public void setSlingY(int slingY) {
		this.slingY = slingY;
	}

	public double getTargetX() {
		return targetX;
	}

	public void setTargetX(int targetX) {
		this.targetX = targetX;
	}

	public double getTargetY() {
		return targetY;
	}

	public void setTargetY(int targetY) {
		this.targetY = targetY;
	}

	public long getShotTime() {
		return shotTime;
	}

	public void setShotTime(long l) {
		this.shotTime = l;
	}

	public long getTapTime() {
		return tapTime;
	}

	public void setTapTime(long tapTime) {
		this.tapTime = tapTime;
	}

	public String prettyPrint() {
		String result = "";
		if (slingX == 0 && slingY == 0) {
			if (tapTime != 0)
				result += "tap at:  " + tapTime;
		} else
			result += "Shoot from: (" + (slingX + dragX) + "  " + (slingY + dragY) + " )"
					+ " at time  " + shotTime;

		return result;
	}

	@Override
	public String toString() {
		return String.format("/%d/%d/%d/%d/%d", getSlingX(), getSlingY(), getDragX(), getDragY(), getTapTime());
	}

	public double getVelocity() {
		return velocity;
	}

	public void setVelocity(double velocity) {
		this.velocity = velocity;
	}
	
	@Override
	public boolean equals(Object obj) {
		// TODO Auto-generated method stub
		if ( obj instanceof Shot) {
			Shot s = (Shot) obj;
			return this.slingX == s.slingX &&
					this.slingY == s.slingY &&
					this.dragX == s.dragX &&
					this.dragY == s.dragY &&
					this.shotTime == s.shotTime &&
					this.tapTime == s.tapTime;
		} else {			
			return super.equals(obj);
		}
	}
}
