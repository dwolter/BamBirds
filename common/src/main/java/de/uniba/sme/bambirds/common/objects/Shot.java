/*****************************************************************************
 ** ANGRYBIRDS AI AGENT FRAMEWORK
 ** Copyright (c) 2014,XiaoYu (Gary) Ge, Stephen Gould,Jochen Renz
 **  Sahan Abeyasinghe, Jim Keys,   Andrew Wang, Peng Zhang
 ** All rights reserved.
 **This work is licensed under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
 **To view a copy of this license, visit http://www.gnu.org/licenses/
 *****************************************************************************/
package de.uniba.sme.bambirds.common.objects;

import com.google.gson.annotations.SerializedName;
import de.uniba.sme.bambirds.common.gson.JsonRequired;

import java.io.Serializable;
import java.util.Objects;

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
	// slingshot (relative to slingX and slingY)
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
	 * @param slingX   coordinate where Bird is
	 * @param slingY   coordinate where Bird is
	 * @param dragX    coordinate where to drag the Bird to
	 * @param dragY    coordinate where to drag the Bird to
	 * @param targetX  coordinate of the targeted point (used to update shot params when scene scale changes)
	 * @param targetY  coordinate of the targeted point (used to update shot params when scene scale changes)
	 * @param shotTime the release time (0 for immediate)
	 * @param tapTime  the gap between the release time and the tap time
	 */
	public Shot(final int slingX, final int slingY, final int dragX, final int dragY, final double targetX, final double targetY, final long shotTime, final long tapTime) {
		this();
		this.slingX = slingX;
		this.slingY = slingY;
		this.dragX = dragX;
		this.dragY = dragY;
		this.targetX = targetX;
		this.targetY = targetY;
		this.shotTime = shotTime;
		this.tapTime = tapTime;
	}

	/**
	 * @param slingX   coordinate where Bird is
	 * @param slingY   coordinate where Bird is
	 * @param dragX    coordinate where to drag the Bird to
	 * @param dragY    coordinate where to drag the Bird to
	 * @param shotTime the release time (0 for immediate)
	 */
	public Shot(final int slingX, final int slingY, final int dragX, final int dragY, final long shotTime) {
		this();
		this.slingX = slingX;
		this.slingY = slingY;
		this.dragX = dragX;
		this.dragY = dragY;
		this.shotTime = shotTime;
	}

	/**
	 * @param slingX   coordinate where Bird is
	 * @param slingY   coordinate where Bird is
	 * @param shotTime the release time (0 for immediate)
	 * @param tapTime  the gap between the release time and the tap time
	 */
	public Shot(int slingX, int slingY, int shotTime, int tapTime) {
		this();
		this.slingX = slingX;
		this.slingY = slingY;
		this.shotTime = shotTime;
		this.tapTime = tapTime;
	}

	/**
	 * @param s Another Shot to recreate
	 */
	public Shot(final Shot s) {
		super();
		this.slingX = s.slingX;
		this.slingY = s.slingY;
		this.dragX = s.dragX;
		this.dragY = s.dragY;
		this.targetX = s.targetX;
		this.targetY = s.targetY;
		this.shotTime = s.shotTime;
		this.tapTime = s.tapTime;
		this.timeOfFlight = s.timeOfFlight;
		this.velocity = s.velocity;
	}

	/**
	 * @param description Shot description of the format "/x/y/dx/dy/tap"
	 * @throws IllegalArgumentException if values are not integers or the format is otherwise not valid
	 */
	public Shot(final String description) {
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

	public void setTOF(final long time) {
		this.timeOfFlight = time;
	}

	public long getTOF() {
		return timeOfFlight;
	}

	public int getDragX() {
		return dragX;
	}

	public void setDragX(final int dragX) {
		this.dragX = dragX;
	}

	public int getDragY() {
		return dragY;
	}

	public void setDragY(final int dragY) {
		this.dragY = dragY;
	}

	public int getSlingX() {
		return slingX;
	}

	public void setSlingX(final int slingX) {
		this.slingX = slingX;
	}

	public int getSlingY() {
		return slingY;
	}

	public void setSlingY(final int slingY) {
		this.slingY = slingY;
	}

	public double getTargetX() {
		return targetX;
	}

	public void setTargetX(final int targetX) {
		this.targetX = targetX;
	}

	public double getTargetY() {
		return targetY;
	}

	public void setTargetY(final int targetY) {
		this.targetY = targetY;
	}

	public long getShotTime() {
		return shotTime;
	}

	public void setShotTime(final long l) {
		this.shotTime = l;
	}

	public long getTapTime() {
		return tapTime;
	}

	public void setTapTime(final long tapTime) {
		this.tapTime = tapTime;
	}

	public String prettyPrint() {
		StringBuilder result = new StringBuilder();
		if (slingX == 0 && slingY == 0) {
			if (tapTime != 0) {
				result.append("tap at:  ").append(tapTime);
			}
		} else {
			result.append("Shoot from: (")
					.append(slingX + dragX)
					.append("  ")
					.append(slingY + dragY)
					.append(" )")
					.append(" at time  ")
					.append(shotTime);
		}
		return result.toString();
	}

	@Override
	public String toString() {
		return String.format("/%d/%d/%d/%d/%d", getSlingX(), getSlingY(), getDragX(), getDragY(), getTapTime());
	}

	public double getVelocity() {
		return velocity;
	}

	public void setVelocity(final double velocity) {
		this.velocity = velocity;
	}

	@Override
	public int hashCode() {
		return Objects.hash(slingX, slingY, dragX, dragY, targetX, targetY, shotTime, tapTime, timeOfFlight, velocity);
	}

	@Override
	public boolean equals(final Object obj) {
		// TODO Auto-generated method stub
		if (obj instanceof Shot) {
			Shot s = (Shot) obj;
			return this.slingX == s.slingX
					&& this.slingY == s.slingY
					&& this.dragX == s.dragX
					&& this.dragY == s.dragY
					&& this.shotTime == s.shotTime
					&& this.tapTime == s.tapTime;
		} else {
			return super.equals(obj);
		}
	}
}
