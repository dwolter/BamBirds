/*****************************************************************************
 ** ANGRYBIRDS AI AGENT FRAMEWORK
 ** Copyright (c) 2014,XiaoYu (Gary) Ge, Stephen Gould,Jochen Renz
 **  Sahan Abeyasinghe, Jim Keys,   Andrew Wang, Peng Zhang
 ** All rights reserved.
 **This work is licensed under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
 **To view a copy of this license, visit http://www.gnu.org/licenses/
 *****************************************************************************/
package ab.demo.other;

public class Shot {

	// point, where bird spawns
	private int x;
	private int y;

	// point, where you are dragging the point at for releasing it in the
	// slingshot
	private int dx;
	private int dy;

	// time of shot
	private long t_shot;
	// time of tap
	private long t_tap;
	
	// how long is the bird going to fly
	private long timeOfFlight;

	private double velocity = 0.0;
	
	public void setTOF(long time){
		this.timeOfFlight = time;
	}
	
	public long getTOF(){
		return timeOfFlight;
	}

	public int getDx() {
		return dx;
	}

	public void setDx(int dx) {
		this.dx = dx;
	}

	public int getDy() {
		return dy;
	}

	public Shot(int x, int y, int dx, int dy, int t_shot, int t_tap) {
		super();
		this.x = x;
		this.y = y;
		this.dx = dx;
		this.dy = dy;
		this.t_shot = t_shot;
		this.t_tap = t_tap;
	}

	public Shot(int x, int y, int dx, int dy, long l) {
		super();
		this.x = x;
		this.y = y;
		this.dx = dx;
		this.dy = dy;
		this.t_shot = l;
	}

	public void setDy(int dy) {
		this.dy = dy;
	}

	public Shot() {
		x = 0;
		y = 0;
		dx = 0;
		dy = 0;
		t_shot = 0;
		t_tap = 0;
	}

	public Shot(int x, int y, int t_shot, int t_tap) {
		super();
		this.x = x;
		this.y = y;
		this.t_shot = t_shot;
		this.t_tap = t_tap;
	}

	public int getX() {
		return x;
	}

	public void setX(int x) {
		this.x = x;
	}

	public int getY() {
		return y;
	}

	public void setY(int y) {
		this.y = y;
	}

	public long getT_shot() {
		return t_shot;
	}

	public void setT_shot(long l) {
		this.t_shot = l;
	}

	public long getT_tap() {
		return t_tap;
	}

	public void setT_tap(long t_tap) {
		this.t_tap = t_tap;
	}

	public String toString() {
		String result = "";
		if (x == 0 && y == 0) {
			if (t_tap != 0)
				result += "tap at:  " + t_tap;
		} else
			result += "Shoot from: (" + (x + dx) + "  " + (y + dy) + " )"
					+ " at time  " + t_shot;

		return result;

	}

	public double getVelocity() {
		return velocity;
	}

	public void setVelocity(double velocity) {
		this.velocity = velocity;
	}
}
