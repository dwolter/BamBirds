package de.uniba.sme.bambirds.feedback;

import de.uniba.sme.bambirds.common.objects.Shot;

public class FailedShot {
	
	private int level;
	private Shot shot;
	
	public FailedShot(int level, Shot shot) {
		this.level = level;
		this.shot = shot;
	}

	
	
	public int getLevel() {
		return level;
	}

	public void setLevel(int level) {
		this.level = level;
	}

	public Shot getShot() {
		return shot;
	}

	public void setShot(Shot shot) {
		this.shot = shot;
	}
	
	public String toString(){
		return "[FailedShot: Level:"+level+" Shot: "+shot.getDragX()+" "+shot.getDragY()+"]";
	}
	
	public boolean equals(Object object){
		
		if(object==null){
			return false;
		}
		
		if(!(object instanceof FailedShot)){
			return false;
		}
		
		FailedShot failedShot = (FailedShot) object;
		
		if((this.level == failedShot.level)&&(this.shot.getDragX() == failedShot.getShot().getDragX())&&(this.shot.getDragY() == failedShot.getShot().getDragY())){
			return true;
		} else {
			return false;
		}
	}
	

}
