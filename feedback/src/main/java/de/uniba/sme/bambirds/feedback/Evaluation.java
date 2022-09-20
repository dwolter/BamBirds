package de.uniba.sme.bambirds.feedback;

import java.util.List;

import de.uniba.sme.bambirds.common.objects.ab.ABObject;
import de.uniba.sme.bambirds.common.objects.ab.ABType;


public class Evaluation {
	
	private ShotInformation shotInfo;
	
	public Evaluation() {
		
	}

	public boolean isFailedShot(ShotInformation shotInfo) {
		
		this.shotInfo = shotInfo;
		if (noObjectsMoved() && noObjectsDestroyed() && noPigesMurdered()) {
			return true;
		}
	
		return false;
	}
	
	
	// Mehtod for defining a FailedShot
	private boolean noObjectsMoved(){
		if (shotInfo.getObjectsMoved().isEmpty()) {
			return true;
		}
		return false;
	}
	
	private boolean noObjectsDestroyed(){
		if (this.shotInfo.getObjectsDestroyed().isEmpty()) {
			return true;
		}
		return false;
	}
	
	private boolean noPigesMurdered(){
		if (this.shotInfo.getMurderedPigs().isEmpty()) {
			return true;
		}
		return false;
	}
	
}
