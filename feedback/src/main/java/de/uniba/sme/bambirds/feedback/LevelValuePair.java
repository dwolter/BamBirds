package de.uniba.sme.bambirds.feedback;

public class LevelValuePair{
	private int level;
	private int value;
	
	public LevelValuePair(int level, int value){
		this.level = level;
		this.value = value;
	}
	
	public int getLevel(){
		return level;
	}
	public int getValue(){
		return value;
	}
	public void setLevel(int level){
		this.level = level;
	}
	public void setValue(int value){
		this.value = value;
	}
}