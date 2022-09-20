package de.uniba.sme.bambirds.common.objects;

public class Timer {
    private final long startTime;
    public Timer(){
        this.startTime = System.currentTimeMillis();
    }

    public long getElapsedTime(){
        return System.currentTimeMillis() - startTime;
    }
}