package de.uniba.sme.bambirds.feedback;

import de.uniba.sme.bambirds.common.objects.ab.ABObject;
import de.uniba.sme.bambirds.common.objects.ab.ABType;
import de.uniba.sme.bambirds.common.objects.ab.shape.Circle;
import de.uniba.sme.bambirds.common.objects.ab.shape.Rect;

public class ABObjectHelper {
    public static double getMaxSidelength(ABObject object){
        if(object instanceof Rect){
            return Math.max(((Rect)object).getPWidth(), ((Rect)object).getPLength());
        }else if(object instanceof Circle){
            return ((Circle)object).getRadius();
        }else{
            return Math.max(object.getWidth(), object.getHeight());
        }
    }

    public static double getMinSidelength(ABObject object){
        if(object instanceof Rect){
            return Math.min(((Rect)object).getPWidth(), ((Rect)object).getPLength());
        }else if(object instanceof Circle){
            return ((Circle)object).getRadius();
        }else{
            return Math.min(object.getWidth(), object.getHeight());
        }
    }

    public static ABObject getABObjectWith(double centerX, double centerY, double width, double height, double theta, ABType type){
        return (ABObject) new Rect(centerX,centerY,width,height,theta,type);
    }

    public static String writeABObjectAsString(ABObject object){
        String objectString = "";
        if(object == null){
            objectString += "\tABOBject[null]\n";
        }else{
            if(object instanceof Circle){
                objectString += String.format("\tABObject[id=%d, x=%.2f, y=%.2f, radius=%.2f, angle=%.2f, type=%s]\n",
            object.getId(),object.getCenterX(),object.getCenterY(),((Circle)object).getRadius(),object.getAngle(), object.getType());
            }else{      
                double width;
                double height;              
                if(object instanceof Rect){
                    width = ((Rect)object).getPWidth();
                    height = ((Rect)object).getPLength();
                }else{
                    width = object.getWidth();
                    height = object.getHeight();
                }
                objectString += String.format("\tABObject[id=%d, x=%.2f, y=%.2f, width=%.2f, height=%.2f, angle=%.2f, type=%s]\n",
            object.getId(),object.getCenterX(),object.getCenterY(),width,height,object.getAngle(), object.getType());
            }
        }
        return objectString;
    }
}