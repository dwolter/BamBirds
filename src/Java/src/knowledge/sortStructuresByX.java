/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package knowledge;

import ab.vision.ABObject;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;

/**
 *
 * @author lordminx
 */
public class sortStructuresByX implements Comparator<List<ABObject>> {
    
    @Override
    public int compare(List<ABObject> struct1, List<ABObject> struct2) {
        
        Collections.sort(struct1, new XComparator());
        Collections.sort(struct2, new XComparator());
        
        ABObject left1 = struct1.get(0);
        ABObject right1 = struct1.get(struct1.size() -1);
        
        ABObject left2 = struct2.get(0);
        ABObject right2 = struct2.get(struct2.size() -1);
        
        if (left1.x < left2.x && right1.x < right2.x) {
            return -1;
        } else if (left1.x > left2.x && right1.x > right2.x) {
            return 1;
        } else {
            return 0;
        }
    }
}
