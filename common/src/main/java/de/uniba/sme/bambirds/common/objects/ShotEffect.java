package de.uniba.sme.bambirds.common.objects;

import de.uniba.sme.bambirds.common.objects.ab.ABObject;

public class ShotEffect {

  public enum EffectType {
    DESTROY,MOVE,FREE
  }
  
  private ABObject object;
  private EffectType type;

  public ShotEffect(ABObject object, EffectType type) {
    this.object = object;
    this.type = type;
  }

  public EffectType getType() {
    return type;
  }

  public ABObject getObject() {
    return object;
  }

  @Override
  public String toString() {
    return "{" + type + ":" + object.getGlobalID() + "}";
  }
}
