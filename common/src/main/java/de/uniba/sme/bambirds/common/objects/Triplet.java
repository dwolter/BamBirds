package de.uniba.sme.bambirds.common.objects;

public class Triplet<S extends Shot, T extends Target, N extends Number > {

	private S shot;
	private T target;
	private N damagePoints;

	public Triplet(S first, T second, N damagePoints) {
		this.shot = first;
		this.target = second;
		this.damagePoints = damagePoints;
	}

	@Override
	public boolean equals(Object o) {
		if (!(o instanceof Triplet)) {
			return false;
		}
		Triplet<?, ?, ?> p = (Triplet<?, ?, ?>) o;
		return shot.equals(p.shot) && target.equals(p.target) && damagePoints.equals(p.damagePoints);
	}

	@Override
	public int hashCode() {
		return (shot == null ? 0 : shot.hashCode()) ^ (target == null ? 0 : target.hashCode())
				^ (damagePoints == null ? 0 : damagePoints.hashCode());
	}

	public Shot getShot() {
		return shot;
	}

	public void setShot(S shot) {
		this.shot = shot;
	}

	public T getTarget() {
		return target;
	}

	public void setTarget(T target) {
		this.target = target;
	}

	public N getDamage() {
		return damagePoints;
	}

	public void setDamage(N damagePoints) {
		this.damagePoints = damagePoints;
	}

}
