package de.uniba.sme.bambirds.common.objects;

public class Triplet<S extends Shot, P extends Plan, N extends Number> {

	private S shot;
	private P target;
	private N damagePoints;

	public Triplet(final S first, final P second, final N damagePoints) {
		this.shot = first;
		this.target = second;
		this.damagePoints = damagePoints;
	}

	@Override
	public boolean equals(final Object o) {
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

	public void setShot(final S shot) {
		this.shot = shot;
	}

	public P getTarget() {
		return target;
	}

	public void setTarget(final P target) {
		this.target = target;
	}

	public N getDamage() {
		return damagePoints;
	}

	public void setDamage(final N damagePoints) {
		this.damagePoints = damagePoints;
	}

}
