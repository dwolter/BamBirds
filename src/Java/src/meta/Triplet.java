package meta;

public class Triplet<Shot, Target, Integer> {

	private Shot shot;
	private Target target;
	private Integer damagePoints;

	public Triplet(Shot first, Target second, Integer damagePoints) {
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

	private static boolean equals(Object x, Object y) {
		return (x == null && y == null) || (x != null && x.equals(y));
	}

	@Override
	public int hashCode() {
		return (shot == null ? 0 : shot.hashCode()) ^ (target == null ? 0 : target.hashCode())
				^ (damagePoints == null ? 0 : damagePoints.hashCode());
	}

	public Shot getShot() {
		return shot;
	}

	public void setShot(Shot shot) {
		this.shot = shot;
	}

	public Target getTarget() {
		return target;
	}

	public void setTarget(Target target) {
		this.target = target;
	}

	public Integer getDamage() {
		return damagePoints;
	}

	public void setDamage(Integer damagePoints) {
		this.damagePoints = damagePoints;
	}

}
