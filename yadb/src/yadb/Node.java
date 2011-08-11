package yadb;

class Node
{
	/** Minimalni pocet prvku v Nodu. Musi platit: 2 &lt;= N &lt;= M/2 */
	static final int N = 2;

	/** Maximalni pocet prvku v Nodu. */
	static final int M = 5;

	/** Je tento node jeden z listu? */
	boolean isLeaf;

	/** Kolik ma tento node synu. */
	short used;

	/** Odkaz na hlavni strukturu stromu, kteremu tento node prislusi. */
	RTree rt;

	/**
	 * Pokud je tento node list je zde ulozena n-dimenzionalni vektor
	 * informaci.
	 */
	Comparable[] leaf;

	/** Horni meze vsech intervalu. */
	Comparable[] boundU;

	/** Dolni meze vsech intervalu. */
	Comparable[] boundD;

	/** Seznam synu. Je jich maximalne Node.M a minimalne Node.N */
	Node[] childrens;

	/** Opravi meze nodu. */
	void normalize()
	{
		if (isLeaf) return;
		if (used == 0) {
			for (int j=0; j<boundU.length; j++)
				boundD[j] = boundU[j] = null;
			return;
		}

		for (int j=0; j<boundU.length; j++) {
			boundU[j] = childrens[0].getBoundU(j);
			boundD[j] = childrens[0].getBoundD(j);

			for (int i=1; i<used; i++) {
				boundU[j] = cMax(boundU[j], childrens[i].getBoundU(j));
				boundD[j] = cMin(boundD[j], childrens[i].getBoundD(j));
			}
		}
	}

	/** Vrati v i-te dimenzi horni mez */
	Comparable getBoundU(int i) { return isLeaf ? leaf[i] : boundU[i]; }

	/** Vrati v i-te dimenzi dolni mez */
	Comparable getBoundD(int i) { return isLeaf ? leaf[i] : boundD[i]; }

	/** Vrati pocet dimenzi */
	int getDimension()  { return isLeaf ? leaf.length : boundU.length; }

	/** Zjisti typ i-te dimenze */
	Type getType(int i) { return this.rt.cols[i].type; }

	/**
	 * Prida novy node jako dite tohoto. Zaroven s nim rozsiri meze pokud je to
	 * potreba a pokud kapacita tohoto nodu presahne hranici, tak jej rozdeli.
	 *
	 * @param n node k pridani
	 * @return null pokud nedoslo ke splitu, jinak odkaz na druhou vetev splitu
	 */
	Node add(Node n)
	{
		if (n == null) return null;
		this.childrens[this.used++] = n;

		for (int dim=this.getDimension(), i=0; i<dim; i++) {
			this.boundU[i] = cMax(this.boundU[i], n.getBoundU(i));
			this.boundD[i] = cMin(this.boundD[i], n.getBoundD(i));
		}

		return invokeSplit();
	}

	/**
	 * Rozsiri tento node tak, aby se do nej vesel ins.
	 *
	 * @param ins vkladany bod, musi byt list
	 */
	void enlarge(Node ins)
	{
		int dim=this.getDimension();
		assert ins.getDimension() == dim;

		for (int i=0; i<dim; i++) {
			this.boundU[i] = cMax(this.boundU[i], ins.getBoundU(i));
			this.boundD[i] = cMin(this.boundD[i], ins.getBoundD(i));
		}
	}

	/**
	 * Pokud je node plny rozdeli jej. Splituje tak, aby soucet ploch obou
	 * vetvi byl nejmensi.
	 *
	 * @return null pokud nedoslo ke splitu, jinak odkaz na druhou vetev splitu
	 */
	Node invokeSplit()
	{
		assert !this.isLeaf;
		if (this.used <= M) return null;
		Node n1 = this;
		Node n2 = newEntry(this.rt, this.getDimension());
		Node[] ch = n1.childrens;
		n1.childrens = new Node[M+1];
		n1.used = 0;

		// pickSeeds - vybere zastupce dvou vetvi po splitu
		// podle nejvetsiho normalizovanyho rozdilu nejmensich hornich a
		// nejvetsich dolnich prvku v nejakejm rozmeru
		double maxNormalizedSeparation = 0, normalizedSeparation;
		int maxU = -1, maxD = -1;
		for (int diMax=this.getDimension(), d=0; d<diMax; d++) {
			int iU = 0, iD = 0;
			for (int i=1; i<this.used; i++) {
				if (ch[i].getBoundU(d).compareTo(ch[iU].getBoundD(d)) < 0) iU = i;
				if (ch[i].getBoundD(d).compareTo(ch[iD].getBoundD(d)) > 0) iD = i;
			}

			normalizedSeparation =
				  RTree.distance(this.getType(d), ch[iU].getBoundU(d), ch[iD].getBoundD(d))
				/ RTree.distance(this.getType(d), this.getBoundU(d),   this.getBoundD(d));

			if (maxU == -1 || maxNormalizedSeparation < normalizedSeparation) {
				maxNormalizedSeparation = normalizedSeparation;
				maxU = iU; maxD = iD;
			}

			if (maxU == maxD)
				maxD += maxD == 0 ? 1 : -1;
		}
		n1.add(ch[maxU]);
		n1.normalize();
		n2.add(ch[maxD]);
		ch[maxU] = null; ch[maxD] = null;

		// pickNext - roztridi prvky mezi dva podstromy
		for (int c=3; c<=M; c++) {
			// rozsypat zbytek, pokud uz mam dost malo pro jednu vetev
			if ((M - c + n1.used == N) || (M - c + n2.used == N)) {
				Node slim = n1.used < n2.used ? n1 : n2;
				for (int i=0; i<M+1; i++) {
					if (ch[i] == null) continue;
					slim.add(ch[i]);
				}
				break;
			}

			// nalezeni nejvhodnejsiho k pridani do jedne z vetvi
			double max = -1; int maxI = -1;
			for (int i=0; i<M+1; i++) {
				if (ch[i] == null) continue;
				double
					d1 = enlargeRequied(n1, ch[i]),
					d2 = enlargeRequied(n2, ch[i]);

				if (Math.abs(d1 - d2) > max)
					maxI = i;
			}

			// pridat maxI do nektere z vetvi splitu
			if (enlargeRequied(n1, ch[maxI]) > enlargeRequied(n2, ch[maxI]))
				n1.add(ch[maxI]);
			else
				n2.add(ch[maxI]);
			ch[maxI] = null;
		}

		return n2;
	}

	/** Zjisti o kolik by se rozsiril n pridanim toInsert. */
	static double enlargeRequied(Node n, Node toInsert)
	{
		assert n != null && toInsert != null;
		int dim = n.getDimension();
		assert dim == toInsert.getDimension() && n.rt == toInsert.rt;

		double res = 1;
		for (int i=0; i<dim; i++) {
			Comparable
				nu = n.getBoundU(i),
				nd = n.getBoundD(i),
				iu = toInsert.getBoundU(i),
				id = toInsert.getBoundD(i);

			res *= java.lang.Math.max(1,
				  (nu.compareTo(iu) < 0 ? RTree.distance(n.getType(i), nu, iu) : 0)
				+ (nd.compareTo(id) > 0 ? RTree.distance(n.getType(i), nd, id) : 0));
		}

		return res;
	}

	/** Najde minimum dvou porovnatelnejch prvku. Null reprezentuje +oo */
	static Comparable cMin(Comparable a, Comparable b) {
		if (a == null) return b;
		if (b == null) return a;
		return a.compareTo(b) < 0 ? a : b;
	}

	/** Najde maximum dvou porovnatelnejch prvku. Null reprezentuje -oo */
	static Comparable cMax(Comparable a, Comparable b) {
		if (a == null) return b;
		if (b == null) return a;
		return a.compareTo(b) > 0 ? a : b;
	}

	static Node newLeaf(RTree rt, Comparable[] l)
	{
		Node n = new Node();
		n.isLeaf = true;
		n.leaf = l;
		n.rt = rt;
		return n;
	}

	static Node newEntry(RTree rt, int length)
	{
		Node n = new Node();
		n.isLeaf = false;
		n.childrens = new Node[Node.M+1];
		n.boundU = new Comparable[length];
		n.boundD = new Comparable[length];
		n.used = 0;
		n.rt = rt;
		return n;
	}

	static Node newRoot(RTree rt, Node a, Node b)
	{
		assert a.getDimension() == b.getDimension();

		Node root = newEntry(rt, a.getDimension());
		root.childrens[0] = a;
		root.childrens[1] = b;
		root.used = 2;
		root.normalize();
		return root;
	}
}
