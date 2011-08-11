package yadb;
import java.util.Hashtable;
import java.util.List;
import java.util.LinkedList;
import java.util.ArrayList;

/**
 * R-stromy jsou vicerozmernou obdobou B-stromu. Jejich popis je k nalezeni na:
 *     http://www-db.deis.unibo.it/courses/SI-LS/papers/Gut84.pdf
 */
public class RTree
{
	/** Specifikace radku tabulky. */
	public Column[] cols;

	/** Mapovani jmeno-sloupecku na cislo sloupecku v cols. */
	Hashtable<String, Integer> records = new Hashtable<String, Integer>();

	/** Odkaz na vrchol stromu. */
	protected Node root;

	/** Zamknuto, protoze nekdo meni strom. */
	protected static boolean locked = false;

	/** Pocet vlaken prohledavajicich strom. */
	protected static int readingCounter = 0;

	public RTree(Column[] columns)
	{
		int i = 0;
		cols = new Column[columns.length];

		for (Column c : columns) {
			cols[i] = c;
			records.put(c.name, i++);
		}
	}

	/**
	 * Zamknuti stromu nebo zviseni readingCounteru.
	 *
	 * @param justReading netreba zamknout proti cteni
	 */
	protected synchronized void lock(boolean justReading)
	{
		while (locked || (!justReading && readingCounter > 0))
			try { wait(); } catch (InterruptedException ie) {}

		if (justReading)
			readingCounter++;
		else
			locked = true;
	}

	/**
	 * Odemknuti stromu nebo snizeni readingCounteru.
	 *
	 * @param justReading snizujeme cteci counter
	 */
	protected synchronized void unlock(boolean justReading)
	{
		if (justReading)
			readingCounter--;
		else
			locked = false;
		notifyAll();
	}

	/**
	 * Vrati seznam prvku odpovidajici danym podminkam.
	 *
	 * @param cons seznam podminek
	 * @return seznam prvku vyhovujicich podminkam
	 */
	public Comparable[][] search(Condition[] cons)
	{
		lock(true);
		LinkedList<Node> stack = new LinkedList<Node>();
		List<Comparable[]> found = new LinkedList<Comparable[]>();

		stack.add(root);

		while (!stack.isEmpty()) {
			Node n = stack.remove();

			if (n == null) continue;
			if (n.isLeaf) {
				if (satisfyConditions(n, cons)) found.add(n.leaf);
				continue;
			}

			for (Node e : n.childrens)
				if (satisfyConditions(e, cons)) stack.add(e);
		}

		unlock(true);
		return found.toArray(new Comparable[0][0]);
	}

	/**
	 * Smaze vrcholi odpovidajici podminkam. Pokud se nejaka vetev stromu
	 * zmensi pod minimalni velikost smaze se a jeji prvky se opet zatridi do
	 * stromu kam patri.
	 *
	 * @param cons mazaci podminky
	 */
	public Comparable[][] delete(Condition[] cons)
	{
		if (this.root == null) return new Comparable[0][0];
		lock(false);

		ArrayList<Node> stack = new ArrayList<Node>();   // zasobnik prochazenych vetvi
		ArrayList<Node> fathers = new ArrayList<Node>(); // otcove nodu se stejnym indexem na stacku
		ArrayList<Integer> depths = new ArrayList<Integer>(); // hloubky nodu ve stromu
		List<Comparable[]> deleted = new LinkedList<Comparable[]>(); // seznam smazanych
		LinkedList<Integer> toReAdd = new LinkedList<Integer>(); // indexy nodu, ktere bude treba
		                                                         // znovu vlozit do stromu
		stack.add(root);
		fathers.add(null);
		depths.add(0);

		// promazani listu
		for (int i=0; i < stack.size(); i++) {
			Node n = stack.get(i);
			int d = depths.get(i);
			if (n == null) continue;

			// smazeme vyhovujici listy
			if (n.isLeaf) {
				if (!satisfyConditions(n, cons)) continue;
				stack.set(i, null);
				deleted.add(n.leaf);
				deleteFromFather(fathers.get(i), n);
				continue;
			}

			for (Node e : n.childrens) {
				if (satisfyConditions(e, cons)) {
					stack.add(e);
					fathers.add(n);
					depths.add(d+1);
				}
			}
		}

		// projdeme odzadu vsechny otce a zjistime, zda nejsou moc mali
		for (int i=stack.size()-1; i > 0; i--) {
			Node n = stack.get(i);
			if (n == null) continue;

			if (n.used < Node.N) {
				deleteFromFather(fathers.get(i), n);
				toReAdd.add(i);
			} else
				n.normalize();
		}

		int skip = -1;
		// hack - mame li prazdnyho roota, dame mu alespon nejaky vrchol,
		//        aby bylo do ceho vkladat
		if (root.used == 0) {
			for (int i : toReAdd) {
				Node n = stack.get(i);
				int d = depths.get(i);

				// aby se to nerozbilo, potrebujeme vrchol v hloubce 1
				if (d == 1 && n.used > 0) {
					skip = i;
					for (int j=0; j<n.used; j++)
						insertInto(root, n.childrens[j], d);
					break;
				}
			}
		}
		root.normalize();

		// znovuvlozeni smazanych vetvi
		for (int i : toReAdd) {
			if (i == skip) continue;
			Node n = stack.get(i);
			int d = depths.get(i);

			for (int j=0; j<n.used; j++)
				maybeSplitRoot(insertInto(root, n.childrens[j], d));
		}
	
		// oprava roota
		if (root != null)
			switch (root.used) {
				case 0: root = null; break;
				case 1: root = root.childrens[0];
			}

		unlock(false);
		return deleted.toArray(new Comparable[0][0]);
	}

	protected void deleteFromFather(Node father, Node n)
	{
		if (father == null) return;

		for (int j=0; j < father.used && father.childrens[j] != null; j++)
			if (n == father.childrens[j]) {
				father.childrens[j] = father.childrens[--father.used];
				father.childrens[father.used] = null;
			}
	}

	/**
	 * Vyhovel dany node vsem podminkam?
	 *
	 * @param n node
	 * @param cons seznam podminek
	 * @return zda jim vyhovel
	 */
	protected static boolean satisfyConditions(Node n, Condition[] cons)
	{
		if (cons == null) return true;
		for (Condition c : cons)
			if (!c.conditionOK(n)) return false;
		return true;
	}

	/**
	 * Vlozeni prvku databaze.
	 *
	 * @param ins vstupni polozky pro vlozeni do databaze, musi jich byt
	 *            cols.length, navic musi sedet typy
	 */
	public void insert(Comparable[] ins)
	{
		if (ins.length != cols.length) throw new IllegalArgumentException();
		Node newNode = Node.newLeaf(this, ins);
		lock(false);

		if (root == null)
			root = newNode;
		else if (root.isLeaf)
			root = Node.newRoot(this, root, newNode);
		else
			maybeSplitRoot(insertInto(root, newNode, -1));
		unlock(false);
	}

	/**
	 * V pripade nenulloveho n rozdeli roota.
	 *
	 * @param n vrchol pro kontrolu splitu
	 */
	protected void maybeSplitRoot(Node n)
	{
		if (n != null) // rozdeleni rootu
			root = Node.newRoot(this, n, root);
	}

	/**
	 * Vlozi do nodu, popripade rekurzivne ins.
	 *
	 * @param n do tohoto Node se vklada
	 * @param ins co se vklada
	 * @param maxLevel maximalni hloubka, do ktere vlozit node, pokud je &lt;0
	 *                 vklada se az do listu
	 * @return null, pokud nedoslo k rozdeleni n, jinak druhou rozdelenou cast
	 */
	protected Node insertInto(Node n, Node ins, int maxLevel)
	{
		assert n != null;
		assert n != ins;

		// choose leaf
		if (n.used == 0 || n.childrens[0].isLeaf || maxLevel == 0)
			return n.add(ins);


		// otherwise choose region to insert (that needs least enlargement)
		double minEn = Double.MAX_VALUE;
		int minI = -1;
		for (int i=0; i<n.used; i++) {
			if (n.childrens[i] == null) continue;
			double en = Node.enlargeRequied(n.childrens[i], ins);
			if (en < minEn) minI = i;
		}

		// zmena mezi intervalu nodu n
		n.enlarge(ins);

		// if tree was adjusted, also pass new branch into this node
		// if this node is full invoke split and propagate it
		Node split = insertInto(n.childrens[minI], ins, maxLevel-1);
		return n.add(split);
	}

	/**
	 * Zjisti vzdalenost mezi dvema objekty daneho typu.
	 *
	 * Vzdalenost mezi dvema Stringy
	 * max{|x-y|, |y-x|} - viz String Matching with Metric Trees Using an
	 *                     Approximate Distance; Ilaria Bartolini, Paolo
	 *                     Ciaccia and Marco Pattela; University of Bologna, Italy
	 * rozdil je zde rozdil poctu vyskytu jednoho znaku v a od b (min. do 0)
	 */
	public static double distance(Type t, Comparable a, Comparable b)
	{
		switch (t) {
			case STRING:
				String
					sa = (String) a,
					sb = (String) b;

				int
					la = sa.length(),
					lb = sb.length();

				Hashtable<Character, Integer>
					ha = hashCountChars(sa, la),
					hb = hashCountChars(sb, lb);

				return Math.max(la - subtractHashes(ha, hb), lb - subtractHashes(hb, ha));
			case INTEGER:
				return Math.abs((Integer)a - (Integer)b);
			case REAL:
				double res = Math.abs((Double)a - (Double)b);
				return res < 1 ? res + 1 : res;
			default:
				throw new IllegalArgumentException();
		}
	}

	/** Spocte rozdil dvou hashu. */
	protected static<K> int subtractHashes(Hashtable<K, Integer> ha, Hashtable<K, Integer> hb)
	{
		int count = 0, ca, cb;
		for (K k : hb.keySet()) {
			if (ha.containsKey(k)) {
				if ((ca = ha.get(k)) > (cb = hb.get(k)))
					count += cb;
				else
					count += ca;
			}
		}

		return count;
	}

	/** Spocte pocet vyskytu znaku v retezci. */
	protected static Hashtable<Character, Integer> hashCountChars(String s, int length)
	{
		Hashtable<Character, Integer> h = new Hashtable<Character, Integer>();

		for (int i=0; i<length; i++) {
			char c = s.charAt(i);
			if (h.containsKey(c))
				h.put(c, h.get(c)+1);
			else
				h.put(c, 1);
		}
		return h;
	}
}
