package yadb;

public class Condition
{
	/**
	 * Levy a pravy index dimenze podminky. Pouzije se, pokud je prislusejici
	 * bound nullova.
	 */
	int indexL, indexR;

	/** Konkretni leva a prava mez. */
	Comparable boundL, boundR;

	/** Typ relace. */
	Relation relation;

	public enum Relation {
		EQ, NE, LT, LE, GT, GE;
	}

	/**
	 * Z danych informaci vytvori podminku.
	 *
	 * @param t tabulka, ve ktere se podminka bude overovat
	 * @param lt leva cast podminky
	 * @param op relace
	 * @param rt prava cast podminky
	 * @return pripravena podminka
	 */
	public static Condition createCondition(RTree t, String lt, String op, String rt)
	{
		Condition c = new Condition();

		// rozpoznani typu relace
		if      (op.equals("<"))  c.relation = Relation.GT;
		else if (op.equals("<=")) c.relation = Relation.GE;
		else if (op.equals("="))  c.relation = Relation.EQ;
		else if (op.equals("!=")) c.relation = Relation.NE;
		else if (op.equals(">=")) c.relation = Relation.LE;
		else if (op.equals(">"))  c.relation = Relation.LT;
		else
			throw new IllegalArgumentException();

		// napastaveni prvniho a druheho clenu relace
		parseBound(t, c, lt, true);
		parseBound(t, c, rt, false);

		return c;
	}

	/**
	 * Rozpozna typ meze v podmince. Vysledek ulozi na spravne misto do
	 * sestavovane podminky v parametru.
	 *
	 * @param t tabulka, ve ktere se podminka bude overovat
	 * @param c sestavovana podminka
	 * @param s vyraz, ze ktereho se pozna o jaky typ objektu se jedna
	 * @param left jedna se o podminku na leve strane vyrazu? (jinak je na
	 * prave strane)
	 */
	protected static void parseBound(RTree t, Condition c, String s, boolean left)
	{
		if (left) c.indexL = -1;
		else      c.indexR = -1;
		char fst = s.charAt(0);

		if (fst == '\'') {
			if (s.charAt(s.length()-1) != '\'') throw new IllegalArgumentException();
			if (left) c.boundL = s.substring(1, s.length()-1);
			else      c.boundR = s.substring(1, s.length()-1);
		} else if (fst == '-' || ('0' <= fst && fst <= '9')) {
			Comparable comp;
			if  (s.indexOf('.') != -1) comp = new  Double(s);
			else                       comp = new Integer(s);

			if (left) c.boundL = comp;
			else      c.boundR = comp;
		} else {
			int i = t.records.get(s);
			if (left) c.indexL = i;
			else      c.indexR = i;
		}
	}

	/**
	 * Zjisti zda dany node vyhovuje teto podmince.
	 *
	 * @param n node na vyzkouseni
	 * @return nase podminka souhlasi s nodem
	 */
	public boolean conditionOK(Node n)
	{
		if (n == null) return false;
		Comparable l = boundL, r = boundR;

		// vyber mezi intervalu, ktere se budou porovnavat
		switch (relation) {
			case NE:
				if (!n.isLeaf) return true;
			case GT:
			case GE:
				l = boundL != null ? boundL : n.getBoundD(indexL);
				r = boundR != null ? boundR : n.getBoundU(indexR);
				break;
			case EQ:
				if (n.isLeaf) {
					l = boundL != null ? boundL : n.getBoundD(indexL);
					r = boundR != null ? boundR : n.getBoundU(indexR);
				} else { // intervaly se alespon trochu prekryvaji
					return (boundL != null ? boundL : n.getBoundD(indexL)).compareTo(boundR != null ? boundR : n.getBoundU(indexR)) <= 0
						|| (boundL != null ? boundL : n.getBoundU(indexL)).compareTo(boundR != null ? boundR : n.getBoundD(indexR)) >= 0;
				}
				break;
			case LE:
			case LT:
				l = boundL != null ? boundL : n.getBoundU(indexL);
				r = boundR != null ? boundR : n.getBoundD(indexR);
				break;
		}

		int cmp = l.compareTo(r);
		switch (relation) {
			case GT: return cmp <  0;
			case GE: return cmp <= 0;
			case NE: return cmp != 0;
			case EQ: return cmp == 0;
			case LE: return cmp >= 0;
			case LT: return cmp >  0;
		}

		return true;
	}
}
