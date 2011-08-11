package yadb;
import java.util.regex.Pattern;
import java.util.regex.Matcher;
import java.util.regex.PatternSyntaxException;
import java.util.List;
import java.util.LinkedList;

public class Parser
{
	/**
	 * Regularni vyraz parsujici korektni nazev sloupce, cislo (zaporny i typu
	 * real) nebo retezec znaku uvozeny uvozovkami.
	 */
	public static final String token = "('[^']*'|-[0-9]+\\.[0-9]+|[0-9]+\\.[0-9]+|-[0-9]+|\\w+)";
	
	/**
	 * Vytvori seznam podminek prislusejici predanemu WHERE.
	 *
	 * @param t strom, nad kterym pracujeme
	 * @param text text vyrazu, ktery chceme previst na seznam podminek
	 * @return podminky
	 */
	public static Condition[] parseWhere(RTree t, String text)
	{
		text = text.trim();
		if (text.equals("")) return new Condition[0];
		if (!text.substring(0, 5).toUpperCase().equals("WHERE"))
			throw new IllegalArgumentException("Syntax error");

		text = text.trim().substring(5);

		List<Condition> res = new LinkedList<Condition>();
		Pattern p = Pattern.compile(token + "\\s*(<|<=|=|!=|>=|>)\\s*" +token+ "\\s*(\\sAND|$)");
		Matcher m = p.matcher(text);
		String lt, op, rt, end = null;

		while (m.find())
		{
			lt  = getMatched(m, text, 1);
			op  = getMatched(m, text, 2);
			rt  = getMatched(m, text, 3);
			end = getMatched(m, text, 4);

			res.add(Condition.createCondition(t, lt, op, rt));
		}

		if (end == null || end.length() != 0)
			throw new IllegalArgumentException("Syntax error");
		return res.toArray(new Condition[0]);
	}

	/**
	 * Vrati strukturu tabulky, kterou odvodi z textu.
	 *
	 * @param text popis struktury tabulky
	 * @return hotova struktura tabulky
	 */
	public static RTree parseCreate(String text)
	{
		LinkedList<Column> res = new LinkedList<Column>();
		String[] assigns = text.split(",");
		Pattern p = Pattern.compile("\\s*(\\w+)\\s+(INTEGER|REAL|STRING)\\s*$");;
		Matcher m;

		for (String a : assigns) {
			m = matchExpression(p, a);

			String
				colName = getMatched(m, a, 1),
				type    = getMatched(m, a, 2);

			if     (type.equals("INTEGER")) res.addLast(new Column(colName, Type.INTEGER));
			else if (type.equals("STRING")) res.addLast(new Column(colName, Type.STRING));
			else if   (type.equals("REAL")) res.addLast(new Column(colName, Type.REAL));
		}

		return new RTree(res.toArray(new Column[0]));
	}

	/**
	 * Prevede text odpovidajici VALUES insertu na pole hodnot.
	 *
	 * @param dim dimenze tabulky
	 * @param text text k parsovani
	 * @return naparsovane hodnoty v poli
	 */
	public static Comparable[] parseValues(RTree t, String text)
	{
		Comparable[] res  = new Comparable[t.cols.length];
		String[] assigns = text.split(",");
		Pattern p = Pattern.compile(token);
		Matcher m;

		int i=0;
		for (String a : assigns) {
			m = matchExpression(p, a);

			String value = getMatched(m, a, 1);
			res[i++] = parseComparable(value);
		}

		// typova kontrola
		for (i=0; i<t.cols.length; i++) {
			if ((t.cols[i].type == Type.INTEGER && !(res[i] instanceof Integer))
					|| (t.cols[i].type == Type.REAL && !(res[i] instanceof Double))
					|| (t.cols[i].type == Type.STRING && !(res[i] instanceof String)))
				throw new IllegalArgumentException("Invalid type");
		}

		return res;
	}

	/**
	 * Prevede text odpovidajici SET updatu na pole hodnot.
	 *
	 * @param t tabulka, ke ktere se vztahuje parsovani
	 * @param text text k parsovani
	 * @return pole hodnot odpovidajici naparsovanemu textu, null znamena, ze
	 * hodnota s danym indexem nebyla v textu obsazena a tudiz nema byt menena.
	 */
	public static Comparable[] parseSet(RTree t, String text)
	{
		Comparable[] res  = new Comparable[t.cols.length];
		String[] assigns = text.split(",");
		Pattern p = Pattern.compile("\\s*(\\w+)\\s*=\\s*"+token+"\\s*$");;
		Matcher m;

		for (String a : assigns) {
			m = matchExpression(p, a);

			String
				colName = getMatched(m, a, 1),
				value   = getMatched(m, a, 2);

			res[t.records.get(colName)] = parseComparable(value);
		}
		
		return res;
	}

	public static Comparable parseComparable(String s)
	{
		if (s == null) return null;
		char fst = s.charAt(0);
		if (fst == '\'') {
			if (s.charAt(s.length()-1) != '\'') throw new IllegalArgumentException("Parse error");
			return s.substring(1, s.length()-1);
		} else if (fst == '-' || ('0' <= fst && fst <= '9')) {
			Comparable comp;
			if  (s.indexOf('.') != -1) return new  Double(s);
			else                       return new Integer(s);
		} else
			throw new IllegalArgumentException("Parse error");
	}

	public static Matcher matchExpression(String expr, String text)
	{
		return matchExpression(Pattern.compile(expr), text);
	}

	public static Matcher matchExpression(Pattern p, String text)
	{
		Matcher m = p.matcher(text);
		if (!m.find())
			throw new IllegalArgumentException("Syntax error");
		return m;
	}

	public static String getMatched(Matcher m, String text, int index)
	{
		return text.substring(m.start(index), m.end(index));
	}
}
