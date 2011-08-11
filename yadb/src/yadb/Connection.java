package yadb;
import java.util.Hashtable;
import yadb.*;
import java.io.*;
import java.net.*;
import java.util.regex.Pattern;
import java.util.regex.Matcher;
import static yadb.Parser.matchExpression;
import static yadb.Parser.getMatched;

public class Connection extends Thread
{
	Socket clientSocket;
	BufferedReader is;
	PrintWriter    os;

	/** Mapovani jmen tabulek na konkretni tabulky. */
	protected static Hashtable<String, RTree> ht = new Hashtable<String, RTree>();

	public Connection(Socket clientSocket)
	{
		this.clientSocket = clientSocket;
	}

	@Override
	public void run() {
		try {
			System.out.println("Connection opened");
			os = new PrintWriter(new OutputStreamWriter(
					clientSocket.getOutputStream()));
			is = new BufferedReader(new InputStreamReader(
					clientSocket.getInputStream()));

			try {
				while (true) {
					String str = is.readLine();
					if (str == null) break;

					try {
						serveInput(str);
						os.println("DONE");
					} catch (IllegalArgumentException e) {
						os.println(e.getMessage());
						System.out.println(e.getMessage());
					} catch (Exception e) {
						os.println("Error occured");
						System.out.println("Error occured");
						e.printStackTrace();
					}
					os.flush();
				}
			} catch (Exception e) {
				e.printStackTrace();
			}
		} catch (IOException e) {
			System.err.println("Connection problem");
		} finally {
			try {
				if (is != null) is.close();
				if (os != null) os.close();
				if (clientSocket != null) clientSocket.close();
			} catch (IOException e) {
				System.err.println("Problem with closing connection");
			}

			System.out.println("Connection closed");
		}
	}

	/**
	 * Ze vstupu rozlisi o jaky typ prikazu jde a provede jej.
	 *
	 * @param str vstupni prikaz
	 */
	public void serveInput(String str)
	{
		String
			a = str.trim().substring(0, 6).toUpperCase(),
			rest = str.substring(6);
		Matcher m;
		boolean isSelect;

		if (a.equals("INSERT")) {
			m = matchExpression("\\s+INTO\\s+([^ ]+)\\s+VALUES\\s*\\((.+)\\)\\s*$", rest);

			String
				table  = getMatched(m, rest, 1),
				values = getMatched(m, rest, 2);

			System.out.println("Inserting (" + values + ") INTO " + table);

			RTree t = ht.get(table);
			t.insert(Parser.parseValues(t, values));
		} else if ((isSelect = a.equals("SELECT")) || a.equals("DELETE")) {
			m = matchExpression("\\s+FROM\\s+([^ ]+)(.*)", rest);

			String
				table = getMatched(m, rest, 1),
				where = getMatched(m, rest, 2);

			RTree t = ht.get(table);
			System.out.println((isSelect ? "Selecting" : "Deleting") + " from " + table + " " + where);
			if (isSelect)
				reportResult(t.search(Parser.parseWhere(t, where)));
			else
				t.delete(Parser.parseWhere(t, where));
		} else if (a.equals("UPDATE")) {
			m = matchExpression("\\s+([^ ]+)\\s+SET\\s+(.+)\\s+(WHERE\\s+.*|$)", rest);

			String
				table = getMatched(m, rest, 1),
				set   = getMatched(m, rest, 2),
				where = getMatched(m, rest, 3);

			RTree t = ht.get(table);
			System.out.println("Updating in " +table+ " " +set+ "; " +where);
			Comparable[][] toUpdate = t.delete(Parser.parseWhere(t, where));
			Comparable[] parsedSet = Parser.parseSet(t, set);

			for (Comparable[] upd : toUpdate) {
				for (int i=0; i<upd.length; i++)
					if (parsedSet[i] != null) upd[i] = parsedSet[i];

				t.insert(upd);
			}
		} else if (a.equals("CREATE")) {
			m = matchExpression("\\s+TABLE\\s+(.+)\\s+\\((.+)\\)\\s*$", rest);

			String
				table = getMatched(m, rest, 1),
				cols  = getMatched(m, rest, 2);

			System.out.println("Creating table " +table+ ": " +cols);
			RTree rt =  Parser.parseCreate(cols);
			ht.put(table, rt);
		} else
			throw new IllegalArgumentException("Syntax error");
	}

	/**
	 * Vypise formatovane vysledek vyhledavani.
	 *
	 * @param res seznam vysledku
	 */
	void reportResult(Comparable[][] res)
	{
		for (Comparable[] cs : res) {
			os.print("  (");
			for (Comparable c : cs)
				os.print(c + " ");
			os.println(")");
		}
	}
}
