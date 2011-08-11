import java.util.Hashtable;
import yadb.*;
import java.io.*;
import java.net.*;

/**
 * YADB, neboli Yet Another DataBase je jednoducha databaze vyuzivajici jako
 * hlavni datovou strukturu R-Stromy.
 *
 * <p>Program je koncipovan jako databazovy server, tak ze kazde vlakno obsluhuje
 * jeden pozadavek. Jednoduse vzdy nejdrive rozparsuje vstup a pak provede
 * prislusnou akci.</p>
 *
 * <p>Ve tridach yadb.RTree a yadb.Node je vse spojeno s R-Stromy, ve tride
 * yadb.Parser jsou pomocne metody pro parsovani konkretnich casti vstupu.
 * Konecne ve tride yadb.Connection je zakotvena obsluha jednotlivych pripojeni
 * a jejich rizeni (naparsovani vstupu, provedeni prikazu, odpoved serveru). Ve
 * zbylych tridach jsou uz jen pomocne struktury.</p>
 *
 * @author Tomas Jakl (jacke.lee@volny.cz)
 */
public class YADB
{
	public static void main(String[] args)
	{
		int PORT;
		ServerSocket serverSocket = null;

		if (args.length < 1) {
			System.err.println("Usage: PORT");
			System.exit(1);
		}

		try {
			PORT = new Integer(args[0]);

			serverSocket = new ServerSocket(PORT);
		} catch (IOException e) {
			System.err.println("Cannot create server socket.");
			System.exit(1);
		} catch (Exception e) {
			System.err.println("Wrong arguments.");
			System.exit(1);
		}

		while (true) {
			try {
				Socket clientSocket = serverSocket.accept();
				new Connection(clientSocket).start();
			} catch (IOException e) {
				System.err.println("Connecting error");
			}
		}
	}
}
