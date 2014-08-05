import java.math.BigDecimal;
import java.util.*;
import java.io.*;
import java.math.MathContext;

public class Calculator
{
	protected static BigDecimal last = new BigDecimal(0);
	protected static BufferedReader br;
	protected static MathContext precision = new MathContext(20);

	protected static void fetchOp(LinkedList<Expr> n, LinkedList<String> o) {
		Expr e = n.removeLast();
		n.add(new Expr(o.removeLast(), n.removeLast(), e));
	}

	public static void main(String[] args) {
		br = new BufferedReader(new InputStreamReader(System.in));
		while (true) {
			LinkedList<Expr>  nums = new LinkedList<Expr>();
			LinkedList<String> ops = new LinkedList<String>();
			try {
				String in = br.readLine();
				int i, j = -1;
				boolean lastNum = true;
				if (in.length() > 10 && in.substring(0, 10).equals("precision ")) {
					precision = new MathContext(Integer.parseInt(in.substring(10)));
					continue;
				}

				while (++j < in.length()) {
					i = j;
					if (Character.isWhitespace(in.charAt(i))) continue;
					if (Character.isLetterOrDigit(in.charAt(j)) || in.charAt(i) == '-'
							&& Character.isLetterOrDigit(in.charAt(j+1)) && !lastNum) {
						while (++j < in.length() && (in.charAt(j) == '.'
								|| Character.isLetterOrDigit(in.charAt(j))));
						nums.add(new Expr(in.substring(i, j), null, null));
						lastNum = true; j--; continue;
					}

					if (in.charAt(i) == ')') {
						while (ops.getLast().charAt(0) != '(') fetchOp(nums, ops);
						ops.removeLast(); continue;
					} else if (ops.size() > 0 && ops.getLast().charAt(0) != '(' &&
							(in.charAt(i) == '+' || in.charAt(i) == '-'))
						fetchOp(nums, ops);

					ops.add(in.substring(i, j+1)); lastNum = false;
				}

				while (ops.size() > 0) fetchOp(nums, ops);
				if (nums.size() > 1) throw new Exception();
				System.out.println((last = nums.getFirst().result()).toPlainString());
			} catch (Exception e) {
				System.out.println("CHYBA");
				last = new BigDecimal(0);
			}
		}
	}

	static class Expr {
		Expr e1, e2;
		String s;

		public Expr(String s, Expr e1, Expr e2) {
			this.s = s; this.e1 = e1; this.e2 = e2;
		}

		public BigDecimal result() throws Exception {
			BigDecimal b1 = null, b2 = null;
			boolean i;
			if ((i = (e1 != null && e2 != null)) == true) {
				b1 = e1.result(); b2 = e2.result();
			}
			if (s.equals("last")) return Calculator.last;
			else if (s.equals("+") && i) return b1.add     (b2, Calculator.precision);
			else if (s.equals("-") && i) return b1.subtract(b2, Calculator.precision);
			else if (s.equals("*") && i) return b1.multiply(b2, Calculator.precision);
			else if (s.equals("/") && i) return b1.divide  (b2, Calculator.precision);
			else return new BigDecimal(s, Calculator.precision);
		}
	}
}
