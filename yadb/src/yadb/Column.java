package yadb;

public class Column
{
	
	/** Jmeno sloupce. */
	public String name;

	/** Typ dat sloupce. */
	public Type type;

	public Column(String name, Type type)
	{
		this.name = name;
		this.type = type;
	}
}
