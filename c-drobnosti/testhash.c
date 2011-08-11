#include <stdio.h>
#include <string.h>

#include "hashovani.h"
#define LINE "-------------------------------------\n"

int vypis_hash(Hash *h)
{

	for (int i=0; i<(h->size); i++) {
		if (h->hashes[i].full != FULL_HASH)
			printf("%3i\t --prazdny--\n", i);
		else
			printf("%3i\t%s\t\t(%lu)\n", i, h->hashes[i].name, h->hashes[i].hash);
	}

	printf("%d used, %d size\n", h->used, h->size);
	return 0;
}


int main(int argc, char *argv[])
{
	#define N 36
	char *slova[] = {
		"static",  "cokfldl", "pus", "min", "vaas", "dfda", "d", "adds", "fdtt",
		"static",  "coskfldl", "ps", "mvin", "vahas", "dfdqa", "ad", "adds", "fdtnt",
		"skvtic",  "cokfsldl", "pus", "miln", "vaaos", "dfada", "di", "aadds", "fdtoot",
		"static",  "call", "plus", "main", "vavas", "dafda", "da", "adfds", "fsdtt"
	};

	Hash *h = new_Hash();
	HashMember *hm;
	Hash *hc;

	for (int i=0; i<N; i++) {
		if (i == 8) hc = clone_Hash(h);
		if (add_string_Hash(h, slova[i], NULL) == NULL) printf("--CHYBA--\n");
	}

	vypis_hash(h);
	printf(LINE);

	HashMember *prvek = get_string_Hash(h, "mvin");
	if (prvek) {
		printf("%s\n", prvek->name);
		printf("%lu\n", prvek->hash);
	} else printf("--CHYBA--\n");
	printf(LINE);


	for (int i=0; i<N/2; i++) {
		if ((hm = del_string_Hash(h, slova[i])) == NULL) printf("--CHYBA-- (%s)\n", slova[i]);
	//	else printf("%s deleted\n", hm->name);
	}
	vypis_hash(h);
	printf(LINE);

	printf("%d\n", strlen("aaaa"));

	printf(LINE);
	free_Hash(hc);
	free_Hash(h);
	return 0;
}
