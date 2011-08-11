#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>

// nejake prefixove stromy TODO zkusit
// http://ksp.mff.cuni.cz/tasks/19/tasks2.html#task2


typedef struct SVrchol {
	char znak;
	struct SVrchol *jiny;
	struct SVrchol *dalsi;
	int konec_slova;
} Vrchol;

Vrchol *koren;

/*
Vrchol *vnor_se(Vrchol *v, char *slovo)
{
	while (v->dalsi != NULL && *slovo != '\0') {
		Vrchol *akt_v = v->dalsi;

		while (akt_v->znak != *slovo && akt_v->jiny != NULL) {
			akt_v = akt_v->jiny;
		}

		if (akt_v->znak == *slovo) {
			v = akt_v;
			*slovo++;
		} else {
			break;
		}
	}
}
*/

int vloz_slovo(char *slovo)
{
	Vrchol *v = koren;

	while (v->dalsi != NULL && *slovo != '\0') {
		Vrchol *akt_v = v->dalsi;

		while (akt_v->znak != *slovo && akt_v->jiny != NULL) {
			akt_v = akt_v->jiny;
		}

		if (akt_v->znak == *slovo) {
			v = akt_v;
			slovo++;
		} else {
			akt_v->jiny = malloc(sizeof(Vrchol));
			v = akt_v->jiny;
			v->znak = *slovo;
			slovo++;
			break;
		}
	}

	for (; *slovo != '\0'; slovo++) {
		v->dalsi = malloc(sizeof(Vrchol));
		v = v->dalsi;
		v->znak = *slovo;
	}

	v->konec_slova = true;
	return 1;
}


int je_vlozeno_slovo(char *slovo)
{
	Vrchol *v = koren;

	while (v->dalsi != NULL && *slovo != '\0') {
		Vrchol *akt_v = v->dalsi;

		while (akt_v->znak != *slovo && akt_v->jiny != NULL) {
			akt_v = akt_v->jiny;
		}

		if (akt_v->znak == *slovo) {
			v = akt_v;
			slovo++;
		} else {
			break;
		}
	}

	return (*slovo == '\0' && v->konec_slova);
}


int zobraz_vrchol(Vrchol *v, int zanoreni)
{
	if (v->znak != '\0') {
		if (v->konec_slova)
			printf("[%c]", v->znak);
		else
			printf(" %c ", v->znak);
	}

	if (v->dalsi !=NULL) {
		zobraz_vrchol(v->dalsi, zanoreni+1);
	}

	if (v->jiny != NULL) {
		printf("\n");
		for (int i=1; i<zanoreni; i++) printf("   ");
		zobraz_vrchol(v->jiny, zanoreni);
	}

	return 0;
}


int main(int argc, char *argv[])
{
	koren = malloc(sizeof(Vrchol));
	// koren->znak = '>';
	

	vloz_slovo("ahoj");
	vloz_slovo("aj");
	vloz_slovo("aa");
	vloz_slovo("agae");
	vloz_slovo("agz");
	vloz_slovo("zafaz");
	vloz_slovo("zagz");
	vloz_slovo("jablicko");
	vloz_slovo("karkulka");

	printf("%i", je_vlozeno_slovo("ahoj"));
	printf("%i\n", je_vlozeno_slovo("karkulk"));

	zobraz_vrchol(koren, 0);
	printf("\n\n");
}
