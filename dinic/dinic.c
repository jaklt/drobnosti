#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <limits.h>

#define min(a,b) ((a) < (b) ? (a) : (b))
#define TO(edge, n) ((edge)->from == (n) ? (edge)->to : (edge)->from)
#define FROM(edge, n) ((edge)->from == (n) ? (edge)->from : (edge)->to)
#define E_RES(edge, n) ((edge)->from == (n) ? (edge)->capac - (edge)->flow : (edge)->flow)
#define E_DEL(edges, del, max) { (max)--; (edges)[del] = (edges)[max]; }
#define E_ADD(e, n, m) {if ((e)->from == (n)) (e)->flow += (m); else (e)->flow -= (m);}

struct Edge {
	int from;
	int to;
	int capac;
	int flow;
};

// graf
int nodes_c;  // pocet vrcholu
int edges_c;  // počet hran
struct Edge *G;

// pomocne datove struktury
struct Edge ***e_out;
int **e_in;
int *akt_out, *akt_in, *tmps;

int *FILO;
struct Edge *e;


void print_graph()
{
	for (int i=0; i<nodes_c; i++) {
		printf("\nfrom %i\n", i);
		for (int j=0; j<akt_out[i] && (e = e_out[i][j]); j++)
			printf(" to %i - capacity %i / used %i\n",
					e->to, e->capac, e->flow);
	}
	printf("\n");
}


/**
 * Vytvori sit rezerv, do e_out[i] ulozi hrany vedouci z i-teho vrcholu a v
 * akt_out[i] bude jejich pocet.
 *
 * @param b pripravit pro vypis
 */
void make_residual_graph(int b)
{
	memset(akt_out,  0, nodes_c * sizeof(int));
	for (int i=0; i<edges_c; i++) {
		if (G[i].capac - G[i].flow || b)
			e_out[G[i].from][akt_out[G[i].from]++] = &G[i];
		if (G[i].flow && !b)
			e_out[G[i].to][akt_out[G[i].to]++] = &G[i];
	}
}


int dinic(int s, int t)
{
	int fi, ft; // FILO_i, FILO_top
	int found, mark;
	int to;
	int SOURCE = s, SINK = t;

	while (1) {
		// sestrojeni site rezerv
		make_residual_graph(0);

		// najiti nejkratsi st-cesty + procisteni site od zbytecnych hran
		memset(tmps, -1, nodes_c * sizeof(int)); // nenavstiveny vrchol ma tmps -1
		FILO[fi = 0] = SOURCE; // inicializace fronty
		tmps[SOURCE] = 1;
		found = mark = 0;
		ft = 1;

		while (fi >= 0 && fi < ft) {
			if (found && found < tmps[FILO[fi]]) { fi--; mark = 1; continue; }
			if (mark && found != tmps[FILO[fi]]) break; // cisti se jen posledni uroven

			for (int i=0, n=FILO[fi]; i<akt_out[n]; i++) {
				to = TO(e_out[n][i], n);

				if (to == SINK) {
					if (found) continue;
					found = tmps[n];
					tmps[SINK] = found+1;
					FILO[ft++] = SINK;
				} else if (!found && tmps[to] == -1) {
					tmps[to] = tmps[n]+1;
					FILO[ft++] = to;
				} else if (mark || tmps[to] <= tmps[n]) {
					// mazani hran zpetnych/v ramci vrstev/dal nez stok
					E_DEL(e_out[n], i, akt_out[n]);
					i--;
				}
			}

			if (mark) fi--;
			else fi++;
		}

		// nebyla nalezena zadna nejkratsi st-cesta? -> konec
		if (mark == 0) break;

		// docisteni vrcholu nuloveho stupne
		memset(akt_in,   0, nodes_c * sizeof(int));
		fi = ft = 0;

		for (int i=0; i<nodes_c; i++) {
			if (tmps[i] == -1) continue;
			if (akt_out[i] == 0) FILO[ft++] = i;

			// premeni tmps na vystupni stupen
			tmps[i] = akt_out[i];

			// vytvoreni zptenych "odkazu"
			for (int j=0; j<akt_out[i]; j++) {
				to = TO(e_out[i][j], i);
				e_in[to][akt_in[to]++] = i;
			}
		}

		while (fi < ft) {
			if (FILO[fi] == SINK) { fi++; continue; }
			for (int i=0, n=FILO[fi]; i<akt_in[n]; i++) {
				if ((--tmps[e_in[n][i]]) == 0)
					FILO[ft++] = e_in[n][i];
			}

			tmps[FILO[fi]] = -1;
			fi++;
		}

		// najiti blokujiciho toku
		// pozn: odted plati, ze akt_in[i] == kolik tece i-tym vrcholem
		//       a tmps[i] == kolik muze i-tym tyct (nebo -1)
		//       mark znaci kolik muze tyct nebo kolik odteklo
		memset(akt_in,   0, nodes_c * sizeof(int));
		tmps[SOURCE] = mark = INT_MAX;
		FILO[fi = 0] = SOURCE;
		found = 0;

		while (fi >= 0) {
			if (FILO[fi] == SINK) { found = mark; fi--; continue; }

			for (int n=FILO[fi]; 0<akt_out[n];) {
				e = e_out[n][0];
				to = TO(e, n);

				// asi budeme plnit/mazat hranu
				if (found || tmps[to] == -1 || E_RES(e, n) == 0) {
					mark = found ? mark : akt_in[to];
					E_ADD(e, n, mark);
					akt_in[n] += mark;
					mark = tmps[n] - akt_in[n];

					// mazeme nasycene a zbytecne hrany
					if (tmps[to] == -1 || E_RES(e, n) == 0) {
						akt_in[to] = 0;
						E_DEL(e_out[n], 0, akt_out[n]);
					}

					if (mark && akt_out[n]) {
						found = 0;
					} else {
						found = mark = akt_in[n];
						break;
					}

				// pokracujeme dal smer SINK
				} else {
					tmps[to] = mark = min(mark, E_RES(e, n));;
					akt_in[to] = 0;
					FILO[fi+1] = to;
					break;
				}
			}

			// cisteni vrcholu bez odchozich hran
			if (akt_out[FILO[fi]] == 0) {
				tmps[FILO[fi]] = -1;
				fi--;
				continue;
			}

			if (found) fi--;
			else fi++;
		}
	}

	// Zjisteni velikosti toku a jeho vynulovani
	found = 0;
	for (int i=0; i<edges_c; i++) {
		if (G[i].from == SOURCE) found += G[i].flow;
		if (G[i].to   == SOURCE) found -= G[i].flow;
		G[i].flow = 0;
	}

	return found;
}


/**
 * Format vstupu:
 * na zacatek dve cisla - "pocet_vrcholu pocet_hran"
 * nasleduje seznam hran ve formatu - "odkud kam"
 */
void load_graph()
{
	int from, to;

	scanf("%i %i", &nodes_c, &edges_c);
	if (nodes_c <= 0 || edges_c < 0
			|| !(G = malloc(sizeof(struct Edge) * edges_c))
			|| !(akt_out = malloc(sizeof(int) *4* nodes_c))
			|| !(e_in    = malloc(sizeof(int *) * nodes_c))
			|| !(e_out = malloc(sizeof(struct Edge **) * nodes_c))
			|| !(e_out[0] = malloc(sizeof(struct Edge *) * 2 * edges_c))
			|| !(e_in[0]  = malloc(sizeof(int) * 2 * edges_c)))
		exit(1);

	akt_in  = akt_out + nodes_c;
	tmps = akt_in + nodes_c;
	FILO = tmps + nodes_c;

	for (int i=0; i<edges_c; i++) {
		scanf("%i %i", &from, &to);

		if (from < 0 || from >= nodes_c || to < 0 || to >= nodes_c) {
			fprintf(stderr, "%i %i je neplatny vstup\n", from, to);
			exit(1);
		}

		G[i].from = from;
		G[i].to = to;
		G[i].capac = 1;
		G[i].flow = 0;
		akt_out[from]++; akt_out[to]++;
	}

	for (int i=1; i<nodes_c; i++) {
		e_out[i] = e_out[i-1] + akt_out[i-1];
		e_in[i]  = e_in[i-1] + akt_out[i-1];
	}
}


int main(int argc, char *argv[])
{
	load_graph();

	int a, b, minimum = edges_c;

	for (int i=1; i<nodes_c; i++) {
		a = dinic(0, i);
		b = dinic(i, 0);
		minimum = min(minimum, min(a, b));
		break;
	}

	printf("Hranova souvislost je %i\n", minimum);

//	Testovani
//	make_residual_graph(1);
//	print_graph();
	return 0;
}
