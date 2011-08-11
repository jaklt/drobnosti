#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>


void zobrazeni(int *data, int len)
{
	for (int i=0; i<len; i++)
		if (data[i] < 10)
			printf(" %d ", data[i]);
		else
			printf("%d ", data[i]);

	printf("\n");
}


int *make_heap(int *data, int len)
{
	for (int i=0; i<len; i++) {
		if (i == 0) continue;
		int p = i;
		int h = p - p/2 -1;

		while (data[h] < data[p]) {
			int pom = data[p];
			data[p] = data[h];
			data[h] = pom;

			p = h;
			h = p - p/2 -1;
			if (h < 0) break;
		}
	}

	return data;
}


int *delete_heap(int *data, int len)
{
	int pom;

	for (int i=0; i<len-1; i++) {
		pom = data[0];
		data[0] = data[len-i-1];
		data[len-i-1] = pom;

		int p = 1, j = 0;

		do {
			if (p >= len-i-1) break;
			if (data[p] < data[p+1] && p+1 < len-i-1) p++;

			pom = data[p];
			data[p] = data[j];
			data[j] = pom;

			j = p;
			p = p*2+1;
		} while(data[j] < data[p]);
	}

	return data;
}


void heap_sort(int *data, int len)
{
	make_heap(data, len);
	delete_heap(data, len);
}


int main(int argc, char *argv[])
{
	int data[] = {3, 1, 2, 0, 4, 12, 7, 8, 31, 4, 5};
	int len = sizeof(data)/sizeof(int);

	zobrazeni(data, len);

	heap_sort(data, len);
	zobrazeni(data, len);
}

