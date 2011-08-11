#include <stdio.h>
#include <stdlib.h>

struct node {
	struct node *l, *r;
	int weight;
	int value;
};


#define get_weight(n) (n ? n->weight : 0)


struct node *tree_to_list(struct node *tree, struct node *rest)
{
	if (!tree) return rest;
	tree->r = tree_to_list(tree->r, rest);

	return tree_to_list(tree->l, tree);
}


struct node *list_to_tree(struct node **list, int count)
{
	struct node *n, *l;
	if (count == 0) return NULL;

	l = list_to_tree(list, count/2);

	n = *list;
	n->l = l;
	*list = n->r;
	
	n->r = list_to_tree(list, count - 1 - count/2);

	n->weight = count;
	return n;
}


struct node *balanced_tree(struct node *n)
{
	if (n->weight > 2
			&& (get_weight(n->l) > 2*get_weight(n->r)
				|| 2*get_weight(n->l) < get_weight(n->r)))
	{
		// rebuild
		int weight = n->weight;

		n = tree_to_list(n, NULL);
		n = list_to_tree(&n, weight);
	}

	return n;
}


struct node *insert(struct node *n, int value, int *inserted)
{
	if (!n) {
		struct node *new = malloc(sizeof(struct node));
		new->value = value;
		new->weight = 1;
		new->l = NULL; new->r = NULL;
		*inserted = 1;
		return new;
	}

	if (n->value == value) {
		*inserted = 0;
		return n;
	}

	if (n->value < value)
		n->r = insert(n->r, value, inserted);
	else
		n->l = insert(n->l, value, inserted);

	if (*inserted) n->weight++;
	return balanced_tree(n);
}


struct node *delete(struct node *n, int value, struct node **deleted)
{
	if (!n) {
		*deleted = NULL;
		return NULL;
	}

	if (n->value == value) {
		*deleted = n;

		struct node *new_root, *tmp;

		if (!n->l) return n->r;
		if (!n->r) return n->l;

		tmp = n->l;
		while (tmp->r) tmp = tmp->r;
		tmp = delete(n->l, tmp->value, &new_root);

		new_root->l = tmp;
		new_root->r = n->r;
		return new_root;

/* /
		new_root = n->l;
		tmp = new_root;

		while (new_root->r) {
			tmp = new_root;
			new_root = new_root->r;
		}

		tmp->r = new_root->l;
		new_root->l = tmp;
		new_root->r = n->r;
		return new_root;
*/
	}

	if (n->value < value)
		n->r = delete(n->r, value, deleted);
	else
		n->l = delete(n->l, value, deleted);

	if (*deleted) n->weight--;
	return balanced_tree(n);
}


void dump(struct node *n)
{
	if (!n) { printf("."); return; }

	printf("("); dump(n->l);
	printf(" %d ", n->value);
	dump(n->r); printf(")");
}


#define SIZE (sizeof(values)/sizeof(int))
int main(int argc, char *argv[])
{
	int values[] = { 10, 5, 15, 3, 1, 2, 12, 13, 4 };
	int inserted = 0;
	struct node *n = NULL, *deleted;

	for (int i=0; i<SIZE; i++) {
		n = insert(n, values[i], &inserted);
		n = insert(n, i, &inserted);
	}

	dump(n);
	printf("\n");

	for (int i=0; i<SIZE/2; i++) {
		n = delete(n, values[i], &deleted);
	}

	dump(n);
	printf("\n");

	return 0;
}
