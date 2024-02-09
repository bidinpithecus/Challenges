#include <stdio.h>
#include <stdlib.h>

int count_parenthesis(FILE* fptr);
int find_index_of_basement(FILE* fptr);

int count_parenthesis(FILE* fptr) {
    int count = 0;
    char str[10000];
    fgets(str, 10000, fptr);

    for (int i = 0; str[i] != '\0'; i++) {
        if (str[i] == '(') count++;
        else if (str[i] == ')') count--;
        else {
            printf("Unexpected token.\nExiting...\n");
            fclose(fptr);
            exit(-1);
        }
    }

    return count;
}

int find_index_of_basement(FILE* fptr) {
    int count = 0;
    int i;
    char str[10000];
    fgets(str, 10000, fptr);

    for (i = 0; count != -1; i++) {
        if (str[i] == '(') count++;
        else if (str[i] == ')') count--;
        else {
            printf("Unexpected token.\nExiting...\n");
            fclose(fptr);
            exit(-1);
        }
    }

    return i;
}

int main(void) {
    FILE* fptr;
    fptr = fopen("input.txt", "r");

    if (!fptr) {
        printf("Unable to read file.\nExiting...\n");
        fclose(fptr);
        exit(-1);
    }

    printf("Floor %d\n", count_parenthesis(fptr));
    printf("Index %d\n", find_index_of_basement(fptr));

    fclose(fptr);

    return 0;
}
