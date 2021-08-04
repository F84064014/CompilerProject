#ifndef COMMON_H
#define COMMON_H

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>

#define MAX_SIZE 100
#define INVALID 0
#define VALID 1
#define IsLIT -383

/* struct ... */
typedef struct TABLE_ELEMENT{
	char* Name;
	char* Type;
	int Address;
	int Lineno;
	char* Element_type;
	int Index;
	int scope;
} TABLE;

typedef struct fstack{
	int stack;
	struct fstack* next;
} FStack;

typedef struct post{
	char pstmt[100];
	int mark;
	struct post* next;
} PStack;

#endif /* COMMON_H */
