




#define ARGTYPE signed char
#define DEREF *
#define TDEF_STMT typedef ARGTYPE DEREF TDEF_NAME
#define TARGET_DECL 
#define MALLOC_CALL arg = malloc(sizeof(arg))
#define FREE_CALL free(arg)
#define VALUE 42
#include "icc2fc.h"