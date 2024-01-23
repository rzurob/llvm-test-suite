




#define ARGTYPE long double
#define DEREF *
#define TDEF_STMT typedef ARGTYPE DEREF TDEF_NAME
#define TARGET_DECL 
#define MALLOC_CALL arg = malloc(sizeof(ARGTYPE))
#define FREE_CALL free(arg)
#define VALUE 42.0l
#include "icc2fc.h"
