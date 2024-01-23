




#define ARGTYPE void *
#define DEREF 
#define TDEF_STMT typedef ARGTYPE DEREF TDEF_NAME
#define TARGET_DECL int targ = 42
#define MALLOC_CALL 
#define FREE_CALL 
#define VALUE &targ
#include "icc2fc.h"
