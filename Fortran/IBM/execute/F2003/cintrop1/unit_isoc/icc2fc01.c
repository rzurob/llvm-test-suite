#if (defined(_AIX51) || defined(_AIX43))
#define true ((unsigned char)1)
#else
#include <stdbool.h>
#endif
#define ARGTYPE _Bool
#define DEREF 
#define TDEF_STMT typedef ARGTYPE DEREF TDEF_NAME
#define TARGET_DECL 
#define MALLOC_CALL 
#define FREE_CALL 
#define VALUE true
#include "icc2fc.h"
