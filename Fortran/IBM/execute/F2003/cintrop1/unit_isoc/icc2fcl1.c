#if (defined(_AIX51) || defined(_AIX43))
#include <sys/types.h>
#else
#include <stdint.h>
#endif
#define ARGTYPE int_least8_t
#define DEREF 
#define TDEF_STMT typedef ARGTYPE DEREF TDEF_NAME
#define TARGET_DECL 
#define MALLOC_CALL 
#define FREE_CALL 
#define VALUE 42
#include "icc2fc.h"
