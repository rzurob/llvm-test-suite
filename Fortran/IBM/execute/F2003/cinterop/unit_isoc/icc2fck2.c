#if (defined(_AIX51) || defined(_AIX43))
#include <sys/types.h>
#else
#include <stdint.h>
#endif
#define ARGTYPE int64_t
#define DEREF *
#define TDEF_STMT typedef ARGTYPE DEREF TDEF_NAME
#define TARGET_DECL 
#define MALLOC_CALL arg = malloc(sizeof(arg))
#define FREE_CALL free(arg)
#define VALUE 42
#include "icc2fc.h"
