#if (defined(_AIX51) || defined(_AIX43))
#include <sys/types.h>
#else
#include <stdint.h>
#endif
#define ARGNAME arg
#define ARGTYPE int8_t
#define COMPARISON ARGNAME == 42
#include "icf2cc.h"
