#if (defined(_AIX51) || defined(_AIX43))
#define true ((unsigned char)1)
#else
#include <stdbool.h>
#endif
#define ARGNAME arg
#define ARGTYPE _Bool
#define COMPARISON ARGNAME == true
#include "icf2cc.h"
