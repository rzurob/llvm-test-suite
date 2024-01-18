#if (defined(_AIX51) || defined(_AIX43))
typedef short int_fast16_t;
#else
#include <stdint.h>
#endif
#define ARGNAME arg
#define ARGTYPE int_fast16_t*
#define COMPARISON *ARGNAME == 42
#include "icf2cc.h"
