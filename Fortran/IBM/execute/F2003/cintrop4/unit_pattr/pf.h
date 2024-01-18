extern int pf (char *fmt, ...);  /* (Print and Flush) */

/*
** Convert text argument supplied by macro into a string:
**   #define X=1
**   str(X) ==> "1"
*/
#define str2(a) #a
#define str(a) str2(a)
