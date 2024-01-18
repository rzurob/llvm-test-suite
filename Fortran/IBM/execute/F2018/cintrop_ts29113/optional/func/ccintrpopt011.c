#include <stdlib.h>

char * realfunc(char *);
void sub_testf_1(char *, char * (*pt2Func)(char *));
void sub_testf_2(char *, char * (*pt2Func)(char *));

int main(int argc, char ** argv)
{   
    char cc;

    sub_testf_1(&cc, NULL);
    sub_testf_2(&cc, NULL);

    sub_testf_1(&cc, realfunc);
    sub_testf_2(&cc, realfunc);

    return 0;
}

