#include <stdio.h>

int realfunc(int);
void c_func_1(int  (*pt2Func)(int ));
void c_func_2(int  (*pt2Func)(int ));

void c_func_1(int  (*pt2Func)(int ))
{
    if (pt2Func != NULL) {
	printf ("pt2Func = present in c_func_1\n");
        c_func_2(pt2Func);
    }
    else {
	printf ("pt2Func = absent in c_func_1\n");
    }
}


void c_func_2(int  (*pt2Func)(int ))
{
    int res;
    
    if (pt2Func != NULL) {
        printf ("pt2Func = present in c_func_2\n");
        res = pt2Func(200);
        printf ("pt2Func in c_func_2 returns %d\n", res);
    }
    else
        printf ("pt2Func = absent in c_func_2\n");
   
}


int  realfunc(int a)
{
    if (a != NULL) {
        printf ("a = %d : present in realfunc\n", a);
        return a+1; 
    }
    else {
        printf ("a = absent in realfunc\n");
        return 0;
    }
}

