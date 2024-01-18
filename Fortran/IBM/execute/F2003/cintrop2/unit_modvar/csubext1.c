extern int ci;
extern float cf;
extern int ca[2];
extern char cc;
void csubext()
{
 printf(" ci=%d\n", ci);
 printf(" cf=%f\n", cf);
 printf(" ca[0]=%d\n", ca[0]);
 printf(" cc=%c\n", cc);
 ci = 11;
 cf = 22;
 ca[0] = 33;
 cc='C';
 printf(" ci=%d\n", ci);
 printf(" cf=%f\n", cf);
 printf(" ca[0]=%d\n", ca[0]);
 printf(" cc=%c\n", cc);
 return;
}
