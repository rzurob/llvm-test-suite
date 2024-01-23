extern int ca[2];
extern float caa[3][2];
extern float caaa[1][2][3];
void csubext_arr()
{
 printf(" ca[0]=%d\n", ca[0]);
 printf(" caa[0][0]=%f\n", caa[0][0]);
 printf(" caaa[0][0][0]=%f\n", caaa[0][0][0]);
 ca[0] = 11;
 caa[0][0] = 22;
 caaa[0][0][0] = 33;
 printf(" ca[0]=%d\n", ca[0]);
 printf(" caa[0][0]=%f\n", caa[0][0]);
 printf(" caaa[0][0][0]=%f\n", caaa[0][0][0]);
 return;
}
