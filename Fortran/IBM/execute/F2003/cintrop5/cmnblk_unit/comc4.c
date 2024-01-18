extern struct str{
int x[2];
float y[2][3];
} blk;
void csub(){
printf("blk.x[0]=%d\n", blk.x[0]);
printf("blk.y[1][1]=%f\n", blk.y[1][1]);
blk.x[0] += 1;
blk.y[1][1] += 2.0;
printf("blk.x[0]=%d\n", blk.x[0]);
printf("blk.y[1][1]=%f\n", blk.y[1][1]);
return;
}
