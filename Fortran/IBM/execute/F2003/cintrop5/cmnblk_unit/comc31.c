extern struct str{
int x;
} blk;
void csub(){
printf("blk.x=%d\n", blk.x);
blk.x += 1;
printf("blk.x=%d\n", blk.x);
return;
}
