extern int blk;
extern fsub();
int main(){
blk = 1;
printf("blk=%d\n", blk);
fsub();
printf("blk=%d\n", blk);
return 0;
}
