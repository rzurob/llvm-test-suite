extern struct str{
int x;
}blk;
extern fsub();
int main(){
blk.x = 1;
printf("blk.x=%d\n", blk.x);
fsub();
printf("blk.x=%d\n", blk.x);
return 0;
}
