extern struct str{
  int x;
  float y;
  char z;
} blk;

extern fsub();

int main(){
  blk.x = 1;
  blk.y = 2.0;
  blk.z = 'C';

  printf("blk.x=%d\n", blk.x);
  printf("blk.y=%f\n", blk.y);
  printf("blk.z=%c\n", blk.z);

  fsub();

  printf("blk.x=%d\n", blk.x);
  printf("blk.y=%f\n", blk.y);
  printf("blk.z=%c\n", blk.z);
  return 0;
}
