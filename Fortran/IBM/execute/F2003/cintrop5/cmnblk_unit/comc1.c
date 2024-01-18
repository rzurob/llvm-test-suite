#include <stdio.h>
#include <string.h>

struct str{
int x;
float y;
char z;
} blk;

void csub(){
printf("blk.x=%d\n", blk.x);
printf("blk.y=%f\n", blk.y);
printf("blk.z=%c\n", blk.z);

blk.x += 1;
blk.y += 2.0;
blk.z = 'C';

printf("blk.x=%d\n", blk.x);
printf("blk.y=%f\n", blk.y);
printf("blk.z=%c\n", blk.z);
return;
}
