struct str{
int x;
float y;
char z[1];
} bar;

void csub(){
printf("bar.x=%d\n", bar.x);
printf("bar.y=%f\n", bar.y);
printf("bar.z[0]=%c\n", bar.z[0]);

bar.x += 1;
bar.y += 2.0;
bar.z[0] = 'C';

printf("bar.x=%d\n", bar.x);
printf("bar.y=%f\n", bar.y);
printf("bar.z[0]=%c\n", bar.z[0]);
return;
}
