/* C code for testcase fxbind_c04wwe.f */

#include <stdio.h>
#include <stdlib.h>
#include <assert.h>

typedef struct people
{
  float  weight;
  int  age;
  char  sex;
} mycperson;

float stats(mycperson  info[3]  ,  int * n )

{
  
  FILE *file;
  
  char c;  
  /* make sure it is large enough to hold all the data! */
  int items,i,j;
  float sum = 0.0;
  j = *n;

  file = fopen("fxbind_c04wwe.dat", "r");

  if(file==NULL) {
    return 1;
  }
 
  for (j = 0; j < 3; j++)
    {
      items = fscanf(file, "%f %d %c", &info[j].weight, &info[j].age, &info[j].sex);
      assert (items ==3);
      sum += info[j].weight; 
    }
  return sum;   
}
