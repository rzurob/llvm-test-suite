/* C code for testcase "fxbind_c08mfa.f" */

#include <stdio.h>
#include <assert.h>

void init_arr_1d(int *);
/* read_data is implemented in Fortran Code. */
void read_data(int *, int *);

int main()
{ 
  int i;
  int raw_data[5],how_many, ret;
 
  how_many = 5;
  init_arr_1d(raw_data);

  read_data(&raw_data[0],&how_many);

  for ( i = 0; i < how_many; i++ ) {
    assert ( raw_data[i] == i+2 ) ;
      
  }
  return 0;
}

void init_arr_1d(int *x) {
  int i;

  for ( i = 0; i < 5; i++ ) {
    x[i] = 0;
  }

}
