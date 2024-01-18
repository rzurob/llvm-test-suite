#include<stdio.h>

int rwfunc() {
  FILE *pfin,*pout;
  int v_int;
  double v_double;
  char v_string[2];

  pfin=fopen("infile","rb");
  if(pfin==NULL) {
    printf("Error: cant open file.\n");
    return 1;
  }

  pout=fopen("outfile","wb"); 

  if(pout==NULL) {
    printf("Error: cant create file.\n");
    return 1;
  }

  fscanf(pfin,"%d", &v_int);
  fscanf(pfin,"%lf", &v_double);
  fscanf(pfin,"%s", &v_string);

  v_int+=100;
  v_double = v_double * 2;
  v_string[0]++;
  v_string[1]++;

  fprintf(pout,"%d\n", v_int);
  fprintf(pout,"%f\n", v_double);
  fprintf(pout,"%s\n", v_string);

  fclose (pfin);
  fclose (pout);
}
