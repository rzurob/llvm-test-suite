void csubext_val(int ci_val, float cf_val, char cc_val)
{
 printf(" ci_val=%d\n", ci_val);
 printf(" cf_val=%f\n", cf_val);
 printf(" cc_val=%c\n", cc_val);
 ci_val = 11;
 cf_val = 22;
 cc_val = 'C';
 printf(" ci_val=%d\n", ci_val);
 printf(" cf_val=%f\n", cf_val);
 printf(" cc_val=%c\n", cc_val);
 return;
}
void csubext_ref(int *ci_ref, float *cf_ref, char *cc_ref, int *ca_ref)
{
 printf(" *ci_ref=%d\n", *ci_ref);
 printf(" *cf_ref=%f\n", *cf_ref);
 printf(" *cc_ref=%c\n", *cc_ref);
 printf(" *ca_ref=%d\n", *ca_ref);
 *ci_ref = 11;
 *cf_ref = 22;
 *cc_ref = 'C';
 *ca_ref = 33;
 printf(" *ci_ref=%d\n", *ci_ref);
 printf(" *cf_ref=%f\n", *cf_ref);
 printf(" *cc_ref=%c\n", *cc_ref);
 printf(" *ca_ref=%d\n", *ca_ref);
 return;
}
