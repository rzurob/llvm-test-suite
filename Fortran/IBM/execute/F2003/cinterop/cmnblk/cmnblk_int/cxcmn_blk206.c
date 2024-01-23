   #include <inttypes.h>

#include <stdio.h>
#include <stdlib.h>
#include <stddef.h>
#include <string.h>


extern struct {
        signed char             int_s1[2][2][2];
        short                   int_s2[2][2][2];
        int                     int_s4[2][2][2];
        long long               int_s8[2][2][2];
        signed char             int_c_signed_char[2][2][2];
        short                   int_c_short[2][2][2];
        int                     int_c_int[2][2][2];
        long                    int_c_long[2][2][2];
        long long               int_c_long_long[2][2][2];
        size_t                  int_c_size_t[2][2][2];
        intptr_t                int_c_intptr_t[2][2][2];
        intmax_t                int_c_intmax_t[2][2][2];
        int8_t                  int_c_int8_t[2][2][2];
        int16_t                 int_c_int16_t[2][2][2];
        int32_t                 int_c_int32_t[2][2][2];
        int64_t                 int_c_int64_t[2][2][2];
        int_least8_t            int_c_int_least8_t[2][2][2];
        int_least16_t           int_c_int_least16_t[2][2][2];
        int_least32_t           int_c_int_least32_t[2][2][2];
        int_least64_t           int_c_int_least64_t[2][2][2];
        int_fast8_t             int_c_int_fast8_t[2][2][2];
  //      int_fast16_t            int_c_int_fast16_t[2][2][2];
        int_fast32_t            int_c_int_fast32_t[2][2][2];
        int_fast64_t            int_c_int_fast64_t[2][2][2];
} blk_all;




void csub_all(){

int i,j,k,a;


signed char   cmp_s1[2][2][2] =  {127,0,-128,127,127,0,0,-111 };
short         cmp_s2[2][2][2] =  {32767,0,-32768,32767,127,0,-1277,-1 };
int           cmp_s4[2][2][2] =  {2147483647,127,-2147483648ll,0,1000000,-2147483648ll,2147483647,0};   
long long     cmp_s8[2][2][2] =  {-9223372036854775807ll,-9223372036854775807ll,-9223372036854775807ll,-9223372036854775807ll,-9223372036854775807ll,-9223372036854775807ll,-9223372036854775807ll,-9223372036854775807ll };
signed char   cmp_c_signed_char[2][2][2] =  {127,0,-128,127,127,-1,-2,-3 };
short         cmp_c_short[2][2][2] =  {32767,0,-32768,32767,127,-1,-2,-3 };
int           cmp_c_int[2][2][2] =  {2147483647,127,-2147483648ll,0,1000000,-1,-2,-3 };
long          cmp_c_long[2][2][2] =  {2147483647,2147483647,2147483647,2147483647,2147483647,2147483647,2147483647,2147483647 };
long long     cmp_c_long_long[2][2][2] =  {-9223372036854775807LL,-9223372036854775807LL,-9223372036854775807LL,-9223372036854775807LL,-9223372036854775807LL,-9223372036854775807LL,-9223372036854775807LL,-9223372036854775807LL };
size_t        cmp_c_size_t[2][2][2] =  {2147483647,127,-2147483648ll,0,1000000,-1,-2,-3 };
intptr_t      cmp_c_intptr_t[2][2][2] =  {1000000000,1000000000,1000000000,1000000000,1000000000,1000000000,1000000000,1000000000 };
intmax_t      cmp_c_intmax_t[2][2][2] =  {9223372036854775807LL,0,-9223372036854775807LL,1000000,-2147483648ll,-1,-2,-3 };
int8_t        cmp_c_int8_t[2][2][2] =  {-128,-128,-128,-128,-128,-128,-128,-128 };
int16_t       cmp_c_int16_t[2][2][2] =  {-128,-128,-128,-128,-128,-128,-128,-128 };
int32_t       cmp_c_int32_t[2][2][2] =  {2147483647,2147483647,2147483647,2147483647,2147483647,2147483647,2147483647,2147483647 };
int64_t       cmp_c_int64_t[2][2][2] =  {9223372036850123456LL,9223372036850123456LL,9223372036850123456LL,9223372036850123456LL,9223372036850123456LL,9223372036850123456LL,9223372036850123456LL,9223372036850123456LL };
int_least8_t  cmp_c_int_least8_t[2][2][2] =  {-128,-128,-128,-128,-128,-128,-128,-128 };
int_least16_t cmp_c_int_least16_t[2][2][2] =  {32767,0,-32768,32767,127,-1,-2,-3 };
int_least32_t cmp_c_int_least32_t[2][2][2] =  {5,5,5,5,5,5,5,5 };
int_least64_t cmp_c_int_least64_t[2][2][2] =  {1111111111111111111LL,1111111111111111111LL,1111111111111111111LL,1111111111111111111LL,1111111111111111111LL,1111111111111111111LL,1111111111111111111LL,1111111111111111111LL };
int_fast8_t   cmp_c_int_fast8_t[2][2][2] =  {1,1,1,1,1,1,1,1 };
int_fast32_t  cmp_c_int_fast32_t[2][2][2] =  {2147483647,127,-2147483648ll,0,1000000,-1,-2,-3 };
int_fast64_t  cmp_c_int_fast64_t[2][2][2] =  { 100ll, 101ll, 102ll, 103ll, 104ll, 105ll, 106ll, 107ll };


/* --------------------------------------------------------------*
*       1) Verify values from Fortran code                       *
*          with expected result arrays                           *
* --------------------------------------------------------------*/


    a=0;

    for ( i = 0; i < 2; i++ ) {
      for ( j = 0; j < 2; j++ ) {
        for ( k = 0; k < 2; k++ ) {

        if (blk_all.int_s1[i][j][k]         
!=  cmp_s1[i][j][k]   
)    exit (60);
        if (blk_all.int_s2[i][j][k]         
!=  cmp_s2[i][j][k]   
)    exit (61);
        if (blk_all.int_s4[i][j][k]         
!=  cmp_s4[i][j][k]   
)    exit (62);
        if (blk_all.int_s8[i][j][k]         
!=  cmp_s8[i][j][k]   
)    exit (63);
        if (blk_all.int_c_signed_char[i][j][k] 
!=  cmp_c_signed_char[i][j][k]  )    exit (64);
        if (blk_all.int_c_short[i][j][k]     
!=  cmp_c_short[i][j][k]        )    exit (65);
        if (blk_all.int_c_int[i][j][k]       
!=  cmp_c_int[i][j][k]          )    exit (66);
        if (blk_all.int_c_long[i][j][k]     
!=  cmp_c_long[i][j][k]         )    exit (67);
        if (blk_all.int_c_long_long[i][j][k]
!=  cmp_c_long_long[i][j][k]    )    exit (68);
        if (blk_all.int_c_size_t[i][j][k]       !=  cmp_c_size_t[i][j][k]       )    exit (69);
        if (blk_all.int_c_intptr_t[i][j][k] 
!=  cmp_c_intptr_t[i][j][k]     )    exit (70);
        if (blk_all.int_c_intmax_t[i][j][k]     !=  cmp_c_intmax_t[i][j][k]  
)    exit (71);
        if (blk_all.int_c_int8_t[i][j][k]   
!=  cmp_c_int8_t[i][j][k]       )    exit (72);
        if (blk_all.int_c_int16_t[i][j][k]      !=  cmp_c_int16_t[i][j][k]      )    exit (73);
        if (blk_all.int_c_int32_t[i][j][k]  
!=  cmp_c_int32_t[i][j][k]      )    exit (74);
        if (blk_all.int_c_int64_t[i][j][k]  
!=  cmp_c_int64_t[i][j][k]     
)    exit (75);
        if (blk_all.int_c_int_least8_t[i][j][k]
!=  cmp_c_int_least8_t[i][j][k] )    exit (76);
        if (blk_all.int_c_int_least16_t[i][j][k]!=  cmp_c_int_least16_t[i][j][k])    exit (77);
        if (blk_all.int_c_int_least32_t[i][j][k]!=  cmp_c_int_least32_t[i][j][k])    exit (78);
        if (blk_all.int_c_int_least64_t[i][j][k]!=  cmp_c_int_least64_t[i][j][k])    exit (79);
        if (blk_all.int_c_int_fast8_t[i][j][k]  !=  cmp_c_int_fast8_t[i][j][k]  )    exit (80);
        if (blk_all.int_c_int_fast32_t[i][j][k] !=  cmp_c_int_fast32_t[i][j][k] )    exit (81);
        if (blk_all.int_c_int_fast64_t[i][j][k] !=  cmp_c_int_fast64_t[i][j][k] )    exit (82);

      }
     }
    }


/* --------------------------------------------------------------*
*      2) modify the values and pass to fortran                  *
* --------------------------------------------------------------*/

    for ( i = 0; i < 2; i++ ) {
      for ( j = 0; j < 2; j++ ) {
        for ( k = 0; k < 2; k++ ) {

        blk_all.int_s1[i][j][k]              =  50; 
        blk_all.int_s2[i][j][k]              =  50;
        blk_all.int_s4[i][j][k]              =  50;
        blk_all.int_s8[i][j][k]              =  50;
        blk_all.int_c_signed_char[i][j][k]   =  50;
        blk_all.int_c_short[i][j][k]         =  50;
        blk_all.int_c_int[i][j][k]           =  50;
        blk_all.int_c_long[i][j][k]          =  50;
        blk_all.int_c_long_long[i][j][k]     =  50;
        blk_all.int_c_size_t[i][j][k]        =  50;
        blk_all.int_c_intptr_t[i][j][k]      =  50;
        blk_all.int_c_intmax_t[i][j][k]      =  50;
        blk_all.int_c_int8_t[i][j][k]        =  50;
        blk_all.int_c_int16_t[i][j][k]       =  50;
        blk_all.int_c_int32_t[i][j][k]       =  50;
        blk_all.int_c_int64_t[i][j][k]       =  50;
        blk_all.int_c_int_least8_t[i][j][k]  =  50;
        blk_all.int_c_int_least16_t[i][j][k] =  50;
        blk_all.int_c_int_least32_t[i][j][k] =  50;
        blk_all.int_c_int_least64_t[i][j][k] =  50;
        blk_all.int_c_int_fast8_t[i][j][k]   =  50;
        blk_all.int_c_int_fast32_t[i][j][k]  =  50;
        blk_all.int_c_int_fast64_t[i][j][k]  =  50ll;

      }
     }
    }


/* --------------------------------------------------------------*
*       3) Verify values before passing to Fortran code          *
* --------------------------------------------------------------*/

    a=0;
    for ( i = 0; i < 2; i++ ) {
      for ( j = 0; j < 2; j++ ) {
        for ( k = 0; k < 2; k++ ) {

        if (blk_all.int_s1[i][j][k]             !=  50     )    exit (85);
        if (blk_all.int_s2[i][j][k]             !=  50     )    exit (86);
        if (blk_all.int_s4[i][j][k]             !=  50     )    exit (87);
        if (blk_all.int_s8[i][j][k]             !=  50     )    exit (88);
        if (blk_all.int_c_signed_char[i][j][k]  !=  50     )    exit (89);
        if (blk_all.int_c_short[i][j][k]        !=  50     )    exit (90);
        if (blk_all.int_c_int[i][j][k]          !=  50     )    exit (91);
        if (blk_all.int_c_long[i][j][k]         !=  50     )    exit (92);
        if (blk_all.int_c_long_long[i][j][k]    !=  50     )    exit (93);
        if (blk_all.int_c_size_t[i][j][k]       !=  50     )    exit (94);
        if (blk_all.int_c_intptr_t[i][j][k]     !=  50     )    exit (95);
        if (blk_all.int_c_intmax_t[i][j][k]     !=  50     )    exit (96);
        if (blk_all.int_c_int8_t[i][j][k]       !=  50     )    exit (97);
        if (blk_all.int_c_int16_t[i][j][k]      !=  50     )    exit (98);
        if (blk_all.int_c_int32_t[i][j][k]      !=  50     )    exit (99);
        if (blk_all.int_c_int64_t[i][j][k]      !=  50     )    exit (100);
        if (blk_all.int_c_int_least8_t[i][j][k] !=  50     )    exit (101);
        if (blk_all.int_c_int_least16_t[i][j][k]!=  50     )    exit (102);
        if (blk_all.int_c_int_least32_t[i][j][k]!=  50     )    exit (103);
        if (blk_all.int_c_int_least64_t[i][j][k]!=  50     )    exit (104);
        if (blk_all.int_c_int_fast8_t[i][j][k]  !=  50     )    exit (105);
        if (blk_all.int_c_int_fast32_t[i][j][k] !=  50     )    exit (106);
        if (blk_all.int_c_int_fast64_t[i][j][k] !=  50ll   )    exit (107);

      }
     }
    }


}

