/* C Code For testcase "fxmdva24.f"  */

#include <inttypes.h>
#include <stdio.h> 

/* Fortran subroutine */
extern fsub();

/* Declaration of variable, use assignment statement assigns */
/* a value to the variable in the main program, this is  */
/* one  check point of this testcase. */
extern size_t x1,x2,x3;

/* Declaration of a variable with its initialization */
extern size_t x4 = 1;

size_t compare_val1,compare_val2, compare_val3,compare_val4;

main()
{

/* Initialization */
 x1 = 1;
 compare_val1 = 2;

 x2 = 1;
 compare_val2 = 2;

 x3 = 1;
 compare_val3 = 2;

 compare_val4 = 2;

 /*Call Fortran subroutine */
 fsub();

/* Testcase 1 */

 /*x1 as global entity is changed in Fortran. */
 printf("Return to C main program: x1 = %d\n", x1);
 if (x1 !=compare_val1)
 return 1; 

/* Testcase 2 */
 
 /*x2 as global entity is changed in Fortran. */
 printf("Return to C main program: x2 = %d\n", x2);
 if (x2 !=compare_val2)
 return 2; 

/* Testcase 3 */
 
 /*x3 as global entity is changed in Fortran. */
 printf("Return to C main program: x3 = %d\n", x3);
 if (x3 !=compare_val3)
 return 3; 

/* Testcase 4 */

 /*x4 as global entity is changed in Fortran. */
 printf("Return to C main program: x4 = %d\n", x4);
 if (x4 !=compare_val4)
 return 4; 
 
 return 0;
}










