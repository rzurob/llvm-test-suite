!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE TITLE            : shifta01d.f
!*
!*  PROGRAMMER                 : Maryam Moghadas
!*  DATE                       : 2013-02-27
!*  ORIGIN                     :
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   : SHIFTA intrinsics
!*                             :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DRIVER STANZA              :
!*
!*  DESCRIPTION                : SHIFTA(I, SHIFT)
!*
!*  Test the compilation fails if:
!*
!*  * first argument is not an integer type, 
!*  * second argument not a nonnegative integer type less than or equal to the BIT-SIZE(I)
!*
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================

program shifta_test

 real :: i_r = 2.0 , shift_r = 4

 print *, SHIFTA(i_r, 4)
 print *, SHIFTA(2.0, 4)
 print *, SHIFTA(2, shift_r)

 print *, SHIFTA(2, -4)
 print *, SHIFTA(2, 35)
 print *, SHIFTA(INT(2,1), 10)

end 







