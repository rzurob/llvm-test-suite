!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE TITLE            : shiftl-shiftr01d.f
!*
!*  PROGRAMMER                 : Maryam Moghadas
!*  DATE                       : 2013-02-27
!*  ORIGIN                     :
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   : SHIFTL, SHIFTR intrinsics
!*                             :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DRIVER STANZA              :
!*
!*  DESCRIPTION                : SHIFTL(I, SHIFT), SHIFTR(I,SHIFT)
!*
!*  Test the compilation fails if:
!*
!*  * first argument not an integer,
!*  * second argument not a nonnegative integer less than or equal to BIT_SIZE(I) 
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

program shiftl_shiftr

 real :: i_r = 2.0, shift_r = 3.0
 
 print *, SHIFTL(i_r, 3)
 print *, SHIFTL(2.0, 3)
 print *, SHIFTL(2, shift_r)
 print *, SHIFTL(2, 3.0)
 print *, SHIFTL(2, -3)
 print *, SHIFTL(2, INT(-3,1))
 print *, SHIFTL(2, 33)
 print *, SHIFTL(INT(2,8), 65)

 print *, SHIFTR(i_r, 3)
 print *, SHIFTR(2.0, 3)
 print *, SHIFTR(2, shift_r)
 print *, SHIFTR(2, 3.0)
 print *, SHIFTR(2, -3)
 print *, SHIFTR(2, INT(-3,2))
 print *, SHIFTR(2, 33)
 print *, SHIFTR(INT(2,8), 65)

end 








