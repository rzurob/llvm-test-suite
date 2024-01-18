! *********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE TITLE            : SHIFTA_TEST
!*
!*  PROGRAMMER                 : Maryam Moghadas
!*  DATE                       : 2013-02-10
!*  ORIGIN                     :
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   : SHIFTA intrinsic
!*                             :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DRIVER STANZA              : xlf2003
!*
!*  DESCRIPTION                : SHIFTA(I,SHIFT)
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012
                                         
   PROGRAM ShIFTA_TEST
   IMPLICIT NONE   
   INTEGER :: i1,j1, shift1
   INTEGER :: SHIFT = 2, a = 2**31 , expr = SHIFTA(2**31, 2)
   INTEGER :: i, n(20), shift_arr(20), a_shift(20)
   INTEGER (KIND = 1) :: a1 = 2_1**7  
   INTEGER (KIND = 2) :: a2 = 2_2**15
   INTEGER (KIND = 4) :: a4 = 2_4**31
   INTEGER (KIND = 8) :: a8 = 2_8**63
   DO i = 1,20
     n(i) = 2**i
     shift_arr(i) = i
     a_shift(i) = SHIFTA(n(i), shift_arr(i))
   END DO  
   

   IF ( SHIFTA(IBSET(0,BIT_SIZE(0)-1), 2) .NE. SHIFTL(7,BIT_SIZE(0)-3) )  ERROR STOP 1

   IF ( SHIFTA(a, SHIFT) .NE. SHIFTA(2**31,2)) ERROR STOP 2

   IF ( expr .NE. SHIFTA(2**31,2)) ERROR STOP 3
 
  !---- arguments reordering ----------------------!

  IF (SHIFTA(SHIFT=2, I=a) .NE. SHIFTA(2**31,2)) ERROR STOP 4 

  !---- passing array as an argument to SHIFTA ----!

   IF ( ANY(SHIFTA(n,1) .NE. n/2) ) ERROR STOP 5
   
   IF ( ANY(SHIFTA(n,shift_arr) .NE. a_shift) ) ERROR STOP 6
   
  !---- passing arguments of different kind -------
 
   IF (SHIFTA(-huge(a1)-1_1,7) .NE. -1_1) ERROR STOP 7
   IF (SHIFTA(a1,7) .NE. INT(-1,1)) ERROR STOP 8

   IF (SHIFTA(-huge(a2)-1_2,15) .NE. -1_2) ERROR STOP 9
   IF (SHIFTA(a2,15) .NE. INT(-1,2)) ERROR STOP 10

   IF (SHIFTA(-huge(a4)-1_4,31) .NE. -1_4) ERROR STOP 11
   IF (SHIFTA(a4,31) .NE. INT(-1,4)) ERROR STOP 12

   IF (SHIFTA(-huge(a8)-1_8,63) .NE. -1_8) ERROR STOP 13
   IF (SHIFTA(a8,63) .NE. INT(-1,8)) ERROR STOP 14 

   print*, "End of the program: Normal termination"

END


