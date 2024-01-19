! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 2013-02-10
!*
!*  PRIMARY FUNCTIONS TESTED   : SHIFTL and SHIFTR intrinsics
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : SHIFTL(I [,KIND]), SHIFTR(I [,KIND])
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

  PROGRAM SHIFTL_SHIFTR_TEST
   IMPLICIT NONE
   INTEGER :: SHIFT = 1, a = 3 , b = 5, expr_1 = SHIFTL(1,1)+1, expr_2 = SHIFTR(10,1)
   INTEGER :: i, n(20)

   DO i = 1,20
     n(i) = 2**i
   END DO

  !------------- SHIFTL(I,SHIFT) -----------!
   IF ( SHIFTL(3,1) .NE. 6) ERROR STOP 1
   IF ( SHIFTL(a,SHIFT) .NE. 6) ERROR STOP 2
   IF ( SHIFTL(expr_1,1)  .NE. 6) ERROR STOP 3
   !----- passing argument of different kinds
   IF ( SHIFTL(INT(3,1),1) .NE. INT(6,1)) ERROR STOP 4
   IF ( SHIFTL(INT(3,2),1) .NE. INT(6,2)) ERROR STOP 5
   IF ( SHIFTL(INT(3,4),1) .NE. INT(6,4)) ERROR STOP 6
   IF ( SHIFTL(INT(3,8),1) .NE. INT(6,8)) ERROR STOP 7
   !---- passing an array argument
   IF ( ANY(SHIFTL(n,3) .NE. n*8)) ERROR STOP 8
   IF ( ANY(SHIFTL((/ 1, 2, 3, 4, 5/),2) .NE. (/ 4, 8, 12, 16, 20/)) )  ERROR STOP 9
   !--- arguments reordering
   IF ( SHIFTL( SHIFT=1, I=3) .NE. 6) ERROR STOP 10

  !------------ SHIFTR(I,SHIFT)------------!
   IF (SHIFTR(5,1) .NE. 2) ERROR STOP 11
   IF (SHIFTR(b,SHIFT) .NE. 2) ERROR STOP 12
   IF (SHIFTR(expr_2,1) .NE. 2) ERROR STOP 13
   !----- passing argument of different kinds
   IF (SHIFTR(INT(5,1),1) .NE. INT(2,1)) ERROR STOP 14
   IF (SHIFTR(INT(5,2),1) .NE. INT(2,2)) ERROR STOP 15
   IF (SHIFTR(INT(5,4),1) .NE. INT(2,4)) ERROR STOP 16
   IF (SHIFTR(INT(5,8),1) .NE. INT(2,8)) ERROR STOP 17

   !---- passing an array argument
   IF (ANY(SHIFTR(n,2) .NE. n/4)) ERROR STOP 18
   IF (ANY(SHIFTR((/ 2, 4, 6 , 8 /), 1) .NE. (/ 1, 2, 3, 4/) ) ) ERROR STOP 19
   !----- arguments reordering
   IF (SHIFTR(SHIFT=1, I=5) .NE. 2) ERROR STOP 20

   print*, "End of the program: Normal termination"

   END


