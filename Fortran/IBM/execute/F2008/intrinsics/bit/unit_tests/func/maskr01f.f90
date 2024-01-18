! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 2013-02-10
!*
!*  PRIMARY FUNCTIONS TESTED   : MASKR intrinsic
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : MASKR(I [,KIND])
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

 PROGRAM maskR_TEST
 IMPLICIT NONE
 INTEGER :: i, j
 INTEGER, PARAMETER :: k1=1, k2=2, k4=4, k8=8
 INTEGER :: m = MASKR(3)
 INTEGER :: a=3
 INTEGER :: arr1(8), mask_arr1(8)

 arr1 = (/1, 2, 3, 4, 5, 6, 7, 8/)
 mask_arr1 =0

 Do i = 1,8
    j = i
    DO WHILE(j .NE. 0)
        mask_arr1(i)=mask_arr1(i) + 2**(j-1)
        j=j-1
  END DO
 END DO

!---------------------------------------------------------!
!------- dafferent methods for argument passang ----------!

 IF (MASKR(3) .NE. 7)   ERROR STOP 1
 IF (MASKR(3,1) .NE. 7_1) ERROR STOP 2
 IF (MASKR(3,2) .NE. 7_2) ERROR STOP 3
 IF (MASKR(3,4) .NE. 7_4) ERROR STOP 4
 IF (MASKR(3,8) .NE. 7_8) ERROR STOP 5

 IF (MASKR(a,k1) .NE. MASKR(3,1)) ERROR STOP 6
 IF (MASKR(a,k2) .NE. MASKR(3,2)) ERROR STOP 7
 IF (MASKR(a,k4) .NE. MASKR(3,4)) ERROR STOP 8
 IF (MASKR(a,k8) .NE. MASKR(3,8)) ERROR STOP 9


 IF (MASKR(3,k1) .NE. MASKR(3,1)) ERROR STOP 10
 IF (MASKR(a,1) .NE. MASKR(3,1))  ERROR STOP 11

 IF (MASKR(3,k2) .NE. MASKR(3,2)) ERROR STOP 12
 IF (MASKR(a,2)  .NE. MASKR(3,2)) ERROR STOP 13

 IF (MASKR(3,k4) .NE. MASKR(3,4)) ERROR STOP 14
 IF (MASKR(a,4) .NE. MASKR(3,4)) ERROR STOP 15

 IF (MASKR(3,k8) .NE. MASKR(3,8)) ERROR STOP 16
 IF (MASKR(a,8) .NE. MASKR(3,8)) ERROR STOP 17

 IF (MASKR(m-4) .NE. 7) ERROR STOP 18
!------- applyang thas intrinsic into an array argument -----!


IF (ANY(MASKR(arr1) .NE. mask_arr1)) ERROR STOP 19

PRINT *,MASKR(arr1)

!print*, "End of the program: Normal termination"
end
