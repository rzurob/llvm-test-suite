! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 2013-02-10
!*  ORIGIN                     :
!*
!*  PRIMARY FUNCTIONS TESTED   : MASKL intrinsic
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : MASKL(I [,KIND])
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
 PROGRAM maskL_TEST
 IMPLICIT NONE
 INTEGER :: i
 INTEGER , PARAMETER :: k1=1, k2=2, k4=4, k8=8
 INTEGER :: t1 = MASKL(3,1), t2 = SHIFTL(7_1,8-3)
 INTEGER  :: a=3
 INTEGER :: arr1(8), mask_arr1(8)=0, arr2(8)=0

 arr1 = (/32, 31, 30, 29, 28, 27, 26, 25/)

 DO i = 1,8
    mask_arr1(i) = -2**(i-1)
 END DO

!---------------------------------------------------------!
!------- different methods for argument passing ----------!
 IF (MASKL(3) .NE. SHIFTL(7,BIT_SIZE(3)-3)) ERROR STOP 1
 IF (MASKL(3,1) .NE. SHIFTL(7_1,5))         ERROR STOP 2
 IF (MASKL(3,2) .NE. SHIFTL(7_2,16-3))      ERROR STOP 3
 IF (MASKL(3,4) .NE. SHIFTL(7_4,32-3))      ERROR STOP 4
 IF (MASKL(3,8) .NE. SHIFTL(7_8,64-3))      ERROR STOP 5

 IF (MASKL(a,k1) .NE. MASKL(3,1)) ERROR STOP 6
 IF (MASKL(a,k2) .NE. MASKL(3,2)) ERROR STOP 7
 IF (MASKL(a,k4) .NE. MASKL(3,4)) ERROR STOP 8
 IF (MASKL(a,k8) .NE. MASKL(3,8)) ERROR STOP 9

 IF (MASKL(3,k1) .NE. MASKL(3,1)) ERROR STOP 10
 IF (MASKL(a,1) .NE. MASKL(3,1))  ERROR STOP 11

 IF (MASKL(3,k2) .NE. MASKL(3,2)) ERROR STOP 12
 IF (MASKL(a,2)  .NE. MASKL(3,2)) ERROR STOP 13

 IF (MASKL(3,k4) .NE. MASKL(3,4)) ERROR STOP 14
 IF (MASKL(a,4) .NE. MASKL(3,4)) ERROR STOP 15

 IF (MASKL(3,k8) .NE. MASKL(3,8)) ERROR STOP 16
 IF (MASKL(a,8) .NE. MASKL(3,8)) ERROR STOP 17

!------- applying this intrinsic into an array argument -----!

IF (ANY(MASKL(arr1) .NE. mask_arr1)) ERROR STOP 18

PRINT *,MASKL(arr1)

!print*, "End of the program: Normal termination"
end
