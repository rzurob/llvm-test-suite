!*******************************************************************************
!*  ============================================================================
!*
!*  TEST CASE NAME             : faddREAL32
!*
!*  DATE                       : 2010-07-12
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  : Feature Number 376078
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*  ADAPTED FROM               :
!*
!*  DESCRIPTION
!*   This test case uses REAL32 from IOS_FORTRAN_ENV module.
!*   Focus of this test case is to add two arrays togther.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

PROGRAM faddREAL32
USE, INTRINSIC :: ISO_FORTRAN_ENV
implicit none

LOGICAL :: precision_r4
INTEGER :: i
REAL :: J
REAL(4) :: Array(30) ,Result(30)
REAL(REAL32) :: Array1(30), Result1(30)

J=31

DO i=1,30
 Array(i)=i * 1.402823E+12
 Array1(i)=i * 1.402823E+12
END DO

DO i=1,30
 Result(i)=Array1(i)+Array(J-1)
 Result1(i)=Array(i)+Array1(J-1)
END DO


DO i=1,30
 IF (.NOT.precision_r4(Result(i),Result1(i)) ) THEN
! IF ( Result(i) .NE. Result1(i) ) THEN
   PRINT*,"Mismatch detected"
   PRINT*,"Position: ",i
   PRINT*,"Values: ",Result(i), " != ", Result1(i)
   error stop 1_4
 ELSE
   PRINT*,"i=",i
   PRINT*,Result(i)," == ", Result1(i)
 END IF
END DO

END PROGRAM
