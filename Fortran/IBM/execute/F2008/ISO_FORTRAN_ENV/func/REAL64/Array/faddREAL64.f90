!*******************************************************************************
!*  ============================================================================
!*  XL Fortran Test Case                                   IBM REALERNAL USE ONLY
!*  ============================================================================
!*
!*  TEST CASE NAME             : faddREAL64
!*
!*  PROGRAMMER                 : Morteza Ershad-Manesh
!*  DATE                       : 2010-07-12
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  REFERENCE                  : Feature Number 376078
!*
!*  DRIVER STANZA              : xlf2003
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*  ADAPTED FROM               : 
!*
!*  DESCRIPTION
!*   This test case uses REAL64 from IOS_FORTRAN_ENV module.
!*   Focus of this test case is to add two arrays togther. 
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

PROGRAM faddREAL64
USE, INTRINSIC :: ISO_FORTRAN_ENV

implicit none

LOGICAL(8) :: precision_r8
INTEGER :: i
REAL :: J
REAL(8) :: Array(30) ,Result(30)
REAL(REAL64) :: Array1(30), Result1(30)

J=31

DO i=1,30
 Array(i)=i * 0.797000D+10
 Array1(i)=i * 0.797000D+10
END DO

DO i=1,30
 Result(i)=Array1(i)+Array(J-1)
 Result1(i)=Array(i)+Array1(J-1)
END DO


DO i=1,30
 IF (Result(i) .NE. Result1(i) ) THEN
! IF (.NOT. precision_r8(Result(i),Result1(i)) ) THEN
   PRINT*,"Mismatch detected"
   PRINT*,"Position: ",i
   PRINT*,"Values: ",Result(i), " != ", Result1(i)
   WRITE(*,10) Result(i)
   WRITE(*,10) Result1(i)
10 FORMAT( Z0)

   error stop 1_4
 ELSE
   PRINT*,"i=",i 
   PRINT*,Result(i)," == ", Result1(i)
 END IF
END DO

END PROGRAM
