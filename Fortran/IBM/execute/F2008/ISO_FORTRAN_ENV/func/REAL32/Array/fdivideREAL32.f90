!*******************************************************************************
!*  ============================================================================
!*  XL Fortran Test Case                                   IBM REALERNAL USE ONLY
!*  ============================================================================
!*
!*  TEST CASE NAME             : fdivideREAL32
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
!*   This test case uses REAL32 from IOS_FORTRAN_ENV module.
!*   Focus of this test case is to divide two arrays.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

PROGRAM fdivideREAL32
USE, INTRINSIC :: ISO_FORTRAN_ENV

implicit none

LOGICAL :: precision_r4
INTEGER :: i
REAL :: J
REAL(4) :: DivideBy,Array(30) ,Result(30)
REAL(REAL32) :: DivideBy1,Array1(30), Result1(30)

J=31
DivideBy=0.5
DivideBy1=0.5

DO i=1,30
 Array(i)=i * 1.402823E+12
 Array1(i)=i * 1.402823E+12 
END DO

DivideBy=Array(29)
DivideBy1=Array(29)

DO i=1,30
 Result(i)=Array(i) / DivideBy
 Result1(i)=Array1(i) / DivideBy1
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
