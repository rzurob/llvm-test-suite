!*******************************************************************************
!*  ============================================================================
!*
!*  TEST CASE NAME             : fdivideINT64
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
!*   This test case uses INT64 from IOS_FORTRAN_ENV module.
!*   Focus of this test case is to divide two arrays.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

PROGRAM fdivideINT64
USE, INTRINSIC :: ISO_FORTRAN_ENV

implicit none

INTEGER :: J, DivideBy,i
INTEGER(8) :: DivideBy1,Array(30) ,Result(30)
INTEGER(INT64) :: DivideBy2,Array1(30), Result1(30)

J=31
DivideBy1=4000
DivideBy2=4000

DO i=1,30
 Array(i)=i * 443044364
 Array1(i)=i * 443044364
END DO

DivideBy1=Array1(29)
DivideBy2=Array(29)

DO i=1,30
 Result(i)=Array(i) / DivideBy1
 Result1(i)=Array1(i) / DivideBy2
END DO


DO i=1,30
 IF ( Result(i) .NE. Result1(i) ) THEN
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
