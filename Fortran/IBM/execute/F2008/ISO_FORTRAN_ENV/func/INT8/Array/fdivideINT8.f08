!*******************************************************************************
!*  ============================================================================
!*
!*  DATE                       : 2010-07-12
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  : Feature Number
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*  ADAPTED FROM               :
!*
!*  DESCRIPTION
!*   This test case uses INT8 from IOS_FORTRAN_ENV module.
!*   Focus of this test case is to divide two arrays.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

PROGRAM fdivideINT8
USE, INTRINSIC :: ISO_FORTRAN_ENV

implicit none

INTEGER :: J, i
INTEGER(1) :: DivideBy,Array(30) ,Result(30)
INTEGER(INT8) :: DivideBy1,Array1(30), Result1(30)

J=31
DivideBy=4
DivideBy1=4

DO i=1,30
 Array(i)=i * 2
 Array1(i)=i * 2
END DO

DivideBy=Array(29)
DivideBy1=Array(29)

DO i=1,30
 Result(i)=Array(i) / DivideBy
 Result1(i)=Array1(i) / DivideBy1
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