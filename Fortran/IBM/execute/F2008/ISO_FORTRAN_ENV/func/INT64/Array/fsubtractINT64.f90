!*******************************************************************************
!*  ============================================================================
!*  XL Fortran Test Case                                   IBM INTERNAL USE ONLY
!*  ============================================================================
!*
!*  TEST CASE NAME             : fsubtractINT64
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
!*   This test case uses INT64 from IOS_FORTRAN_ENV module.
!*   Focus of this test case is to subtact two arrays.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

PROGRAM fsubtractINT64
USE, INTRINSIC :: ISO_FORTRAN_ENV

implicit none

INTEGER :: J, i
INTEGER(8) :: Array(30) ,Result(30)
INTEGER(INT64) :: Array1(30), Result1(30)

J=31

DO i=1,30
 Array(i)=i * 922337547
 !Array(i)=i * 9223375477209
 Array1(i)=i * 307445734
END DO

DO i=1,30
 Result(i)=Array(i)-Array1(J-1)
 Result1(i)=Array(i)-Array1(J-1)
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
