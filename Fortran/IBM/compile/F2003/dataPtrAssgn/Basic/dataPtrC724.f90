!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             :  dataPtrC724.f
!*
!*  DATE                       : Feb. 03, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Pointer Assignment Enhancement
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  : Feature Number 289075
!*
!*  REQUIRED COMPILER OPTIONS  : -qfree=f90
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*  C724 (R739) An expr shall be a reference to a function whose result is a data pointer.
!*
!*  (323051)
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM dataPtrC724
  IMPLICIT NONE

  INTEGER              :: I, J
  INTEGER, POINTER     :: Ptr(:)


  Ptr(I:) => (Ptr)
  Ptr(I:J) => (Ptr)

  Ptr(I:) => F1()
  Ptr(I:J) => F1()

  Ptr(I:) => F2()
  Ptr(I:J) => F2()

  CONTAINS

  FUNCTION F1()
  PROCEDURE(INTEGER), POINTER :: F1
    F1 => NULL()
  END FUNCTION

  FUNCTION F2()
  INTEGER :: F2(3)
    F2 = 1
  END FUNCTION

  END


