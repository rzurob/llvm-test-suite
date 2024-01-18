!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             :  dataPtrPrivIntent.f
!*
!*  DATE                       : Feb. 13, 2006
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
!*  Intent / Proctected
!*
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  MODULE M
  INTEGER, POINTER,  PROTECTED  :: PArr1(:, :) =>NULL()
  END MODULE


  PROGRAM dataPtrPrivIntent
  END

  SUBROUTINE Sub(PArr)
  USE M
  IMPLICIT NONE

  INTEGER, TARGET  :: Arr1(0:99)
  INTEGER, TARGET  :: Arr2(0:9, 0:9)

  INTEGER, POINTER,  INTENT(IN)  :: PArr(:, :)


  PArr(1:, 1:)  => Arr2

  PArr(1:10, 1:10) => Arr1

  PArr1(1:, 1:)  => Arr2

  PArr1(1:10, 1:10) => Arr1

  END


