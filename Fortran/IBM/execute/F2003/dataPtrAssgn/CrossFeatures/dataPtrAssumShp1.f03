!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Feb. 09, 2006
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
!*  Assumed shape array
!*
!*  zero size
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890



  PROGRAM dataPtrAssumShp1
  IMPLICIT NONE

  TYPE :: DT
    INTEGER :: ID
  END TYPE


  TYPE (DT),  TARGET :: Arr1(10, 10)
  TYPE (DT),  TARGET :: Arr2(1:0)

  INTEGER           :: N, L, U

  N =10
  L = 0
  U = 9

  CALL S(Arr1(:,1:0), Arr2, 0, 9)

  CONTAINS

  SUBROUTINE S(Arr1, Arr2, L, U)
  CLASS(DT), TARGET  :: Arr1(L:, L:)
  TYPE(DT),  TARGET  :: Arr2(L:)
  INTEGER           :: L, U
  CLASS(*), POINTER :: Ptr(:, :)

  Ptr(L:, L:) => Arr1
  IF ( .NOT. ASSOCIATED(Ptr))                       ERROR STOP 11
  IF (ANY( LBOUND(Ptr)         .NE. (/L, 1 /)))     ERROR STOP 12
  IF (ANY( SHAPE (Ptr)         .NE. (/U-L+1, 0 /))) ERROR STOP 13

  Ptr(L:U, L:L-1) => Arr2
  IF ( .NOT. ASSOCIATED(Ptr))                       ERROR STOP 21
  IF (ANY( LBOUND(Ptr)         .NE. (/L, 1 /)))     ERROR STOP 22
  IF (ANY( SHAPE (Ptr)         .NE. (/U-L+1, 0 /))) ERROR STOP 23

  END SUBROUTINE

  END

