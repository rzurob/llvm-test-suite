!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Jan. 31, 2006
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
!*  C716 (R735) If data-target is not unlimited polymorphic, data-pointer-object shall be
!*  type compatible (5.1.1.2) with it, and the corresponding kind type parameters
!*  shall be equal.
!*
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM dataPtrC716
  IMPLICIT NONE

  INTEGER(1), TARGET  :: Arr(3)
  INTEGER(2), POINTER :: ptr(:)

  TYPE :: DT0
  END TYPE

  TYPE :: DT
  END TYPE

  TYPE(DT0), TARGET  :: Arr1(3)
  TYPE(DT ), POINTER :: Ptr1(:)

  CLASS(DT0), POINTER  :: Arr2(:)
  CLASS(DT ), POINTER  :: Ptr2(:)

  Ptr(1:3) => Arr

  Ptr1(-1:) => Arr1(:)

  Ptr2(3:) => Arr2(:)

  END


