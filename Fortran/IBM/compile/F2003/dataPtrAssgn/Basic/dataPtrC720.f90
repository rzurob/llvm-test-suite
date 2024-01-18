!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             :  dataPtrC720.f
!*
!*  DATE                       : Feb. 02, 2006
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
!*  C720 (R735) If bounds-remapping-list is specified, data-target shall have rank one;
!*  otherwise, the ranks of data-pointer-object and data-target shall be the same.
!*
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM dataPtrC720
  IMPLICIT NONE

  TYPE :: DT
  END TYPE

  TYPE(DT),   TARGET  :: T
  TYPE(DT),   TARGET  :: Arr1(1), Arr2(2,2)
  CLASS(*),   POINTER :: Ptr1(:), Ptr2(:,:)


  Ptr1(1:1) => T
  Ptr1(1:)  => T

  Ptr1(1:1) => Arr2
  Ptr2(1:1, 2:2) => Arr2

  Ptr1(1:) => Arr2
  Ptr2(1:, 1:) => Arr1
  Ptr2 => Arr1

  END


