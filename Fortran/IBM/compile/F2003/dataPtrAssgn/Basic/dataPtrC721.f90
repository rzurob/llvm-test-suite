!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             :  dataPtrC721.f
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
!*  C721 (R736) A variable-name shall have the POINTER attribute.
!*
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM dataPtrC721
  IMPLICIT NONE

  TYPE :: DT
  END TYPE

  CONTAINS
  SUBROUTINE S(Arr1, Arr2)

  TYPE(DT),   TARGET  :: Arr(1)
  CLASS(DT),  POINTER :: Ptr1(:)
  TYPE(DT)            :: Arr1(1), Arr2(1,1)

  Arr1(1:) => Arr1
  Arr2(1:1, 1:1) => Arr1

  ASSOCIATE ( As => Ptr1 )
    AS(1:) => Arr
    As(1:1) => Arr
  END ASSOCIATE

  END SUBROUTINE

  END


