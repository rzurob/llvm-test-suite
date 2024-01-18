! GB DTP extension using:
! ftcx_dtp -qk -qnol /tstdev/F2003/dataPtrAssgn/Basic/dataPtrC721.f
! opt variations: -qnok -ql

!*********************************************************************
!*  ===================================================================
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

  TYPE :: DT(K1)    ! (4)
      INTEGER, KIND :: K1
  END TYPE

  CONTAINS
  SUBROUTINE S(Arr1, Arr2)

  TYPE(DT(4)),   TARGET  :: Arr(1)
  CLASS(DT(4)),  POINTER :: Ptr1(:)
  TYPE(DT(4))            :: Arr1(1), Arr2(1,1)

  Arr1(1:) => Arr1
  Arr2(1:1, 1:1) => Arr1

  ASSOCIATE ( As => Ptr1 )
    AS(1:) => Arr
    As(1:1) => Arr
  END ASSOCIATE

  END SUBROUTINE

  END


