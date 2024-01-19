! GB DTP extension using:
! ftcx_dtp -qk -qnol /tstdev/F2003/dataPtrAssgn/Basic/dataPtrC718.f
! opt variations: -qnok -ql

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
!*  C718 (R735) If bounds-spec-list is specified, the number of bounds-specs shall equal
!*  the rank of data pointer-object.
!*
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM dataPtrC718
  IMPLICIT NONE

  TYPE :: DT(K1)    ! (4)
      INTEGER, KIND :: K1
  END TYPE

  TYPE(DT(4)),   TARGET  :: T(1)
  CLASS(DT(4) ), POINTER :: Ptr1(:)
  TYPE(DT(4)),   TARGET  :: Arr19(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1)
  TYPE(DT(4)),   TARGET  :: Arr20(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1)
  CLASS(DT(4) ), POINTER :: Ptr19(:,:,:,:,:,:,:,:,:,:,:,:,:,:,:,:,:,:,:)
  CLASS(DT(4) ), POINTER :: Ptr20(:,:,:,:,:,:,:,:,:,:,:,:,:,:,:,:,:,:,:,:)


  Ptr1(1:,1:) => T

  Ptr19(1:,1:,1:,1:,1:,1:,1:,1:,1:,1:,1:,1:,1:,1:,1:,1:,1:,1:) => Arr19

  Ptr19(1:,1:,1:,1:,1:,1:,1:,1:,1:,1:,1:,1:,1:,1:,1:,1:,1:,1:,1:,1:) => Arr19

  Ptr20(1:,1:,1:,1:,1:,1:,1:,1:,1:,1:,1:,1:,1:,1:,1:,1:,1:,1:,1:) => Arr20

  Ptr20(1:,1:,1:,1:,1:,1:,1:,1:,1:,1:,1:,1:,1:,1:,1:,1:,1:,1:,1:,1:,1:) => Arr20

  END


