!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             :  dataPtrC719.f
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
!*  C719 (R735) If bounds-remapping-list is specified, the number of bounds-remappings
!*  shall equal the rank of data-pointer-object.
!*
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM dataPtrC719
  IMPLICIT NONE

  TYPE :: DT
  END TYPE

  TYPE(DT),   TARGET  :: T(1)
  TYPE(DT ),  POINTER :: Ptr1(:)
  TYPE(DT),   TARGET  :: Arr(1)
  CLASS(*),   POINTER :: Ptr19(:,:,:,:,:,:,:,:,:,:,:,:,:,:,:,:,:,:,:)
  TYPE(DT ),  POINTER :: Ptr20(:,:,:,:,:,:,:,:,:,:,:,:,:,:,:,:,:,:,:,:)


  Ptr1(1:1, 1:1) => T

  Ptr19(1:1,1:1,1:1,1:1,1:1,1:1,1:1,1:1,1:1,1:1,1:1,1:1,1:1,1:1,1:1,1:1,1:1,1:1) => Arr

  Ptr19(1:1,1:1,1:1,1:1,1:1,1:1,1:1,1:1,1:1,1:1,1:1,1:1,1:1,1:1,1:1,1:1,1:1,1:1,1:1,1:1) => Arr

  Ptr20(1:1,1:1,1:1,1:1,1:1,1:1,1:1,1:1,1:1,1:1,1:1,1:1,1:1,1:1,1:1,1:1,1:1,1:1,1:1) => Arr

  Ptr20(1:1,1:1,1:1,1:1,1:1,1:1,1:1,1:1,1:1,1:1,1:1,1:1,1:1,1:1,1:1,1:1,1:1,1:1,1:1,1:1,1:1) => Arr



  END


