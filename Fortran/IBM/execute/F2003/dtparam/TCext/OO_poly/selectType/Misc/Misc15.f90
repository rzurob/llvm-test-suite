! GB DTP extension using:
! ftcx_dtp -qk -qnol /tstdev/OO_poly/selectType/Misc/Misc15.f
! opt variations: -qnok -ql

! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Jan. 17, 2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Select Type
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  : Feature 219934.OO_poly
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*  ICE on ALLOCATE with function return as source
!*    (ICE-298283)
!*
!234567890123456789012345678901234567890123456789012345678901234567890



  PROGRAM Misc15
  IMPLICIT NONE
  TYPE :: Child(K1)    ! (4)
      INTEGER, KIND :: K1
  END TYPE

  TYPE(Child(4)) :: V(3,3)
  INTEGER :: B1(3)=(/1,2,3/)
  INTEGER :: B2(3)=(/1,2,3/)

  CLASS(*), ALLOCATABLE :: As (:,:)
  allocate(As(3,3), source=Fun(v))

  CONTAINS

  FUNCTION Fun(Arg)
  CLASS(*) :: Arg(:,:)
  CLASS(*), POINTER :: Fun(:,:)
  INTEGER :: SubScript(2)

  SubScript = SHAPE(Arg)
  ALLOCATE(Fun(SubScript(1), SubScript(2)), SOURCE=Arg)

  END FUNCTION
  END


