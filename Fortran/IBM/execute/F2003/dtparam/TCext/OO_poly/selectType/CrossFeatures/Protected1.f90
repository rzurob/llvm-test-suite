! GB DTP extension using:
! ftcx_dtp -qck -qdeferredlp -qreuse=base /tstdev/OO_poly/selectType/CrossFeatures/Protected1.f
! opt variations: -qnock -qnodeferredlp -qreuse=none

! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qfree=f90
! %GROUP: Protected1.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : Protected
!*
!*  DATE                       : Feb. 02, 2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Select Type
!*
!*  SECONDARY FUNCTIONS TESTED : Selector
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
!* Protected
!* (Comp failed-299257)
!*
!234567890123456789012345678901234567890123456789012345678901234567890



  MODULE M
    TYPE  :: DT0(K1,N1)    ! (1,513)
      INTEGER, KIND             :: K1
      INTEGER, LEN              :: N1
      CHARACTER(kind=K1,len=N1) :: C0="0"
      CONTAINS
      PROCEDURE, PASS   :: SetObj
    END TYPE

    TYPE, ABSTRACT, EXTENDS(DT0) :: DT1(N2)    ! (1,513,1025)
      INTEGER, LEN              :: N2
      CHARACTER(kind=K1,len=N2) :: C1="1"
    END TYPE

    TYPE, EXTENDS(DT1) :: DT(N3)    ! (1,513,1025,2049)
      INTEGER, LEN              :: N3
      CHARACTER(kind=K1,len=N3) :: C2="2"
    END TYPE

    TYPE(DT(1,513,1025,2049)),  SAVE, TARGET,  PROTECTED :: Tar
    CLASS(DT(1,:,:,:)), SAVE, POINTER, PROTECTED :: PPtr

    CONTAINS

    SUBROUTINE SetObj(Arg)
    CLASS(DT0(1,*)) :: Arg
      Arg%C0 = "SetDT0"
    END SUBROUTINE

    SUBROUTINE SetPtr(Ptr, Tar)
    TYPE(DT(1,*,*,*)), TARGET  :: Tar
    CLASS(DT(1,:,:,:)), POINTER :: Ptr
      Ptr => Tar
    END SUBROUTINE

  END MODULE

  PROGRAM Protected1
  USE M
  IMPLICIT NONE

  CALL SetPtr(PPtr, Tar)

  SELECT TYPE (Ptr => PPtr)
  CLASS IS (DT(1,*,*,*))

    IF (TRIM(Ptr%C0) .NE. "0") STOP 31
    IF (TRIM(Ptr%C1) .NE. "1") STOP 32
    IF (TRIM(Ptr%C2) .NE. "2") STOP 33

  CLASS DEFAULT
    STOP 40
  END SELECT


  END



