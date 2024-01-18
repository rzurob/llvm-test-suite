! GB DTP extension using:
! ftcx_dtp -qck -qreuse=base /tstdev/OO_poly/selectType/CrossFeatures/Protected2.f
! opt variations: -qnock -qreuse=none

! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qfree=f90
! %GROUP: redherring.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD: tcomp Protected2.f
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
!* ()
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

    !TYPE, ABSTRACT, EXTENDS(DT0) :: DT1
    TYPE, EXTENDS(DT0) :: DT1(N2)    ! (1,513,1025)
      INTEGER, LEN              :: N2
      CHARACTER(kind=K1,len=N2) :: C1="1"
    END TYPE

    TYPE, EXTENDS(DT1) :: DT(N3)    ! (1,513,1025,2049)
      INTEGER, LEN              :: N3
      CHARACTER(kind=K1,len=N3) :: C2="2"
    END TYPE

    TYPE(DT(1,513,1025,2049)), SAVE, TARGET, PROTECTED :: V

    CONTAINS

    SUBROUTINE SetObj(Arg)
    CLASS(DT0(1,*)) :: Arg
      Arg%C0 = "SetDT0"
    END SUBROUTINE

  END MODULE

  PROGRAM Protected2
  USE M
  IMPLICIT NONE
  CLASS(*), POINTER :: Ptr

  Ptr => V

  SELECT TYPE (Ptr)
  CLASS IS (DT(1,*,*,*))
    Ptr%C1 ="?"
  CLASS DEFAULT
    STOP 40
  END SELECT

  IF (V%C1 .NE. "1" ) STOP 40

  END



