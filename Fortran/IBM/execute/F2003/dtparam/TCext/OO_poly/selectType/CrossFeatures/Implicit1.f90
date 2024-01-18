! GB DTP extension using:
! ftcx_dtp -qk /tstdev/OO_poly/selectType/CrossFeatures/Implicit1.f
! opt variations: -qck -qnok

! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qfree=f90
! %GROUP: Implicit1.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : Implicit
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
!* Implicit
!* ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890



  MODULE M
    TYPE  :: DT0(K1,N1)    ! (4,513)
      INTEGER, KIND :: K1
      INTEGER, LEN  :: N1
      CHARACTER(N1) :: C0="0"
      CONTAINS
      PROCEDURE, PASS   :: SetObj
    END TYPE

    !TYPE, ABSTRACT, EXTENDS(DT0) :: DT1
    TYPE,  EXTENDS(DT0) :: DT1(K2,N2)    ! (4,513,4,1025)
      INTEGER, KIND :: K2
      INTEGER, LEN  :: N2
      CHARACTER(N2) :: C1="1"
    END TYPE

    TYPE, EXTENDS(DT1) :: DT(K3,N3)    ! (4,513,4,1025,4,2049)
      INTEGER, KIND :: K3
      INTEGER, LEN  :: N3
      CHARACTER(N3) :: C2="2"
    END TYPE

    TYPE (DT(4,513,4,1025,4,2049)), SAVE, TARGET :: V

    CONTAINS

    SUBROUTINE SetObj(Arg)
    CLASS(DT0(4,*)) :: Arg
      Arg%C0 = "SetDT0"
    END SUBROUTINE

  END MODULE

  PROGRAM Implicit1
  USE M
  IMPLICIT CLASS(*)(U)
  POINTER :: U

  U => V

  SELECT TYPE (U)
  CLASS IS (DT(4,*,4,*,4,*))
    U%DT0%C0 ="?"
    U%DT1%C1 ="?"
    U%C2 ="?"

    IF (TRIM(V%C0) .NE. "?") STOP 20
    IF (TRIM(V%C1) .NE. "?") STOP 21
    IF (TRIM(V%C2) .NE. "?") STOP 22

  CLASS DEFAULT
    STOP 40
  END SELECT


  END



