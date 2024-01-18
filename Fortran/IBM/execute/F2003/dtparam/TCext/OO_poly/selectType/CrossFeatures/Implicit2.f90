! GB DTP extension using:
! ftcx_dtp -qk /tstdev/OO_poly/selectType/CrossFeatures/Implicit2.f
! opt variations: -qck -qnok

! *********************************************************************
!*  ===================================================================
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
    TYPE,  EXTENDS(DT0) :: DT1(N2)    ! (4,513,1025)
      INTEGER, LEN  :: N2
      CHARACTER(N2) :: C1="1"
    END TYPE

    TYPE, EXTENDS(DT1) :: DT(N3)    ! (4,513,1025,2049)
      INTEGER, LEN  :: N3
      CHARACTER(N3) :: C2="2"
    END TYPE

    TYPE (DT(4,513,1025,2049)), SAVE, TARGET :: V

    CONTAINS

    SUBROUTINE SetObj(Arg)
    CLASS(DT0(4,*)) :: Arg
      Arg%C0 = "SetDT0"
    END SUBROUTINE

  END MODULE

  PROGRAM Implicit2
  USE M
  IMPLICIT CLASS(*)(U)

  CALL Sub(V)

  CONTAINS

  SUBROUTINE Sub(U)

  SELECT TYPE (U)
  CLASS IS (DT(4,*,*,*))
    IF (TRIM(V%C0) .NE. "0") STOP 20
    IF (TRIM(V%C1) .NE. "1") STOP 21
    IF (TRIM(V%C2) .NE. "2") STOP 22

    U%DT0%C0 ="?"
    U%DT1%C1 ="?"
    U%C2 ="?"

    IF (TRIM(V%C0) .NE. "?") STOP 30
    IF (TRIM(V%C1) .NE. "?") STOP 31
    IF (TRIM(V%C2) .NE. "?") STOP 32

  CLASS DEFAULT
    STOP 40
  END SELECT

  END SUBROUTINE

  END



