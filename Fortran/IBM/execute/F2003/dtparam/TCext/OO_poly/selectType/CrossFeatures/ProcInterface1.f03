! GB DTP extension using:
! ftcx_dtp -qk -qreuse=base /tstdev/OO_poly/selectType/CrossFeatures/ProcInterface1.f
! opt variations: -qck -qnok -qreuse=none

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
!* Procedure Interface
!* ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890




  MODULE M
    TYPE  :: DT0(K1,N1)    ! (4,513)
      INTEGER, KIND :: K1
      INTEGER, LEN  :: N1
      CHARACTER(N1) :: C0="0"
      CONTAINS
      PROCEDURE, PASS   :: GetChar
    END TYPE

    !TYPE, ABSTRACT, EXTENDS(DT0) :: DT1
    TYPE,  EXTENDS(DT0) :: DT1    ! (4,513)
      CHARACTER(N1) :: C1="1"
    END TYPE

    TYPE, EXTENDS(DT1) :: DT    ! (4,513)
      CHARACTER(N1) :: C2="2"
    END TYPE

    TYPE (DT(4,513)), SAVE, TARGET :: V

    CONTAINS

    FUNCTION GetChar(Arg)
    CLASS(DT0(4,*)) :: Arg
    CHARACTER(513), ALLOCATABLE :: GetChar
      SELECT TYPE (Arg)
      TYPE IS (DT0(4,*))
        ALLOCATE(GetChar, SOURCE=Arg%C0)
      TYPE IS (DT1(4,*))
        ALLOCATE(GetChar, SOURCE=Arg%C1)
      TYPE IS (DT(4,*))
        ALLOCATE(GetChar, SOURCE=Arg%C2)
      CLASS DEFAULT
        STOP 20
      END SELECT
    END FUNCTION

  END MODULE

  PROGRAM ProcInterface1
  USE M
  IMPLICIT CLASS(DT(4,513))(U)

  CALL Sub(DT(4,513)())

  CONTAINS

  SUBROUTINE Sub(UArg)

  INTERFACE
    FUNCTION UFun(UArg)
      CLASS(*), POINTER :: UFun
      CLASS(*) :: UArg
    END FUNCTION
  END INTERFACE


  SELECT TYPE ( V =>UFun(UArg) )
  CLASS IS (DT(4,*))

    IF (TRIM(V%C0) .NE. "0") ERROR STOP 30
    IF (TRIM(V%C1) .NE. "1") ERROR STOP 31
    IF (TRIM(V%C2) .NE. "2") ERROR STOP 32

    IF (TRIM(V%DT0%GetChar()) .NE. "0") ERROR STOP 40
    IF (TRIM(V%DT1%GetChar()) .NE. "1") ERROR STOP 41
    IF (TRIM(V%GetChar())     .NE. "2") ERROR STOP 42

  CLASS DEFAULT
    STOP 50
  END SELECT

  END SUBROUTINE

  END

  FUNCTION UFun(UArg)
  IMPLICIT NONE
  CLASS(*), POINTER :: UFun
  CLASS(*) :: UArg
    ALLOCATE(UFun, SOURCE=UArg)
  END FUNCTION

