! GB DTP extension using:
! ftcx_dtp -qk -qreuse=base /tstdev/OO_poly/selectType/CrossFeatures/Final1.f
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
!* Finalization
!* (ICE-299454)
!* (Finalization earlier-301357)
!*
!234567890123456789012345678901234567890123456789012345678901234567890




  MODULE M
    TYPE  :: DT0(K1,N1)    ! (4,513)
      INTEGER, KIND :: K1
      INTEGER, LEN  :: N1
      CHARACTER(N1) :: C0="0"
      CONTAINS
      PROCEDURE, PASS  :: GetChar
      Final :: FinalDT0
    END TYPE

    TYPE,  EXTENDS(DT0) :: DT1    ! (4,513)
      CHARACTER(N1) :: C1="1"
      CONTAINS
      Final :: FinalDT1
    END TYPE

    TYPE, EXTENDS(DT1) :: DT    ! (4,513)
      CHARACTER(N1) :: C2="2"
      CONTAINS
      Final :: FinalDT
    END TYPE

    LOGICAL :: Final(0:2) = .FALSE.

    CONTAINS

    SUBROUTINE FinalDT0(Arg)
    TYPE(DT0(4,*)) :: Arg
      Final(0) = .TRUE.
    END SUBROUTINE

    SUBROUTINE FinalDT1(Arg)
    TYPE(DT1(4,*)) :: Arg
      Final(1) = .TRUE.
    END SUBROUTINE

    SUBROUTINE FinalDT(Arg)
    TYPE(DT(4,*)) :: Arg
      Final(2) = .TRUE.
    END SUBROUTINE


    FUNCTION GetChar(Arg)
    CLASS(DT0(4,*)) :: Arg
    CHARACTER(513) :: GetChar
      SELECT TYPE (Arg)
      TYPE IS (DT0(4,*))
        GetChar = Arg%C0
      TYPE IS (DT1(4,*))
        GetChar = Arg%C1
      TYPE IS (DT(4,*))
        GetChar = Arg%C2
      CLASS DEFAULT
        STOP 20
      END SELECT
    END FUNCTION

  END MODULE

  PROGRAM final1
  USE M
  IMPLICIT CLASS(*)(U)

  Final = .FALSE.
  SELECT TYPE ( V =>UFun(DT(4,513)(C2="-2", C1="-1", C0="-0")) )
    CLASS IS (DT(4,*))
      SELECT TYPE (V)
        CLASS DEFAULT
          SELECT TYPE (V)
            TYPE IS (DT(4,*))

              IF (TRIM(V%C0) .NE. "-0") ERROR STOP 30
              IF (TRIM(V%C1) .NE. "-1") ERROR STOP 31
              IF (TRIM(V%C2) .NE. "-2") ERROR STOP 32

              IF (TRIM(V%DT0%GetChar()) .NE. "-0") ERROR STOP 40
              IF (TRIM(V%DT1%GetChar()) .NE. "-1") ERROR STOP 41
              IF (TRIM(V%GetChar())     .NE. "-2") ERROR STOP 42

            CLASS DEFAULT
              STOP 50
          END SELECT

        !NO finalization
        IF (ANY(Final)) ERROR STOP 60
      END SELECT

      !NO finalization
      IF (ANY(Final)) ERROR STOP 61
    CLASS DEFAULT
      STOP 62
  END SELECT

  ! Finalization on DT structure constructor
  IF (ANY(Final) .NEQV. .TRUE.) ERROR STOP 63

  CONTAINS

  FUNCTION UFun(UArg)
  USE M
  POINTER :: UFun
    ALLOCATE(UFun, SOURCE=UArg)
  END FUNCTION


  END

