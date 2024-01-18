! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: 
! %COMPOPTS: -qfree=f90 
! %GROUP: Final1.f 
! %VERIFY:  
! %STDIN:
! %STDOUT: 
! %EXECARGS:
! %POSTCMD: 
! %END
! *********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : Final1 
!*  TEST CASE TITLE            : 
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : Feb. 02, 2005
!*  ORIGIN                     : AIX Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Select Type 
!*
!*  SECONDARY FUNCTIONS TESTED : Selector 
!*
!*  REFERENCE                  : Feature 219934.OO_poly
!*
!*  DRIVER STANZA              :
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
    TYPE  :: DT0
      CHARACTER(513) :: C0="0"
      CONTAINS
      PROCEDURE, PASS  :: GetChar
      Final :: FinalDT0 
    END TYPE

    TYPE,  EXTENDS(DT0) :: DT1
      CHARACTER(513) :: C1="1"
      CONTAINS
      Final :: FinalDT1 
    END TYPE

    TYPE, EXTENDS(DT1) :: DT
      CHARACTER(513) :: C2="2"
      CONTAINS
      Final :: FinalDT 
    END TYPE

    LOGICAL :: Final(0:2) = .FALSE. 

    CONTAINS

    SUBROUTINE FinalDT0(Arg)
    TYPE(DT0) :: Arg
      Final(0) = .TRUE.
    END SUBROUTINE

    SUBROUTINE FinalDT1(Arg)
    TYPE(DT1) :: Arg
      Final(1) = .TRUE.
    END SUBROUTINE

    SUBROUTINE FinalDT(Arg)
    TYPE(DT) :: Arg
      Final(2) = .TRUE.
    END SUBROUTINE


    FUNCTION GetChar(Arg)
    CLASS(DT0) :: Arg
    CHARACTER(513) :: GetChar
      SELECT TYPE (Arg)
      TYPE IS (DT0)
        GetChar = Arg%C0
      TYPE IS (DT1)
        GetChar = Arg%C1
      TYPE IS (DT)
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
  SELECT TYPE ( V =>UFun(DT(C2="-2", C1="-1", C0="-0")) )
    CLASS IS (DT)
      SELECT TYPE (V)
        CLASS DEFAULT
          SELECT TYPE (V)
            TYPE IS (DT)
  
              IF (TRIM(V%C0) .NE. "-0") STOP 30
              IF (TRIM(V%C1) .NE. "-1") STOP 31
              IF (TRIM(V%C2) .NE. "-2") STOP 32
    
              IF (TRIM(V%DT0%GetChar()) .NE. "-0") STOP 40
              IF (TRIM(V%DT1%GetChar()) .NE. "-1") STOP 41
              IF (TRIM(V%GetChar())     .NE. "-2") STOP 42

            CLASS DEFAULT
              STOP 50
          END SELECT

        !NO finalization
        IF (ANY(Final)) STOP 60
      END SELECT

      !NO finalization
      IF (ANY(Final)) STOP 61
    CLASS DEFAULT
      STOP 62
  END SELECT
  
  ! Finalization on DT structure constructor
  IF (ANY(Final) .NEQV. .TRUE.) STOP 63

  CONTAINS

  FUNCTION UFun(UArg)
  USE M
  POINTER :: UFun
    ALLOCATE(UFun, SOURCE=UArg)
  END FUNCTION


  END

