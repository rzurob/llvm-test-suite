! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qfree=f90
! %GROUP:  Assign1.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : Assign1.f
!*
!*  DATE                       : May. 16, 2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Procedure pointer
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  : Feature 289058
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*  A derived-type intrinsic assignment
!*  (Mem fault- 305097)
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M

    INTERFACE
      FUNCTION IToI(Arg)
       INTEGER :: Arg
       INTEGER :: Fun
      END FUNCTION
    END INTERFACE

    TYPE :: Base
      PROCEDURE(IToI), NOPASS, POINTER :: ProcPtr=>NULL()
    END TYPE

    TYPE :: DT
      INTEGER :: Id
      TYPE(Base), ALLOCATABLE :: BComp
    END TYPE

    INTERFACE ASSIGNMENT ( = )
      MODULE PROCEDURE PToP
    END INTERFACE ASSIGNMENT ( = )

    CONTAINS

    FUNCTION Fun(Arg)
    INTEGER :: Arg
    INTEGER :: Fun
      Fun = Arg
    END FUNCTION

    SUBROUTINE PToP (Arg1, Arg2)
    TYPE(DT), INTENT (OUT) :: Arg1
    TYPE(Dt), INTENT (IN)  :: Arg2
      Arg1%Id  = Arg2%Id
      ALLOCATE(Arg1%BComp)
      Arg1%BComp%ProcPtr => NULL()
    END SUBROUTINE

  END MODULE


  PROGRAM Assign1
  USE M
  IMPLICIT NONE

  TYPE (DT) :: V
  PROCEDURE(IToI), POINTER :: ProcPtr

  ProcPtr => Fun
  V = DT(-1, Base(ProcPtr))

  IF (V%Id .NE. -1 )                 STOP 11
  IF ( .NOT. ALLOCATED(V%BComp) )    STOP 12
  IF ( ASSOCIATED(V%BComp%ProcPtr) ) STOP 13


  END

