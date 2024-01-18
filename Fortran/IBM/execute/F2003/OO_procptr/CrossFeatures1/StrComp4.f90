! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qfree=f90
! %GROUP: StrComp4.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : StrComp4.f
!*
!*  DATE                       : May. 18, 2005
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
!*  Structure component - Paramter
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  MODULE M

    TYPE :: Base
      INTEGER :: BaseID=1
      PROCEDURE(LOGICAL(2)), NOPASS, POINTER :: ProcPtr=>NULL()
    END TYPE

    TYPE  :: DT
      INTEGER :: ChildID=2
      TYPE(Base) :: BComp=Base(-1, NULL())
    END TYPE

  END MODULE

  PROGRAM StrComp4
  USE M
  IMPLICIT NONE

  TYPE(DT), PARAMETER :: Para=DT(-2,              &
                           &     Base(-3, NULL()))

  TYPE(DT), PARAMETER :: U=Para
  TYPE(DT)            :: V

  IF ( Para%ChildId .NE. -2 ) STOP 12
  IF ( Para%BComp%BaseId .NE. -3 ) STOP 11
  IF ( ASSOCIATED(Para%BComp%ProcPtr) ) STOP 13

  IF ( U%ChildId .NE. -2 ) STOP 22
  IF ( U%BComp%BaseId .NE. -3 ) STOP 21
  IF ( ASSOCIATED(U%BComp%ProcPtr) ) STOP 23

  V = Para
  IF ( V%ChildId .NE. -2 ) STOP 22
  IF ( V%BComp%BaseId .NE. -3 ) STOP 21
  IF ( ASSOCIATED(V%BComp%ProcPtr) ) STOP 23


  END

