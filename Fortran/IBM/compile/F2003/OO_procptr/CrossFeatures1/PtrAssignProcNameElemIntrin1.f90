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
! %POSTCMD: tcomp PtrAssignProcNameElemIntrin1.f
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : PtrAssignProcNameElemIntrin1.f
!*
!*  DATE                       : Mar. 18, 2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Procedure pointer
!*
!*  SECONDARY FUNCTIONS TESTED : Pointer assignment
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
!*  test if a procedure pointer can have the elemental attribute
!*  (328094.test)
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M

    CONTAINS

    ELEMENTAL FUNCTION GetChildId(Arg)
    INTEGER             :: GetChildId
    INTEGER, INTENT(IN) :: Arg
      GetChildId = 1
    END FUNCTION

  END MODULE

  MODULE M1
  USE M

    CONTAINS

    ELEMENTAL SUBROUTINE ModSub(Arg)
      PROCEDURE(IntSub),     POINTER :: Ptr1
      PROCEDURE(GetChildId), POINTER :: Ptr2
      INTEGER,            INTENT(IN) :: Arg

!     Ptr1 => IntSub
!     Ptr2 => GetChildId

      CONTAINS
      ELEMENTAL SUBROUTINE IntSub(Arg)
      INTEGER,            INTENT(IN) :: Arg
      END SUBROUTINE

    END SUBROUTINE

  END MODULE

  PROGRAM PtrAssignProcNameElemIntrin1
  USE M
  IMPLICIT NONE

  INTERFACE
    ELEMENTAL SUBROUTINE ExtSub(Arg)
      INTEGER,            INTENT(IN) :: Arg
    END SUBROUTINE
  END INTERFACE

  PROCEDURE(IntSub),     POINTER :: Ptr1
  PROCEDURE(GetChildId), POINTER :: Ptr2

! Ptr1 => IntSub
! Ptr2 => GetChildId

  CONTAINS

  ELEMENTAL SUBROUTINE IntSub(Arg)
    PROCEDURE(IntSub),     POINTER :: Ptr1
    PROCEDURE(GetChildId), POINTER :: Ptr2
    INTEGER,            INTENT(IN) :: Arg

!   Ptr1 => IntSub
!   Ptr2 => GetChildId

  END SUBROUTINE

  END

  ELEMENTAL SUBROUTINE ExtSub(Arg)
  USE M
  PROCEDURE(GetChildId), POINTER :: Ptr2
  INTEGER,            INTENT(IN) :: Arg

! Ptr2 => GetChildId

  END SUBROUTINE

